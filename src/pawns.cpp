/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad
  Copyright (C) 2015-2019 Marco Costalba, Joona Kiiski, Gary Linscott, Tord Romstad

  Stockfish is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Stockfish is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <algorithm>
#include <cassert>

#include "bitboard.h"
#include "pawns.h"
#include "position.h"
#include "thread.h"

namespace {

  #define V Value
  #define S(mg, eg) make_score(mg, eg)

  // Pawn penalties
  constexpr Score Backward      = S( 9, 24);
  constexpr Score BlockedStorm  = S(82, 82);
  constexpr Score Doubled       = S(11, 56);
  constexpr Score Isolated      = S( 5, 15);
  constexpr Score WeakLever     = S( 0, 56);
  constexpr Score WeakUnopposed = S(13, 27);

  // Connected pawn bonus
  constexpr int Connected[RANK_NB] = { 0, 7, 8, 12, 29, 48, 86 };

  // Strength of pawn shelter for our king by [distance from edge][rank].
  // RANK_1 = 0 is used for files where we have no pawn, or pawn is behind our king.
  constexpr Value ShelterStrength[int(FILE_NB) / 2][RANK_NB] = {
    { V( -6), V( 81), V( 93), V( 58), V( 39), V( 18), V(  25) },
    { V(-43), V( 61), V( 35), V(-49), V(-29), V(-11), V( -63) },
    { V(-10), V( 75), V( 23), V( -2), V( 32), V(  3), V( -45) },
    { V(-39), V(-13), V(-29), V(-52), V(-48), V(-67), V(-166) }
  };

  // Danger of enemy pawns moving toward our king by [distance from edge][rank].
  // RANK_1 = 0 is used for files where the enemy has no pawn, or their pawn
  // is behind our king. Note that UnblockedStorm[0][1-2] accommodate opponent pawn
  // on edge, likely blocked by our king.
  constexpr Value UnblockedStorm[int(FILE_NB) / 2][RANK_NB] = {
    { V( 89), V(-285), V(-185), V(93), V(57), V( 45), V( 51) },
    { V( 44), V( -18), V( 123), V(46), V(39), V( -7), V( 23) },
    { V(  4), V(  52), V( 162), V(37), V( 7), V(-14), V( -2) },
    { V(-10), V( -14), V(  90), V(15), V( 2), V( -7), V(-16) }
  };

  #undef S
  #undef V

template<Color C>
constexpr Bitboard pawn_double_attacks_bb(Bitboard b) {
  return C == WHITE ? shift<NORTH_WEST>(b) & shift<NORTH_EAST>(b)
                    : shift<SOUTH_WEST>(b) & shift<SOUTH_EAST>(b);
}

    template<Color Us>
  inline Score evaluate_pawn( Bitboard square_file_bb,
                              Bitboard square_rank_bb,
                              int connected_scale_factor,
                              int rank_scale_factor,
                              Bitboard ourPawns,
                              Bitboard theirPawns,
                              Bitboard doubleAttackThem,
                              Bitboard &phalanxResult,
                              Bitboard &leverPushResult,
                              Bitboard &passedResult ) {
  
    constexpr Color     Them = (Us == WHITE ? BLACK : WHITE);
    constexpr Direction Up   = (Us == WHITE ? NORTH : SOUTH);
    constexpr Direction Down = (Us == WHITE ? SOUTH : NORTH);
    constexpr Bitboard  FarSideBB = (Us == WHITE) ? (RANK_5 | RANK_6 | RANK_7 | RANK_8)
                                                  : (RANK_1 | RANK_2 | RANK_3 | RANK_4);
    
    Bitboard square_bb          = square_file_bb & square_rank_bb;
    Bitboard above2_bb          = shift<Direction(NORTH+NORTH)>(square_bb);
    Bitboard neighbour_mask_bb  = adjacent_files_bb(square_file_bb);
    Bitboard above1_mask_bb     = shift<Up>(neighbour_mask_bb & square_rank_bb);
    Bitboard above2_mask_bb     = shift<Up>(above1_mask_bb);

    Bitboard opposed    = theirPawns & forward_file_bb( Us, square_file_bb, square_rank_bb);
    Bitboard stoppers   = theirPawns & passed_pawn_span(Us, square_file_bb, square_rank_bb);
    Bitboard lever      = theirPawns & above1_mask_bb;
    Bitboard leverPush  = theirPawns & above2_mask_bb;
    Bitboard doubled    = ourPawns   & shift<Down>(square_bb); // should this be anywhere behind, not just immediately behind?
    Bitboard neighbours = ourPawns   & neighbour_mask_bb;
    Bitboard phalanx    = neighbours & square_rank_bb;
    Bitboard support    = neighbours & shift<Down>(square_rank_bb);
    Bitboard west_bb    = support    & shift<WEST>(square_file_bb);
    Bitboard east_bb    = support    & shift<EAST>(square_file_bb);

    // A pawn is backward when it is behind all pawns of the same color on
    // the adjacent files and cannot safely advance.
    // Phalanx and isolated pawns will be excluded when the pawn is scored.
    Bitboard forward_neighbours       = neighbours & forward_ranks_bb(Them, square_rank_bb);
    Bitboard stoppers_leverPush_above = stoppers & (leverPush | above2_bb); // FIXME?!?!? is above2_bb correct here?
    int backward_is_true = (forward_neighbours == 0) & (stoppers_leverPush_above != 0);

    // A pawn is passed if one of the three following conditions is true:
    // (a) there is no stoppers except some levers
    int passed_is_true = ((stoppers ^ lever) == 0);

    // (b) the only stoppers are the leverPush, but we outnumber them
    // However, because we can't vectorize popcount(), we have to defer this
    // calculation to the caller's exit phase
    int stoppers_xor_leverpush_iszero   = ((stoppers ^ leverPush) == 0);
    
    // (c) there is only one front stopper which can be levered.
    Bitboard theirPawns_or_doubleAttackThem = (theirPawns | doubleAttackThem);
    passed_is_true |= (   (stoppers == above2_bb)
                       &  ((square_rank_bb & FarSideBB) != 0)
                       &  (shift<Up>(support) & ~theirPawns_or_doubleAttackThem)   );
    
    int support_or_phalanx_iszero           = ((support | phalanx) == 0);
    int support_or_phalanx_isnonzero        = 1 - support_or_phalanx_iszero;
    int phalanx_iszero                      = (phalanx == 0);
    int opposed_iszero                      = (opposed == 0);

    int phalanx_scale = 3 - phalanx_iszero;
    int opposed_scale = 2 - opposed_iszero;   // reversed sense, to avoid having to divide
    int v = connected_scale_factor * phalanx_scale * opposed_scale;
    v += 17 * (west_bb != 0);
    v += 17 * (east_bb != 0);
      
    int isolated_score = Isolated + WeakUnopposed * opposed_iszero;

    int backward_score = Backward + WeakUnopposed * opposed_iszero;

    int lever_more_than_one = lever & (lever - 1);
    int doubled_score = int(Doubled) * (doubled != 0) + int(WeakLever) * (lever_more_than_one != 0);

    // Score this pawn
    int score = v * rank_scale_factor * support_or_phalanx_isnonzero;
      
    score -= isolated_score * support_or_phalanx_iszero * (neighbours == 0);

    score += backward_score * support_or_phalanx_iszero * (neighbours == 0) * backward_is_true;

    score -= doubled_score * (support == 0);

    passedResult    = square_bb * passed_is_true;
    phalanxResult   = phalanx   * stoppers_xor_leverpush_iszero;
    leverPushResult = leverPush * stoppers_xor_leverpush_iszero;
    return Score(score);
  }

  template<Color Us, int NUM_PAWNS>
  Score evaluate_pawn_set(const Square* SquareList, Bitboard ourPawns, Bitboard theirPawns, Bitboard &passedResult)
  {
    Bitboard Square_file_bb [NUM_PAWNS];
    Bitboard Square_rank_bb [NUM_PAWNS];
    Score    score_array    [NUM_PAWNS];
    Bitboard passed_array   [NUM_PAWNS];
    Bitboard phalanx_array  [NUM_PAWNS];
    Bitboard leverPush_array[NUM_PAWNS];
    
    int connected_scale_array[NUM_PAWNS];
    int rank_scale_array     [NUM_PAWNS];
    Score    final_score;
    Bitboard final_passed;
    int i;
    Square s;
    
    constexpr Color Them = (Us == WHITE) ? BLACK : WHITE;

    Bitboard doubleAttackThem = pawn_double_attacks_bb<Them>(theirPawns);

    // Non-vectorizable entry code
    for (i=0; i<NUM_PAWNS; i++)
    {
        s = SquareList[i];
        Square_file_bb[i] = file_bb(s);
        Square_rank_bb[i] = rank_bb(s);
      
        Rank r = relative_rank(Us, s);
        connected_scale_array[i] = Connected[r] >> 1;
        rank_scale_array[i] = 65536 + (r - 2) / 4;    // scales v to yield a Score
    }

    // Vectorizable main code
    for (i=0; i<NUM_PAWNS; i++)
    {
        score_array[i] = evaluate_pawn<Us>( Square_file_bb[i],
                                            Square_rank_bb[i],
                                            connected_scale_array[i],
                                            rank_scale_array[i],
                                            ourPawns,
                                            theirPawns,
                                            doubleAttackThem,
                                            phalanx_array[i],
                                            leverPush_array[i],
                                            passed_array[i] );
    }
    
    // Non-vectorizable exit code
    final_passed = Bitboard(0);
    final_score = SCORE_ZERO;
    for (i=0; i<NUM_PAWNS; i++)
    {
      final_passed |= passed_array[i];
      if (    phalanx_array[i]
          && (popcount(phalanx_array[i]) >= popcount(leverPush_array[i]))  )
      {
        final_passed |= Square_file_bb[i] & Square_rank_bb[i];
      }
      final_score += score_array[i];
    }
    
    passedResult = final_passed;
    return final_score;
  }
  
  // To evaluate all the pawns, select the appropriate unrolled template
  // based on both the current side's colour and on the number of pawns.
  // This allows the compiler to implement this differently as needed.
  template<Color Us>
  Score evaluate(const Position& pos, Pawns::Entry* e) {
    const Square* pl = pos.squares<PAWN>(Us);
    Score     final_score;
    Bitboard  final_passed;
    
    constexpr Color Them = (Us == WHITE ? BLACK : WHITE);
    
    Bitboard ourPawns   = pos.pieces(Us,   PAWN);
    Bitboard theirPawns = pos.pieces(Them, PAWN);

    e->kingSquares[Us] = SQ_NONE;
    e->pawnAttacks[Us] = pawn_attacks_bb<Us>(ourPawns);

    switch (popcount(ourPawns))
    {
      case 0:
        final_score = SCORE_ZERO;
        final_passed = NoSquares;
        break;
      case 1: final_score = evaluate_pawn_set<Us,1>(pl, ourPawns, theirPawns, final_passed);  break;
      case 2: final_score = evaluate_pawn_set<Us,2>(pl, ourPawns, theirPawns, final_passed);  break;
      case 3: final_score = evaluate_pawn_set<Us,3>(pl, ourPawns, theirPawns, final_passed);  break;
      case 4: final_score = evaluate_pawn_set<Us,4>(pl, ourPawns, theirPawns, final_passed);  break;
      case 5: final_score = evaluate_pawn_set<Us,5>(pl, ourPawns, theirPawns, final_passed);  break;
      case 6: final_score = evaluate_pawn_set<Us,6>(pl, ourPawns, theirPawns, final_passed);  break;
      case 7: final_score = evaluate_pawn_set<Us,7>(pl, ourPawns, theirPawns, final_passed);  break;
      case 8: final_score = evaluate_pawn_set<Us,8>(pl, ourPawns, theirPawns, final_passed);  break;
      default:
        final_score = SCORE_ZERO;
        final_passed = NoSquares;
        break;
    }

    e->passedPawns[Us] = final_passed;

    return final_score;
  }

} // namespace

namespace Pawns {

/// Pawns::probe() looks up the current position's pawns configuration in
/// the pawns hash table. It returns a pointer to the Entry if the position
/// is found. Otherwise a new Entry is computed and stored there, so we don't
/// have to recompute all when the same pawns configuration occurs again.

Entry* probe(const Position& pos) {

  Key key = pos.pawn_key();
  Entry* e = pos.this_thread()->pawnsTable[key];

  if (e->key == key)
      return e;

  e->key = key;
  e->scores[WHITE] = evaluate<WHITE>(pos, e);
  e->scores[BLACK] = evaluate<BLACK>(pos, e);

  return e;
}


/// Entry::evaluate_shelter() calculates the shelter bonus and the storm
/// penalty for a king, looking at the king file and the two closest files.

template<Color Us>
Score Entry::evaluate_shelter(const Position& pos, Square ksq) {

  constexpr Color Them = (Us == WHITE ? BLACK : WHITE);

  Bitboard b = pos.pieces(PAWN) & ~forward_ranks_bb(Them, ksq);
  Bitboard ourPawns = b & pos.pieces(Us);
  Bitboard theirPawns = b & pos.pieces(Them);

  Score bonus = make_score(5, 5);

  File center = clamp(file_of(ksq), FILE_B, FILE_G);
  for (File f = File(center - 1); f <= File(center + 1); ++f)
  {
      b = ourPawns & file_bb(f);
      int ourRank = b ? relative_rank(Us, frontmost_sq(Them, b)) : 0;

      b = theirPawns & file_bb(f);
      int theirRank = b ? relative_rank(Us, frontmost_sq(Them, b)) : 0;

      int d = std::min(f, ~f);
      bonus += make_score(ShelterStrength[d][ourRank], 0);

      if (ourRank && (ourRank == theirRank - 1))
          bonus -= BlockedStorm * int(theirRank == RANK_3);
      else
          bonus -= make_score(UnblockedStorm[d][theirRank], 0);
  }

  return bonus;
}


/// Entry::do_king_safety() calculates a bonus for king safety. It is called only
/// when king square changes, which is about 20% of total king_safety() calls.

template<Color Us>
Score Entry::do_king_safety(const Position& pos) {

  Square ksq = pos.square<KING>(Us);
  kingSquares[Us] = ksq;
  castlingRights[Us] = pos.castling_rights(Us);

  Score shelters[3] = { evaluate_shelter<Us>(pos, ksq),
                        make_score(-VALUE_INFINITE, 0),
                        make_score(-VALUE_INFINITE, 0) };

  // If we can castle use the bonus after castling if it is bigger
  if (pos.can_castle(Us & KING_SIDE))
      shelters[1] = evaluate_shelter<Us>(pos, relative_square(Us, SQ_G1));

  if (pos.can_castle(Us & QUEEN_SIDE))
      shelters[2] = evaluate_shelter<Us>(pos, relative_square(Us, SQ_C1));

  for (int i : {1, 2})
     if (mg_value(shelters[i]) > mg_value(shelters[0]))
         shelters[0] = shelters[i];

  // In endgame we like to bring our king near our closest pawn
  Bitboard pawns = pos.pieces(Us, PAWN);
  int minPawnDist = pawns ? 8 : 0;

  if (pawns & PseudoAttacks[KING][ksq])
      minPawnDist = 1;
  else while (pawns)
      minPawnDist = std::min(minPawnDist, distance(ksq, pop_lsb(&pawns)));

  return shelters[0] - make_score(0, 16 * minPawnDist);
}

// Explicit template instantiation
template Score Entry::do_king_safety<WHITE>(const Position& pos);
template Score Entry::do_king_safety<BLACK>(const Position& pos);

} // namespace Pawns
