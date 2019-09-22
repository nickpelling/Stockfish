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

  template<Color Us>
  inline Score evaluate_pawn( Bitboard Square_file_bb,
                              Bitboard Square_rank_bb,
                              Bitboard ourPawns,
                              Bitboard theirPawns
                              Bitboard &passedResult ) {
  
    constexpr Color     Them = (Us == WHITE ? BLACK : WHITE);
    constexpr Direction Up   = (Us == WHITE ? NORTH : SOUTH);
    constexpr Direction Down = (Us == WHITE ? SOUTH : NORTH);
    constexpr Bitboard  FarSideBB = (Us == WHITE) ? (RANK_5 | RANK_6 | RANK_7 | RANK_8)
                                                  : (RANK_1 | RANK_2 | RANK_3 | RANK_4);

    int passed_is_true;
    
    Bitboard Square_bb  = Square_file_bb & Square_rank_bb;
    Bitboard above_bb   = shift<Up>(Square_bb);

    Bitboard opposed    = theirPawns & forward_file_bb(Us, Square_file_bb, Square_rank_bb);
    Bitboard stoppers   = theirPawns & passed_pawn_span(Us, Square_file_bb, Square_rank_bb);
    Bitboard lever      = theirPawns & adjacent_files_bb(Square_bb);
    Bitboard leverPush  = theirPawns & adjacent_files_bb(Above_bb);
    Bitboard doubled    = ourPawns   & shift<Down>(Square_bb);
    Bitboard neighbours = ourPawns   & adjacent_files_bb(Square_file_bb);
    Bitboard phalanx    = neighbours & Square_rank_bb;
    Bitboard support    = neighbours & shift<Down>(Square_rank_bb);

    // A pawn is backward when it is behind all pawns of the same color on
    // the adjacent files and cannot safely advance.
    // Phalanx and isolated pawns will be excluded when the pawn is scored.
    neighbours &= forward_ranks_bb(Them, Square_rank_bb);
    Bitboard stoppers_leverPush_above = stoppers & (leverPush | Above_bb);  
    int neighbours_iszero                   = (neighbours == 0);
    int stoppers_leverPush_above_isnonzero  = (stoppers_leverPush_above == 0);
    int backward_is_true = neighbours_iszero & stoppers_leverPush_above_iszero;
    int backward_score = Backward + WeakUnopposed * opposed_nonzero;

    // A pawn is passed if one of the three following conditions is true:
    // (a) there is no stoppers except some levers
    Bitboard stoppers_xor_lever = stoppers ^ lever;
    passed_is_true = (stoppers_xor_lever == 0);

    // (b) the only stoppers are the leverPush, but we outnumber them
    Bitboard stoppers_xor_leverPush     = stoppers ^ leverPush;    
    int stoppers_xor_leverpush_iszero   = (stoppers_xor_leverPush == 0);
    passed_is_true |= (   stoppers_xor_leverpush_iszero
                      &  (popcount(phalanx) >= popcount(leverPush))      );
    
    // (c) there is only one front stopper which can be levered.
    Bitboard theirPawns_or_doubleAttackThem = (theirPawns | doubleAttackThem;
    passed_is_true |= (   (stoppers == above_bb)
                       &  ((Square_rank_bb & FarSideBB) != 0)
                       &  (shift<Up>(support) & ~theirPawns_or_doubleAttackThem)   );
    
    int support_or_phalanx_iszero           = ((support | phalanx) == 0);
    int opposed_iszero                      = (opposed == 0);
    
    // Score this pawn
    int score = backward_score * backward_is_true * support_or_phalanx_iszero;


    passedResult = Square_bb * passed_is_true;
    return Score(score);
  }

  template<Color Us, int NUM_PAWNS>
  Score evaluate_pawn_set(const Square* pl, Bitboard &passedResult)
  {
    int i;
    Square s;
    Bitboard Square_file_bb[NUM_PAWNS];
    Bitboard Square_rank_bb[NUM_PAWNS;
    Score    score_array [NUM_PAWNS];
    Bitboard passed_array[NUM_PAWNS];
    Score     final_score;
    Bitboard  final_passed;
    
    constexpr Color Them = (Us == WHITE ? BLACK : WHITE);

    Bitboard ourPawns   = pos.pieces(Us,   PAWN);
    Bitboard theirPawns = pos.pieces(Them, PAWN);

    // Non-vectorizable stuff goes in here
    for (int i=0; i<NUM_PAWNS; i++)
    {
        s = mySquare[i];
        Square_file_bb[i] = file_bb(s);
        Square_rank_bb[i] = rank_bb(s);
    }

    // Vectorizable stuff goes in here
    for (int i=0; i<NUM_PAWNS; i++)
    {
        myScore[i] = evaluate_pawn<Us>( Square_file_bb[i],
                                            Square_rank_bb[i],
                                            ourPawns,
                                            theirPawns );
    }
    
    // Non-vectorizable stuff goes in here
    final_passed = Bitboard(0);
    final_score = SCORE_ZERO;
    for (int i=0; i<NUM_PAWNS; i++)
    {
      final_passed |= passed_array[i];
      final_score += score_array[i];
    }
    
    passedResult = final_passed;
    return final_score;
  }

  // To evaluate all the pawns, select the appropriate unrolled template
  // based on both the current side's colour and on the number of pawns.
  // This lets the compiler implement this in different ways
  template<Color Us>
  Score evaluate(const Position& pos, Pawns::Entry* e) {
    const Square* pl = pos.squares<PAWN>(Us);
    Score     final_score;
    Bitboard  final_passed;
    
    Bitboard ourPawns = pos.pieces(Us, PAWN);

    e->kingSquares[Us] = SQ_NONE;
    e->pawnAttacks[Us] = pawn_attacks_bb<Us>(ourPawns);

    switch (popcount(ourPawns))
    {
      case 0:
        final_score = 0;
        final_passed = Bitboard(0);
        break;
      case 1: final_score = evaluate_pawn_set<Us,1>(pl, final_passed);  break;
      case 2: final_score = evaluate_pawn_set<Us,2>(pl, final_passed);  break;
      case 3: final_score = evaluate_pawn_set<Us,3>(pl, final_passed);  break;
      case 4: final_score = evaluate_pawn_set<Us,4>(pl, final_passed);  break;
      case 5: final_score = evaluate_pawn_set<Us,5>(pl, final_passed);  break;
      case 6: final_score = evaluate_pawn_set<Us,6>(pl, final_passed);  break;
      case 7: final_score = evaluate_pawn_set<Us,7>(pl, final_passed);  break;
      case 8: final_score = evaluate_pawn_set<Us,8>(pl, final_passed);  break;
      default:
        final_score = 0;
        final_passed = Bitboard(0);
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
