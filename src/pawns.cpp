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
  Score evaluate(const Position& pos, Pawns::Entry* e) {

    constexpr Color     Them = (Us == WHITE ? BLACK : WHITE);
    constexpr Direction Up   = (Us == WHITE ? NORTH : SOUTH);
    constexpr Direction Down = (Us == WHITE ? SOUTH : NORTH);
    constexpr Bitboard  FarSideBB = (Us == WHITE) ? (Rank5BB | Rank6BB | Rank7BB | Rank8BB)
                                                  : (Rank1BB | Rank2BB | Rank3BB | Rank4BB);

    Bitboard sq_file_bb, sq_rank_bb, sq_bb;
    Bitboard pawnAttacksBB, eastBB, westBB, adjacentFilesBB, passedPawnSpanBB;
    Bitboard neighbours, stoppers, support, phalanx, opposed;
    Bitboard lever, leverPush, blocked;
    int numPhalanx, numLeverPush, numSupport;
    bool backward, passed, doubled;
    Square s;
    Score score, finalScore;
    Bitboard finalPassed;
    const Square* pl = pos.squares<PAWN>(Us);

    Bitboard ourPawns   = pos.pieces(  Us, PAWN);
    Bitboard theirPawns = pos.pieces(Them, PAWN);
    int numPawns = popcount(ourPawns);

    Bitboard doubleAttackThem = pawn_double_attacks_bb<Them>(theirPawns);

    // vector input arrays
    Bitboard sq_file_bb_array[SQUARE_NB];
    Bitboard sq_rank_bb_array[SQUARE_NB];
    
    // vector output arrays
    Score score_output_array[SQUARE_NB];
    Bitboard passed_output_array[SQUARE_NB];
    int v1_output_array[SQUARE_NB];
    int v2_output_array[SQUARE_NB];
//    Bitboard support_result_array[SQUARE_NB];
//    Bitboard attacksSpan_output_array[SQUARE_NB];

    e->passedPawns[Us] = 0;
    e->kingSquares[Us] = SQ_NONE;
    e->pawnAttacks[Us] = e->pawnAttacksSpan[Us] = pawn_attacks_bb<Us>(ourPawns);

    // Loop through all pawns of the current color, filling up the input vectors
    for (int i=0; i<numPawns; i++)
    {
      s = pl[i];
      
      assert(pos.piece_on(s) == make_piece(Us, PAWN));
      
      sq_file_bb_array[i] = file_bb(s);
      sq_rank_bb_array[i] = rank_bb(s);
    }

    // Loop through all pawns of the current color and score each pawn
    for (int i=0; i<numPawns; i++)
    {
        sq_file_bb = sq_file_bb_array[i];
        sq_rank_bb = sq_rank_bb_array[i];
        sq_bb = sq_file_bb & sq_rank_bb;
        
        pawnAttacksBB = pawn_attacks_bb<Us>(sq_bb);
        eastBB = shift<EAST>(sq_file_bb);
        westBB = shift<WEST>(sq_file_bb);
        adjacentFilesBB = eastBB | westBB;
        passedPawnSpanBB = forward_ranks_bb(Us, sq_rank_bb) & (adjacentFilesBB | sq_file_bb);

        // Flag the pawn
        opposed    = theirPawns & forward_file_bb(Us, sq_file_bb, sq_rank_bb);
        blocked    = theirPawns & shift<Up>(sq_bb);
        stoppers   = theirPawns & passedPawnSpanBB;
        lever      = theirPawns & pawnAttacksBB;
        leverPush  = theirPawns & shift<Up>(pawnAttacksBB);
        doubled    = bool(ourPawns & shift<Down>(sq_bb));
        neighbours = ourPawns   & adjacentFilesBB;
        phalanx    = neighbours & sq_rank_bb;
        support    = neighbours & shift<Down>(sq_rank_bb);

        // A pawn is backward when it is behind all pawns of the same color on
        // the adjacent files and cannot safely advance.
        backward =  !(neighbours & forward_ranks_bb(Them, shift<Up>(sq_rank_bb)))
                  && (stoppers & (leverPush | blocked));

        // Compute additional span if pawn is not backward nor blocked
        if (!backward && !blocked)
            e->pawnAttacksSpan[Us] |= pawn_attack_span(Us, sq_file_bb, sq_rank_bb);

        numPhalanx   = bool(phalanx & westBB)   + bool(phalanx & eastBB);
        numLeverPush = bool(leverPush & westBB) + bool(leverPush & eastBB);

        // A pawn is passed if one of the three following conditions is true:
        // (a) there is no stoppers except some levers
        // (b) the only stoppers are the leverPush, but we outnumber them
        // (c) there is only one front stopper which can be levered.
        passed =   !(stoppers ^ lever)
                || (   !(stoppers ^ leverPush)
                    && numPhalanx >= numLeverPush)
                || (   stoppers == blocked
                    && (sq_bb & FarSideBB)
                    && (shift<Up>(support) & ~(theirPawns | doubleAttackThem)));

        // Passed pawns will be properly scored later in evaluation when we have
        // full attack info.
        passed_output_array[i] = (passed) ? sq_bb : Bitboard(0);

        v1_output_array[i] = (support | phalanx) ? (2 + bool(phalanx) - bool(opposed)) : 0;
        
        numSupport = bool(support & eastBB) + bool(support & westBB);
        v2_output_array[i] = 21 * numSupport;

        // Score this pawn
        score = SCORE_ZERO;
        if (support | phalanx)
          ;
        else if (!neighbours)
            score -=   Isolated
                     + WeakUnopposed * !opposed;

        else if (backward)
            score -=   Backward
                     + WeakUnopposed * !opposed;

        if (!support)
            score -=   Doubled * doubled
                     + WeakLever * more_than_one(lever);
                     
        score_output_array[i] = score;
    }


    // Loop through all pawns of the current color, processing all the output vectors
    finalScore = SCORE_ZERO;
    finalPassed = Bitboard(0);
    for (int i=0; i<numPawns; i++)
    {
      finalScore  += score_output_array[i];
      finalPassed |= passed_output_array[i];
      
      if (v1_output_array[i]) 
      {
        s = pl[i];
        Rank r = relative_rank(Us, s);
        int v = Connected[ relative_rank(Us, s) ] * v1_output_array[i] + v2_output_array[i];
        finalScore += make_score(v, v * (r - 2) / 4);
      }
    }

    e->passedPawns[Us] = finalPassed;
    return finalScore;
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

      File d = map_to_queenside(f);
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
  auto compare = [](Score a, Score b) { return mg_value(a) < mg_value(b); };

  Score shelter = evaluate_shelter<Us>(pos, ksq);

  // If we can castle use the bonus after castling if it is bigger

  if (pos.can_castle(Us & KING_SIDE))
      shelter = std::max(shelter, evaluate_shelter<Us>(pos, relative_square(Us, SQ_G1)), compare);

  if (pos.can_castle(Us & QUEEN_SIDE))
      shelter = std::max(shelter, evaluate_shelter<Us>(pos, relative_square(Us, SQ_C1)), compare);

  // In endgame we like to bring our king near our closest pawn
  Bitboard pawns = pos.pieces(Us, PAWN);
  int minPawnDist = pawns ? 8 : 0;

  if (pawns & PseudoAttacks[KING][ksq])
      minPawnDist = 1;
  else while (pawns)
      minPawnDist = std::min(minPawnDist, distance(ksq, pop_lsb(&pawns)));

  return shelter - make_score(0, 16 * minPawnDist);
}

// Explicit template instantiation
template Score Entry::do_king_safety<WHITE>(const Position& pos);
template Score Entry::do_king_safety<BLACK>(const Position& pos);

} // namespace Pawns
