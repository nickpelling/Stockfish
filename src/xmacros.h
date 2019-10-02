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

#ifndef XMACROS_H_INCLUDED
#define XMACROS_H_INCLUDED

/* Define an XMacro for file-related functions */
#define XMACRO_FILE(F) \
	F(FILE_A)	F(FILE_B)	F(FILE_C)	F(FILE_D)	F(FILE_E)	F(FILE_F)	F(FILE_G)	F(FILE_H)

/* Define an XMacro for rank-related functions */
#define XMACRO_RANK(F) \
	F(RANK_1)	F(RANK_2)	F(RANK_3)	F(RANK_4)	F(RANK_5)	F(RANK_6)	F(RANK_7)	F(RANK_8)

/* Define an XMacro for square-related functions */
#define XMACRO_SQUARE(F) \
	F(SQ_A1)	F(SQ_B1)	F(SQ_C1)	F(SQ_D1)	F(SQ_E1)	F(SQ_F1)	F(SQ_G1)	F(SQ_H1)	\
	F(SQ_A2)	F(SQ_B2)	F(SQ_C2)	F(SQ_D2)	F(SQ_E2)	F(SQ_F2)	F(SQ_G2)	F(SQ_H2)	\
	F(SQ_A3)	F(SQ_B3)	F(SQ_C3)	F(SQ_D3)	F(SQ_E3)	F(SQ_F3)	F(SQ_G3)	F(SQ_H3)	\
	F(SQ_A4)	F(SQ_B4)	F(SQ_C4)	F(SQ_D4)	F(SQ_E4)	F(SQ_F4)	F(SQ_G4)	F(SQ_H4)	\
	F(SQ_A5)	F(SQ_B5)	F(SQ_C5)	F(SQ_D5)	F(SQ_E5)	F(SQ_F5)	F(SQ_G5)	F(SQ_H5)	\
	F(SQ_A6)	F(SQ_B6)	F(SQ_C6)	F(SQ_D6)	F(SQ_E6)	F(SQ_F6)	F(SQ_G6)	F(SQ_H6)	\
	F(SQ_A7)	F(SQ_B7)	F(SQ_C7)	F(SQ_D7)	F(SQ_E7)	F(SQ_F7)	F(SQ_G7)	F(SQ_H7)	\
	F(SQ_A8)	F(SQ_B8)	F(SQ_C8)	F(SQ_D8)	F(SQ_E8)	F(SQ_F8)	F(SQ_G8)	F(SQ_H8)

#endif
