/* This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>. 

Modifications Copyright 2025 Joseph Huang

Based on Belette Copyright 2024 Vincent Bab */

#include <iostream>

#include <map>
#include <string>
#include <sstream>
#include <istream>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <filesystem>

#include <set>
#include <cassert>
#include <functional>

#include <chrono>

namespace bchess {

using TimeMs = std::chrono::milliseconds::rep;

inline TimeMs now() {
    return std::chrono::duration_cast<std::chrono::milliseconds> (std::chrono::steady_clock::now().time_since_epoch()).count();
}

inline int parseInt(const std::string &str) {
    try {
        return std::stoi(str);
    } catch (const std::invalid_argument & e) {
        return 0;
    } catch (const std::out_of_range & e) {
        return 0;
    }

    return 0;
}

inline int64_t parseInt64(const std::string &str) {
    try {
        return std::stoll(str);
    } catch (const std::invalid_argument & e) {
        return 0;
    } catch (const std::out_of_range & e) {
        return 0;
    }

    return 0;
}

// https://stackoverflow.com/questions/9779105/generic-member-function-pointer-as-a-template-parameter
template <typename T, typename R, typename ...Args>
class MemberFunctionProxy {
public:
    typedef void (T::*Func)(Args...);
    
    inline MemberFunctionProxy() { };
    inline MemberFunctionProxy(Func func_): func(func_) { };
    inline R operator()(T &obj, Args ...args) { (obj.*func)(std::forward<Args>(args)...); }
private:
    Func func;
};

} /* namespace bchess */

namespace bchess {

class UciOption {
public:
    using OnUpdate = std::function<void(const UciOption&)>;
    using OptionValues = std::set<std::string>;

    UciOption(OnUpdate = nullptr); // button
    explicit UciOption(bool defaultValue_, OnUpdate = nullptr); // check
    explicit UciOption(const std::string &defaultValue_, OnUpdate = nullptr); // string
    explicit UciOption(const char *defaultValue_, OnUpdate = nullptr); // string
    explicit UciOption(int defaultValue_, int min_, int max_, OnUpdate = nullptr); // spin
    explicit UciOption(const std::string &defaultValue_, const OptionValues &allowedValues_, OnUpdate = nullptr); // combo

    inline bool isButton() const { return type == "button"; }
    inline bool isCheck() const { return type == "check"; }
    inline bool isString() const { return type == "string"; }
    inline bool isSpin() const { return type == "spin"; }
    inline bool isCombo() const { return type == "combo"; }

    inline operator int64_t() const {
        assert(type == "spin");
        return parseInt64(value);
    }
    inline operator std::string() const {
        assert(type != "button");
        return value;
    }
    inline operator bool() const {
        if (isCheck()) return value == "true";
        if (isSpin()) return parseInt(value) != 0;
        
        return !value.empty();
    }

    UciOption& operator=(const std::string& v);
    friend std::ostream &operator<<(std::ostream &, UciOption const &);

private:
    std::string type; // button, check, string, spin, combo
    std::string defaultValue;
    std::string value;
    int min;
    int max;
    std::set<std::string> allowedValues;
    OnUpdate onUpdate;
};

} /* namespace bchess */

#include <atomic>
#include <memory>

#include <cstdint>
#include <cassert>
#include <string>

namespace bchess {

constexpr int MAX_PLY = 128;
constexpr int MAX_HISTORY   = 2048;
constexpr int MAX_MOVE   = 220;

using Bitboard = uint64_t;
constexpr Bitboard EmptyBB = 0ULL;

using Score = int;
constexpr Score SCORE_NONE = 32600;
constexpr Score SCORE_INFINITE = 32500;
constexpr Score SCORE_MATE = 32000;
constexpr Score SCORE_MATE_MAX_PLY = 32000 - MAX_PLY;
constexpr Score SCORE_DRAW = 0;

// bit  0- 5: destination square (from 0 to 63)
// bit  6-11: origin square (from 0 to 63)
// 
// bit 14-15: promotion: pieceType-2 (from KNIGHT-2 to QUEEN-2)
// bit 12-13: special move flag: castling (1), en passant (2), promotion (3)
enum Move : uint16_t {
    MOVE_NONE,
    MOVE_NULL = 65
};

enum MoveType {
    NORMAL,
    PROMOTION = 1 << 14,
    EN_PASSANT = 2 << 14,
    CASTLING  = 3 << 14
};

enum Side {
    WHITE, BLACK, NB_SIDE = 2
};

enum PieceType {
    ALL_PIECES = 0,
    NO_PIECE_TYPE = 0, 
    PAWN = 1, KNIGHT, BISHOP, ROOK, QUEEN, KING,
    NB_PIECE_TYPE = 7
};

enum Piece {
    NO_PIECE,
    W_PAWN = PAWN,     W_KNIGHT, W_BISHOP, W_ROOK, W_QUEEN, W_KING,
    B_PAWN = PAWN + 8, B_KNIGHT, B_BISHOP, B_ROOK, B_QUEEN, B_KING,
    NB_PIECE = 16
};

enum Square : int {
    SQ_A1, SQ_B1, SQ_C1, SQ_D1, SQ_E1, SQ_F1, SQ_G1, SQ_H1,
    SQ_A2, SQ_B2, SQ_C2, SQ_D2, SQ_E2, SQ_F2, SQ_G2, SQ_H2,
    SQ_A3, SQ_B3, SQ_C3, SQ_D3, SQ_E3, SQ_F3, SQ_G3, SQ_H3,
    SQ_A4, SQ_B4, SQ_C4, SQ_D4, SQ_E4, SQ_F4, SQ_G4, SQ_H4,
    SQ_A5, SQ_B5, SQ_C5, SQ_D5, SQ_E5, SQ_F5, SQ_G5, SQ_H5,
    SQ_A6, SQ_B6, SQ_C6, SQ_D6, SQ_E6, SQ_F6, SQ_G6, SQ_H6,
    SQ_A7, SQ_B7, SQ_C7, SQ_D7, SQ_E7, SQ_F7, SQ_G7, SQ_H7,
    SQ_A8, SQ_B8, SQ_C8, SQ_D8, SQ_E8, SQ_F8, SQ_G8, SQ_H8,
    SQ_NONE,

    SQ_FIRST = SQ_A1,
    NB_SQUARE = 64
};

enum File : int {
    FILE_A, FILE_B, FILE_C, FILE_D, FILE_E, FILE_F, FILE_G, FILE_H, 
    NB_FILE
};

enum Rank : int {
    RANK_1, RANK_2, RANK_3, RANK_4, RANK_5, RANK_6, RANK_7, RANK_8, 
    NB_RANK
};

enum Direction : int {
    UP =  8,
    DOWN = -UP,
    RIGHT  =  1,
    LEFT  = -RIGHT,

    UP_RIGHT = UP + RIGHT,
    UP_LEFT = UP + LEFT,
    DOWN_RIGHT = DOWN + RIGHT,
    DOWN_LEFT = DOWN + LEFT,
};

constexpr int dir8(Direction d) {
    switch (d) {
        case UP: return 0;
        case DOWN: return 1;
        case RIGHT: return 2;
        case LEFT: return 3;

        case UP_RIGHT: return 4;
        case UP_LEFT: return 5;
        case DOWN_RIGHT: return 6;
        case DOWN_LEFT: return 7;
    }

    static_assert(true, "invalid dir");
    return -1;
}

enum CastlingRight {
    NO_CASTLING = 0,
    WHITE_KING_SIDE = 1,
    WHITE_QUEEN_SIDE = 2,
    BLACK_KING_SIDE  = 4,
    BLACK_QUEEN_SIDE = 8,

    KING_SIDE      = WHITE_KING_SIDE  | BLACK_KING_SIDE,
    QUEEN_SIDE     = WHITE_QUEEN_SIDE | BLACK_QUEEN_SIDE,
    WHITE_CASTLING = WHITE_KING_SIDE  | WHITE_QUEEN_SIDE,
    BLACK_CASTLING = BLACK_KING_SIDE  | BLACK_QUEEN_SIDE,
    ANY_CASTLING   = WHITE_CASTLING | BLACK_CASTLING,

    NB_CASTLING_RIGHT = 16
};

enum Phase {
    MG,
    EG,
    NB_PHASE = 2
};

constexpr Bitboard FileABB = 0x0101010101010101ULL;
constexpr Bitboard FileBBB = FileABB << 1;
constexpr Bitboard FileCBB = FileABB << 2;
constexpr Bitboard FileDBB = FileABB << 3;
constexpr Bitboard FileEBB = FileABB << 4;
constexpr Bitboard FileFBB = FileABB << 5;
constexpr Bitboard FileGBB = FileABB << 6;
constexpr Bitboard FileHBB = FileABB << 7;

constexpr Bitboard Rank1BB = 0xFF;
constexpr Bitboard Rank2BB = Rank1BB << (8 * 1);
constexpr Bitboard Rank3BB = Rank1BB << (8 * 2);
constexpr Bitboard Rank4BB = Rank1BB << (8 * 3);
constexpr Bitboard Rank5BB = Rank1BB << (8 * 4);
constexpr Bitboard Rank6BB = Rank1BB << (8 * 5);
constexpr Bitboard Rank7BB = Rank1BB << (8 * 6);
constexpr Bitboard Rank8BB = Rank1BB << (8 * 7);

/*constexpr Bitboard SQUARE_BB[65] = {
	0x1, 0x2, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80,
	0x100, 0x200, 0x400, 0x800, 0x1000, 0x2000, 0x4000, 0x8000,
	0x10000, 0x20000, 0x40000, 0x80000, 0x100000, 0x200000, 0x400000, 0x800000,
	0x1000000, 0x2000000, 0x4000000, 0x8000000, 0x10000000, 0x20000000, 0x40000000, 0x80000000,
	0x100000000, 0x200000000, 0x400000000, 0x800000000, 0x1000000000, 0x2000000000, 0x4000000000, 0x8000000000,
	0x10000000000, 0x20000000000, 0x40000000000, 0x80000000000, 0x100000000000, 0x200000000000, 0x400000000000, 0x800000000000,
	0x1000000000000, 0x2000000000000, 0x4000000000000, 0x8000000000000, 0x10000000000000, 0x20000000000000, 0x40000000000000, 0x80000000000000,
	0x100000000000000, 0x200000000000000, 0x400000000000000, 0x800000000000000, 0x1000000000000000, 0x2000000000000000, 0x4000000000000000, 0x8000000000000000,
	0
};*/
constexpr Bitboard bb(Square s) { return (1ULL << s); }
//constexpr Bitboard bb(Square s) { return SQUARE_BB[s]; }

inline Square& operator++(Square& sq) { return sq = Square(int(sq) + 1); }
inline Square& operator--(Square& sq) { return sq = Square(int(sq) - 1); }

constexpr Square operator+(Square sq, Direction dir) { return Square(int(sq) + int(dir)); }
constexpr Square operator-(Square sq, Direction dir) { return Square(int(sq) - int(dir)); }
inline Square& operator+=(Square& sq, Direction dir) { return sq = sq + dir; }
inline Square& operator-=(Square& sq, Direction dir) { return sq = sq - dir; }

constexpr CastlingRight operator|(CastlingRight cr1, CastlingRight cr2) { return CastlingRight(int(cr1) | int(cr2)); }
constexpr CastlingRight operator&(CastlingRight cr1, CastlingRight cr2) { return CastlingRight(int(cr1) & int(cr2)); }
constexpr CastlingRight operator~(CastlingRight cr1) { return CastlingRight(~int(cr1)); }
inline CastlingRight& operator|=(CastlingRight& cr, CastlingRight other) { return cr = cr | other; }
inline CastlingRight& operator&=(CastlingRight& cr, CastlingRight other) { return cr = cr & other; }
constexpr CastlingRight operator&(Side s, CastlingRight cr) { return CastlingRight((s == WHITE ? WHITE_CASTLING : BLACK_CASTLING) & cr); }

constexpr Side operator~(Side c) { return Side(c ^ BLACK); }

constexpr Square CastlingKingTo[NB_CASTLING_RIGHT] = {
    SQ_NONE, // NO_CASTLING
    SQ_G1,   // WHITE_KING_SIDE = 1,
    SQ_C1,   // WHITE_QUEEN_SIDE = 2,
    SQ_NONE,
    SQ_G8,   // BLACK_KING_SIDE  = 4,
    SQ_NONE,SQ_NONE,SQ_NONE,
    SQ_C8    // BLACK_QUEEN_SIDE = 8,
};
constexpr Square CastlingRookFrom[NB_CASTLING_RIGHT] = {
    SQ_NONE, // NO_CASTLING
    SQ_H1,   // WHITE_KING_SIDE = 1,
    SQ_A1,   // WHITE_QUEEN_SIDE = 2,
    SQ_NONE,
    SQ_H8,   // BLACK_KING_SIDE  = 4,
    SQ_NONE,SQ_NONE,SQ_NONE,
    SQ_A8    // BLACK_QUEEN_SIDE = 8,
};
constexpr Square CastlingRookTo[NB_CASTLING_RIGHT] = {
    SQ_NONE, // NO_CASTLING
    SQ_F1,   // WHITE_KING_SIDE = 1,
    SQ_D1,   // WHITE_QUEEN_SIDE = 2,
    SQ_NONE,
    SQ_F8,   // BLACK_KING_SIDE  = 4,
    SQ_NONE,SQ_NONE,SQ_NONE,
    SQ_D8    // BLACK_QUEEN_SIDE = 8,
};

constexpr CastlingRight CastlingRightsMask[NB_SQUARE] = {
    WHITE_QUEEN_SIDE, NO_CASTLING, NO_CASTLING, NO_CASTLING, WHITE_QUEEN_SIDE|WHITE_KING_SIDE, NO_CASTLING, NO_CASTLING, WHITE_KING_SIDE,
    NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING,
    NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING,
    NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING,
    NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING,
    NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING,
    NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING, NO_CASTLING,
    BLACK_QUEEN_SIDE, NO_CASTLING, NO_CASTLING, NO_CASTLING, BLACK_QUEEN_SIDE|BLACK_KING_SIDE, NO_CASTLING, NO_CASTLING, BLACK_KING_SIDE,
};

// Squares that need to be empty for castling
constexpr Bitboard CastlingPath[NB_CASTLING_RIGHT] = {
    EmptyBB,                            // NO_CASTLING
    bb(SQ_F1) | bb(SQ_G1),              // WHITE_KING_SIDE = 1,
    bb(SQ_D1) | bb(SQ_C1) | bb(SQ_B1),  // WHITE_QUEEN_SIDE = 2,
    EmptyBB,
    bb(SQ_F8) | bb(SQ_G8),              // BLACK_KING_SIDE  = 4,
    EmptyBB,EmptyBB,EmptyBB,
    bb(SQ_D8) | bb(SQ_C8) | bb(SQ_B8)   // BLACK_QUEEN_SIDE = 8,
};

// Squares that need not to be attacked by ennemy for castling
constexpr Bitboard CastlingKingPath[NB_CASTLING_RIGHT] = {
    EmptyBB,                            // NO_CASTLING
    bb(SQ_E1) | bb(SQ_F1) | bb(SQ_G1),  // WHITE_KING_SIDE = 1,
    bb(SQ_E1) | bb(SQ_D1) | bb(SQ_C1),  // WHITE_QUEEN_SIDE = 2,
    EmptyBB,
    bb(SQ_E8) | bb(SQ_F8) | bb(SQ_G8),  // BLACK_KING_SIDE  = 4,
    EmptyBB,EmptyBB,EmptyBB,
    bb(SQ_E8) | bb(SQ_D8) | bb(SQ_C8)   // BLACK_QUEEN_SIDE = 8,
};

constexpr bool isValidSq(Square s) {
    return s >= SQ_A1 && s <= SQ_H8;
}

constexpr bool isValidMove(Move m) {
    return m != MOVE_NONE && m != MOVE_NULL;
}

constexpr bool isValidPiece(Piece p) {
    switch(p) {
        case W_PAWN: case W_KNIGHT: case W_BISHOP: case W_ROOK: case W_QUEEN: case W_KING:
        case B_PAWN: case B_KNIGHT: case B_BISHOP: case B_ROOK: case B_QUEEN: case B_KING:
            return true;
        default:
            return false;
    }
}

constexpr Square square(File f, Rank r) {
    return Square((r << 3) + f);
}

constexpr File fileOf(Square sq) {
    return File(sq & 7);
}

constexpr Rank rankOf(Square sq) {
    return Rank(sq >> 3);
}

constexpr Bitboard bb(Rank r) {
  return Rank1BB << (8 * r);
}

constexpr Bitboard bb(File f) {
  return FileABB << f;
}

constexpr Square relativeSquare(Side side, Square sq) {
  return Square(sq ^ (side * 56));
}

constexpr Rank relativeRank(Side side, Rank r) {
  return Rank(r ^ (side * 7));
}

constexpr Rank relativeRank(Side side, Square sq) {
  return relativeRank(side, rankOf(sq));
}

constexpr Piece piece(Side side, PieceType p) {
    return Piece((side << 3) + p);
}

constexpr PieceType pieceType(Piece p) {
    return PieceType(p & 7);
}

constexpr Side side(Piece p) {
    assert(p != NO_PIECE);
    return Side(p >> 3);
}

constexpr Move makeMove(Square from, Square to) {
    return Move((from << 6) + to);
}

template<MoveType Type>
constexpr Move makeMove(Square from, Square to, PieceType pt = KNIGHT) {
    return Move(Type + ((pt - KNIGHT) << 12) + (from << 6) + to);
}

constexpr Square moveTo(Move m) {
    return Square(m & 0x3F);
}

constexpr Square moveFrom(Move m) {
    return Square((m >> 6) & 0x3F);
}

constexpr uint16_t moveFromTo(Move m) {
    return m & 0xFFF;
}

constexpr MoveType moveType(Move m) {
    return MoveType(m & (3 << 14));
}

constexpr PieceType movePromotionType(Move m) {
    return PieceType(((m >> 12) & 3) + KNIGHT);
}

constexpr Direction pawnDirection(Side s) {
    return s == WHITE ? UP : DOWN;
}

inline Bitboard operator&(Bitboard b, Square s) { return b & bb(s); }
inline Bitboard operator|(Bitboard b, Square s) { return b | bb(s); }
inline Bitboard operator^(Bitboard b, Square s) { return b ^ bb(s); }
inline Bitboard operator&(Square s, Bitboard b) { return b & s; }
inline Bitboard operator|(Square s, Bitboard b) { return b | s; }
inline Bitboard operator^(Square s, Bitboard b) { return b ^ s; }

inline Bitboard& operator|=(Bitboard& b, Square s) { return b |= bb(s); }
inline Bitboard& operator^=(Bitboard& b, Square s) { return b ^= bb(s); }
inline Bitboard  operator|(Square s1, Square s2) { return bb(s1) | bb(s2); }

} /* namespace bchess */

#include <string>
#include <array>
#include <vector>

#include <immintrin.h>
#include <cassert>

namespace bchess {

namespace BB {

void debug(Bitboard b);
void init();

} // namespace Bitboard

inline uint64_t pext(uint64_t b, uint64_t m) {
    return _pext_u64(b, m);
}

inline int popcount(Bitboard b) {
    return __builtin_popcountll(b);
}

inline Square bitscan(Bitboard b) {
    return Square(__builtin_ctzll(b));
    //return Square(_tzcnt_u64(b));
}
/*inline Square bitscan_reverse(Bitboard b) {
    return Square(63 - __builtin_clzll(b));
}*/

#define bitscan_loop(B) for(; B; B &= B - 1)
//#define bitscan_loop(B) for(; B; B = _blsr_u64(B))

struct PextEntry {
    Bitboard  mask;
    Bitboard *data;

    inline Bitboard attacks(Bitboard occupied) const {
        return data[pext(occupied, mask)];
    }
};

extern Bitboard PAWN_ATTACK[NB_SIDE][NB_SQUARE];
extern Bitboard KNIGHT_MOVE[NB_SQUARE];
extern Bitboard KING_MOVE[NB_SQUARE];
extern PextEntry BISHOP_MOVE[NB_SQUARE];
extern PextEntry ROOK_MOVE[NB_SQUARE];

extern Bitboard BETWEEN_BB[NB_SQUARE][NB_SQUARE];

template<Direction D>
constexpr Bitboard shift(Bitboard b)
{
    switch(D) {
        case UP:         return b << 8;
        case DOWN:       return b >> 8;
        case RIGHT:      return (b & ~FileHBB) << 1;
        case LEFT:       return (b & ~FileABB) >> 1;
        case UP_RIGHT:   return (b & ~FileHBB) << 9;
        case UP_LEFT:    return (b & ~FileABB) << 7;
        case DOWN_RIGHT: return (b & ~FileHBB) >> 7;
        case DOWN_LEFT:  return (b & ~FileABB) >> 9;
        default: static_assert(true, "invalid shift direction");
    }
}

inline Bitboard pawnAttacks(Side side, Square sq) {
    return PAWN_ATTACK[side][sq];
}

template<Side Me>
inline Bitboard pawnAttacks(Bitboard b) {
    if constexpr (Me == WHITE)
        return shift<UP_LEFT>(b) | shift<UP_RIGHT>(b);
    else
        return shift<DOWN_RIGHT>(b) | shift<DOWN_LEFT>(b);
}

template<PieceType Pt>
inline Bitboard sliderAttacks(Square sq, Bitboard occupied) {
    static_assert(Pt == ROOK || Pt == BISHOP);

    if constexpr (Pt == ROOK)
        return ROOK_MOVE[sq].attacks(occupied);
    else
        return BISHOP_MOVE[sq].attacks(occupied);
}

template<PieceType Pt>
inline Bitboard attacks(Square sq, Bitboard occupied = 0) {
    if constexpr (Pt == KING) return KING_MOVE[sq];
    if constexpr (Pt == KNIGHT) return KNIGHT_MOVE[sq];
    if constexpr (Pt == BISHOP) return sliderAttacks<BISHOP>(sq, occupied);
    if constexpr (Pt == ROOK) return sliderAttacks<ROOK>(sq, occupied);
    if constexpr (Pt == QUEEN) return sliderAttacks<BISHOP>(sq, occupied) | sliderAttacks<ROOK>(sq, occupied);
}

inline Bitboard betweenBB(Square from, Square to) {
    assert(isValidSq(from) && isValidSq(to));
    return BETWEEN_BB[from][to];
}

} /* namespace bchess */

#include <cstdint>

namespace bchess {

namespace Zobrist {
    inline Bitboard keys[NB_PIECE][NB_SQUARE];
    inline Bitboard enpassantKeys[NB_FILE+1];
    inline Bitboard castlingKeys[NB_CASTLING_RIGHT];
    inline Bitboard sideToMoveKey;

    inline uint64_t fastrand() {
        static uint64_t seed = 1234567890;

        uint64_t z = (seed += 0x9E3779B97F4A7C15ull); 
        z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ull; 
        z = (z ^ (z >> 27)) * 0x94D049BB133111EBull;
        return z ^ (z >> 31);
    }

    inline void init() {
        for (int i=0; i<NB_PIECE; i++) {
            for (int j=0; j<NB_SQUARE; j++) {
                keys[i][j] = fastrand();
            }
        }

        for (int i=0; i<NB_CASTLING_RIGHT; i++) {
            castlingKeys[i] = fastrand();
        }

        for (int i=0; i<NB_FILE; i++) {
            enpassantKeys[i] = fastrand();
        }
        enpassantKeys[NB_FILE] = 0; // used to avoid branching in doMove()

        sideToMoveKey = fastrand();
    }
}

} /* namespace bchess */

#define STARTPOS_FEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
#define KIWIPETE_FEN "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"

namespace bchess {

struct State {
    CastlingRight castlingRights;
    Square epSquare;
    int fiftyMoveRule;
    int halfMoves;
    Move move;

    Piece capture;

    uint64_t hash;
    Bitboard threatsFor[NB_PIECE_TYPE];
    Bitboard checkers;
    Bitboard checkMask;
    Bitboard pinDiag;
    Bitboard pinOrtho;

    inline State& prev() { return *(this-1); }
    inline const State& prev() const { return *(this-1); }

    inline State& next() { return *(this+1); }
    inline const State& next() const { return *(this+1); }
};

class Position {
public:
    Position();
    Position(const Position &other);
    Position& operator=(const Position &other);

    void reset();
    bool setFromFEN(const std::string &fen);
    std::string fen() const;
    
    inline void doMove(Move m) { getSideToMove() == WHITE ? doMove<WHITE>(m) : doMove<BLACK>(m); }
    template<Side Me> inline void doMove(Move m);

    inline void undoMove(Move m) { getSideToMove() == WHITE ? undoMove<WHITE>(m) : undoMove<BLACK>(m); }
    template<Side Me> inline void undoMove(Move m);

    template<Side Me> void doNullMove();
    template<Side Me> void undoNullMove();

    //bool givesCheck(Move m);

    inline Side getSideToMove() const { return sideToMove; }
    inline int getFiftyMoveRule() const { return state->fiftyMoveRule; }
    inline int getHalfMoves() const { return state->halfMoves; }
    inline int getFullMoves() const { return 1 + (getHalfMoves() - (sideToMove == BLACK)) / 2; }
    inline Square getEpSquare() const { return state->epSquare; }
    inline Piece getPieceAt(Square sq) const { return pieces[sq]; }
    inline bool isEmpty(Square sq) const { return getPieceAt(sq) == NO_PIECE; }
    inline bool isEmpty(Bitboard b) const { return !(b & getPiecesBB()); }
    inline bool canCastle(CastlingRight cr) const { return state->castlingRights & cr; }
    inline CastlingRight getCastlingRights() const { return state->castlingRights; }
    
    /*inline Bitboard getPiecesBB() const { return typeBB[ALL_PIECES]; }
    inline Bitboard getPiecesBB(Side side) const { return sideBB[side]; }
    inline Bitboard getPiecesBB(Side side, PieceType pt) const { return sideBB[side] & typeBB[pt]; }
    inline Bitboard getPiecesBB(Side side, PieceType pt1, PieceType pt2) const { return sideBB[side] & (typeBB[pt1] | typeBB[pt2]); }
    inline Square getKingSquare(Side side) const { return Square(bitscan(sideBB[side] & typeBB[KING])); }*/

    //inline Bitboard getPiecesBB(Side side) const { return side == WHITE ? piecesBB[W_PAWN]|piecesBB[W_KING]|piecesBB[W_KNIGHT]|piecesBB[W_BISHOP]|piecesBB[W_ROOK]|piecesBB[W_QUEEN] : piecesBB[B_PAWN]|piecesBB[B_KING]|piecesBB[B_KNIGHT]|piecesBB[B_BISHOP]|piecesBB[B_ROOK]|piecesBB[B_QUEEN]; }
    inline Bitboard getPiecesBB(Side side) const { return sideBB[side]; }
    inline Bitboard getPiecesBB() const { return getPiecesBB(WHITE) | getPiecesBB(BLACK); }
    inline Bitboard getPiecesBB(Side side, PieceType pt) const { return piecesBB[piece(side, pt)]; }
    //template<Side Me> inline Bitboard getPiecesBB(PieceType pt) const { return piecesBB[piece(Me, pt)]; }
    inline Bitboard getPiecesBB(Side side, PieceType pt1, PieceType pt2) const { return piecesBB[piece(side, pt1)] | piecesBB[piece(side, pt2)]; }
    //template<Side Me>  inline Bitboard getPiecesBB(PieceType pt1, PieceType pt2) const { return piecesBB[piece(Me, pt1)] | piecesBB[piece(Me, pt2)]; }
    inline Square getKingSquare(Side side) const { return Square(bitscan(piecesBB[side == WHITE ? W_KING : B_KING])); }
    //template<Side Me> inline Square getKingSquare() const { return Square(bitscan(piecesBB[Me == WHITE ? W_KING : B_KING])); }

    inline Bitboard getPiecesTypeBB(PieceType pt) const { return getPiecesBB(WHITE, pt) | getPiecesBB(BLACK, pt); }
    inline Bitboard getPiecesTypeBB(PieceType pt1, PieceType pt2) const { return getPiecesBB(WHITE, pt1, pt2) | getPiecesBB(BLACK, pt1, pt2); }

    inline Bitboard nbPieces(Side side) const { return popcount(getPiecesBB(side)); }
    inline Bitboard nbPieces() const { return popcount(getPiecesBB(WHITE) | getPiecesBB(BLACK)); }
    inline Bitboard nbPieces(Side side, PieceType pt) const { return popcount(getPiecesBB(side, pt)); }
    inline Bitboard nbPieces(Side side, PieceType pt1, PieceType pt2) const { return popcount(getPiecesBB(side, pt1, pt2)); }
    inline Bitboard nbPieceTypes(PieceType pt) const { return popcount(getPiecesBB(WHITE, pt) | getPiecesBB(BLACK, pt)); }

    inline Bitboard getEmptyBB() const { return ~getPiecesBB(); }

    inline Bitboard getAttackers(Square sq, Bitboard occupied) const;

    inline Bitboard threatsFor(PieceType pt) const { return state->threatsFor[pt]; }
    inline Bitboard checkedSquares() const { return threatsFor(KING); }
    inline Bitboard checkers() const { return state->checkers; }
    inline Bitboard nbCheckers() const { return popcount(state->checkers); }
    inline bool inCheck() const { return !!state->checkers; }

    inline uint64_t hash() const { return state->hash; }
    uint64_t computeHash() const;
    inline uint64_t getHashAfter(Move m) const;
    inline uint64_t getHashAfterNullMove() const { return hash() ^ Zobrist::sideToMoveKey; };

    inline Bitboard checkMask() const { return state->checkMask; }
    inline Bitboard pinDiag() const { return state->pinDiag; }
    inline Bitboard pinOrtho() const { return state->pinOrtho; }

    inline size_t historySize() const { return state - history; }

    // Check if a position occurs 3 times in the game history
    inline bool isRepetitionDraw() const;

    inline bool isFiftyMoveDraw() const { return state->fiftyMoveRule > 99; }
    template<Side Me> inline bool hasNonPawnMateriel() { return getPiecesBB(Me, PAWN, KING) != getPiecesBB(Me); }

    template<Side Me> bool isLegal(Move m) const;
    inline bool isLegal(Move m) const { return getSideToMove() == WHITE ? isLegal<WHITE>(m) : isLegal<BLACK>(m); };
    inline bool isCapture(Move m) const { return getPieceAt(moveTo(m)) != NO_PIECE || moveType(m) == EN_PASSANT; }
    inline bool isTactical(Move m) const { return isCapture(m) || (moveType(m) == PROMOTION && movePromotionType(m) == QUEEN); }

    inline Move previousMove() const { return state->move; }
    
    bool see(Move m, int threshold) const ;

    std::string debugHistory();

private:
    void setCastlingRights(CastlingRight cr);

    template<Side Me, MoveType Mt> void doMove(Move m);
    template<Side Me, MoveType Mt> void undoMove(Move m);

    template<Side Me, bool InCheck, bool IsCapture> bool isLegal(Move m, Piece pc) const;

    template<Side Me> inline void setPiece(Square sq, Piece p);
    template<Side Me> inline void unsetPiece(Square sq);
    template<Side Me> inline void movePiece(Square from, Square to);

    template<Side Me> inline void updateThreatenedSquares();
    template<Side Me> inline void updateCheckers();
    template<Side Me, bool InCheck> inline void updatePinsAndCheckMask();

    inline void updateBitboards();
    template<Side Me> inline void updateBitboards();

    Piece pieces[NB_SQUARE];
    //Bitboard typeBB[NB_PIECE_TYPE];
    Bitboard sideBB[NB_SIDE];
    Bitboard piecesBB[NB_PIECE];

    Side sideToMove;

    State *state;
    State history[MAX_HISTORY];
};

std::ostream& operator<<(std::ostream& os, const Position& pos);

inline Bitboard Position::getAttackers(Square sq, Bitboard occupied) const {
    return ((pawnAttacks(BLACK, sq) & getPiecesBB(WHITE, PAWN))
          | (pawnAttacks(WHITE, sq) & getPiecesBB(BLACK, PAWN))
          | (attacks<KNIGHT>(sq) & getPiecesTypeBB(KNIGHT))
          | (attacks<BISHOP>(sq, occupied) & getPiecesTypeBB(BISHOP, QUEEN))
          | (attacks<ROOK>(sq, occupied) & getPiecesTypeBB(ROOK, QUEEN))
          | (attacks<KING>(sq) & getPiecesTypeBB(KING))
    );
}

// Check if a position occurs 3 times in the game history
inline bool Position::isRepetitionDraw() const {
    if (getFiftyMoveRule() < 4)
        return false;

    int reps = 0;
    //State *start = std::max(history + 2, state - getFiftyMoveRule());
    const State *historyStart = history+2;
    const State *fiftyMoveStart = state - getFiftyMoveRule();
    const State *start = fiftyMoveStart > historyStart ? fiftyMoveStart : historyStart;

    for (State *st = state - 2; st >= start; st -= 2) {
        assert(st >= history && st <= state);

        if (st->hash == state->hash && ++reps == 2) {
            return true;
        }
    }

    return false;
}

template<Side Me>
inline void Position::doMove(Move m) {
    switch(moveType(m)) {
        case NORMAL:     doMove<Me, NORMAL>(m); return;
        case CASTLING:   doMove<Me, CASTLING>(m); return;
        case PROMOTION:  doMove<Me, PROMOTION>(m); return;
        case EN_PASSANT: doMove<Me, EN_PASSANT>(m); return;
    }
}

template<Side Me>
inline void Position::undoMove(Move m) {
    switch(moveType(m)) {
        case NORMAL:     undoMove<Me, NORMAL>(m); return;
        case CASTLING:   undoMove<Me, CASTLING>(m); return;
        case PROMOTION:  undoMove<Me, PROMOTION>(m); return;
        case EN_PASSANT: undoMove<Me, EN_PASSANT>(m); return;
    }
}

inline uint64_t Position::getHashAfter(Move m) const {
    uint64_t h = hash();
    const Square from = moveFrom(m), to = moveTo(m);
    const Piece p = getPieceAt(from);
    const Piece capture = getPieceAt(to);

    h ^= Zobrist::sideToMoveKey;
    h ^= Zobrist::keys[p][from] ^ Zobrist::keys[p][to];
    if (capture != NO_PIECE) 
        h ^= Zobrist::keys[capture][to];

    return h;
}

} /* namespace bchess */

namespace bchess {

constexpr Score Tempo = 10;

constexpr Score PawnValueMg = 85;
constexpr Score PawnValueEg = 100;

constexpr Score KnightValueMg = 335;
constexpr Score KnightValueEg = 285;

constexpr Score BishopValueMg = 360;
constexpr Score BishopValueEg = 300;

constexpr Score RookValueMg = 455;
constexpr Score RookValueEg = 525;

constexpr Score QueenValueMg = 1025;
constexpr Score QueenValueEg = 985;

constexpr Score PIECE_TYPE_VALUE[NB_PIECE_TYPE][NB_PHASE] = {
    {},
    {PawnValueMg, PawnValueEg},
    {KnightValueMg, KnightValueEg},
    {BishopValueMg, BishopValueEg},
    {RookValueMg, RookValueEg},
    {QueenValueMg, QueenValueEg},
};

constexpr Score PIECE_VALUE[NB_PIECE][NB_PHASE] = {
    {},
    {PawnValueMg, PawnValueEg}, {KnightValueMg, KnightValueEg}, {BishopValueMg, BishopValueEg}, {RookValueMg, RookValueEg}, {QueenValueMg, QueenValueEg},
    {}, {}, {},
    {PawnValueMg, PawnValueEg}, {KnightValueMg, KnightValueEg}, {BishopValueMg, BishopValueEg}, {RookValueMg, RookValueEg}, {QueenValueMg, QueenValueEg},
};

template <Phase P>
constexpr Score PieceValue(PieceType pt) { return PIECE_TYPE_VALUE[pt][P]; }

template <Phase P>
constexpr Score PieceValue(Piece p) { return PIECE_VALUE[p][P]; }

constexpr int PHASE_TOTAL = 24;

constexpr Score PSQT[NB_PIECE_TYPE][NB_PHASE][NB_SQUARE] = {
    {},
    // Pawn
    {
        {
            0,   0,   0,   0,   0,   0,  0,   0,
            -35,  -1, -20, -23, -15,  24, 38, -22,
            -26,  -4,  -4, -10,   3,   3, 33, -12,
            -27,  -2,  -5,  12,  17,   6, 10, -25,
            -14,  13,   6,  21,  23,  12, 17, -23,
            -6,   7,  26,  31,  65,  56, 25, -20,
            98, 134,  61,  95,  68, 126, 34, -11,
            0,   0,   0,   0,   0,   0,  0,   0,
        },
        {
            0,   0,   0,   0,   0,   0,   0,   0,
            13,   8,   8,  10,  13,   0,   2,  -7,
            4,   7,  -6,   1,   0,  -5,  -1,  -8,
            13,   9,  -3,  -7,  -7,  -8,   3,  -1,
            32,  24,  13,   5,  -2,   4,  17,  17,
            94, 100,  85,  67,  56,  53,  82,  84,
            178, 173, 158, 134, 147, 132, 165, 187,
            0,   0,   0,   0,   0,   0,   0,   0,
        }
    },
    // Knight
    {
        {
            -105, -21, -58, -33, -17, -28, -19,  -23,
            -29, -53, -12,  -3,  -1,  18, -14,  -19,
            -23,  -9,  12,  10,  19,  17,  25,  -16,
            -13,   4,  16,  13,  28,  19,  21,   -8,
            -9,  17,  19,  53,  37,  69,  18,   22,
            -47,  60,  37,  65,  84, 129,  73,   44,
            -73, -41,  72,  36,  23,  62,   7,  -17,
            -167, -89, -34, -49,  61, -97, -15, -107,
        },
        {
            -29, -51, -23, -15, -22, -18, -50, -64,
            -42, -20, -10,  -5,  -2, -20, -23, -44,
            -23,  -3,  -1,  15,  10,  -3, -20, -22,
            -18,  -6,  16,  25,  16,  17,   4, -18,
            -17,   3,  22,  22,  22,  11,   8, -18,
            -24, -20,  10,   9,  -1,  -9, -19, -41,
            -25,  -8, -25,  -2,  -9, -25, -24, -52,
            -58, -38, -13, -28, -31, -27, -63, -99,
        }
    },
    // Bishop
    {
        {
            -33,  -3, -14, -21, -13, -12, -39, -21,
            4,  15,  16,   0,   7,  21,  33,   1,
            0,  15,  15,  15,  14,  27,  18,  10,
            -6,  13,  13,  26,  34,  12,  10,   4,
            -4,   5,  19,  50,  37,  37,   7,  -2,
            -16,  37,  43,  40,  35,  50,  37,  -2,
            -26,  16, -18, -13,  30,  59,  18, -47,
            -29,   4, -82, -37, -25, -42,   7,  -8,
        },
        {
            -23,  -9, -23,  -5, -9, -16,  -5, -17,
            -14, -18,  -7,  -1,  4,  -9, -15, -27,
            -12,  -3,   8,  10, 13,   3,  -7, -15,
            -6,   3,  13,  19,  7,  10,  -3,  -9,
            -3,   9,  12,   9, 14,  10,   3,   2,
            2,  -8,   0,  -1, -2,   6,   0,   4,
            -8,  -4,   7, -12, -3, -13,  -4, -14,
            -14, -21, -11,  -8, -7,  -9, -17, -24,
        }
    },
    // Rook
    {
        {
            -19, -13,   1,  17, 16,  7, -37, -26,
            -44, -16, -20,  -9, -1, 11,  -6, -71,
            -45, -25, -16, -17,  3,  0,  -5, -33,
            -36, -26, -12,  -1,  9, -7,   6, -23,
            -24, -11,   7,  26, 24, 35,  -8, -20,
            -5,  19,  26,  36, 17, 45,  61,  16,
            27,  32,  58,  62, 80, 67,  26,  44,
            32,  42,  32,  51, 63,  9,  31,  43,
        },
        {
            -9,  2,  3, -1, -5, -13,   4, -20,
            -6, -6,  0,  2, -9,  -9, -11,  -3,
            -4,  0, -5, -1, -7, -12,  -8, -16,
            3,  5,  8,  4, -5,  -6,  -8, -11,
            4,  3, 13,  1,  2,   1,  -1,   2,
            7,  7,  7,  5,  4,  -3,  -5,  -3,
            11, 13, 13, 11, -3,   3,   8,   3,
            13, 10, 18, 15, 12,  12,   8,   5,
        }
    },
    // Queen
    {
        {
            -1, -18,  -9,  10, -15, -25, -31, -50,
            -35,  -8,  11,   2,   8,  15,  -3,   1,
            -14,   2, -11,  -2,  -5,   2,  14,   5,
            -9, -26,  -9, -10,  -2,  -4,   3,  -3,
            -27, -27, -16, -16,  -1,  17,  -2,   1,
            -13, -17,   7,   8,  29,  56,  47,  57,
            -24, -39,  -5,   1, -16,  57,  28,  54,
            -28,   0,  29,  12,  59,  44,  43,  45,
        },
        {
            -33, -28, -22, -43,  -5, -32, -20, -41,
            -22, -23, -30, -16, -16, -23, -36, -32,
            -16, -27,  15,   6,   9,  17,  10,   5,
            -18,  28,  19,  47,  31,  34,  39,  23,
            3,  22,  24,  45,  57,  40,  57,  36,
            -20,   6,   9,  49,  47,  35,  19,   9,
            -17,  20,  32,  41,  58,  25,  30,   0,
            -9,  22,  22,  27,  27,  19,  10,  20,
        }
    },
    // King
    {
        {
            -15,  36,  12, -54,   8, -28,  24,  14,
            1,   7,  -8, -64, -43, -16,   9,   8,
            -14, -14, -22, -46, -44, -30, -15, -27,
            -49,  -1, -27, -39, -46, -44, -33, -51,
            -17, -20, -12, -27, -30, -25, -14, -36,
            -9,  24,   2, -16, -20,   6,  22, -22,
            29,  -1, -20,  -7,  -8,  -4, -38, -29,
            -65,  23,  16, -15, -56, -34,   2,  13,
        },
        {
            -53, -34, -21, -11, -28, -14, -24, -43
            -27, -11,   4,  13,  14,   4,  -5, -17,
            -19,  -3,  11,  21,  23,  16,   7,  -9,
            -18,  -4,  21,  24,  27,  23,   9, -11,
            -8,  22,  24,  27,  26,  33,  26,   3,
            10,  17,  23,  15,  20,  45,  44,  13,
            -12,  17,  14,  17,  17,  38,  23,  11,
            -74, -35, -18, -18, -11,  15,   4, -17,
        }

    }
};

template<Side Me>
Score evaluate(const Position &pos);

inline Score evaluate(const Position &pos) {
    return pos.getSideToMove() == WHITE ? evaluate<WHITE>(pos) : evaluate<BLACK>(pos);
};

} /* namespace bchess */

#include <initializer_list>
#include <cassert>
#include <cstdint>
#include <algorithm>

namespace bchess {

template <typename T, int N, typename SizeT = uint32_t> class fixed_vector {
    T elements[N];
    SizeT count;
public:
    typedef T* iterator;
    typedef const T* const_iterator;

    inline iterator begin() { return &elements[0]; }
    inline const_iterator begin() const { return &elements[0]; }
    inline iterator end() { return &elements[count]; }
    inline const_iterator end() const { return &elements[count]; }

    fixed_vector() noexcept : count(0) {}
    inline fixed_vector(std::initializer_list<T> il) {copy(il.begin(), il.end(), begin());count = il.end() - il.begin();}
    inline fixed_vector(fixed_vector const &ml) = default;
    inline fixed_vector(fixed_vector&&)=default;
    inline fixed_vector& operator=(fixed_vector const &ml)=default;
    inline fixed_vector& operator=(fixed_vector&&)=default;
    inline T &operator[](SizeT i) {assert(i < count); return elements[i];}
    inline const T &operator[](SizeT i) const {assert(i < count); return elements[i];}
    inline const T &front() const {assert(count > 0); return elements[0]; }
    inline T &front() {assert(count > 0); return elements[0]; }
    inline const T &back() const {assert(count > 0); return elements[count-1]; }
    inline T &back() {assert(count > 0); return elements[count-1]; }
    inline void push_back(const T& e) {assert(count < N); elements[count++] = e;}
    inline void push_back(T&& e) {assert(count < N); elements[count++] = e;}
    inline void pop_back() {assert(count > 0); count--;}
    inline void clear() {count = 0;}
    inline SizeT size() const {return count;}
    inline void resize(SizeT s) {assert(s <= count); count = s;} // only resize to smaller count
    inline bool empty() const {return count==0;}
    inline bool contains(const T &e) { return std::find(begin(), end(), e) != end(); }
    inline SizeT capacity() const {return N;}
    inline iterator erase (const_iterator _pos) {
        iterator pos = begin() + (_pos - begin());
        std::copy(pos+1, end(), pos);count--;
        return pos;
    }
    inline iterator erase (const_iterator _first, const_iterator _last) {
        iterator first = begin() + (_first - begin());
        iterator last = begin() + (_last - begin());
        std::copy(last, end(), first);
        this->count -= last-first;return first;
    }
    
    inline iterator insert(const_iterator _first, const_iterator _last) {
        assert(std::distance(_first, _last) + count <= N);
        std::copy(_first, _last, end());
        count += std::distance(_first, _last);
        return end();
    }

    template<typename Compare>
    inline void insert_sorted(const T& e, Compare comp) {
        T *cur = end();

        for (; cur != begin() && comp(e, *(cur - 1)); cur--) {
            *cur = *(cur - 1);
        }

        *cur = e;
        count++;
    }

    template<typename... Args> inline void emplace_back(Args&&... args) {
        assert(count < N);
        new(&elements[count++]) T(std::forward<Args>(args)...);
    }
};

} /* namespace bchess */

namespace bchess {

using MoveList = fixed_vector<Move, MAX_MOVE, uint8_t>;

std::ostream& operator<<(std::ostream& os, const MoveList& pos);

enum MoveGenType {
    QUIET_MOVES = 1,
    TACTICAL_MOVES = 2,
    ALL_MOVES = QUIET_MOVES | TACTICAL_MOVES,
};

#define CALL_HANDLER(...) if (!handler(__VA_ARGS__)) return false

#define CALL_ENUMERATOR(...) if (!__VA_ARGS__) return false

template<Side Me, PieceType PromotionType, typename Handler>
inline bool enumeratePromotion(Square from, Square to, const Handler& handler) {
    CALL_HANDLER(makeMove<PROMOTION>(from, to, PromotionType));

    return true;
}

template<Side Me, MoveGenType MGType = ALL_MOVES, typename Handler>
inline bool enumeratePromotions(Square from, Square to, const Handler& handler) {
    if constexpr (MGType & TACTICAL_MOVES) {
        CALL_ENUMERATOR(enumeratePromotion<Me, QUEEN>(from, to, handler));
    }
    
    if constexpr (MGType & QUIET_MOVES) {
        CALL_ENUMERATOR(enumeratePromotion<Me, KNIGHT>(from, to, handler));
        CALL_ENUMERATOR(enumeratePromotion<Me, ROOK>(from, to, handler));
        CALL_ENUMERATOR(enumeratePromotion<Me, BISHOP>(from, to, handler));
    }

    return true;
}

template<Side Me, bool InCheck, MoveGenType MGType = ALL_MOVES, typename Handler>
inline bool enumeratePawnPromotionMoves(const Position &pos, Bitboard source, const Handler& handler) {
    constexpr Side Opp = ~Me;
    //constexpr Bitboard Rank3 = (Me == WHITE) ? Rank3BB : Rank6BB;
    constexpr Bitboard Rank7 = (Me == WHITE) ? Rank7BB : Rank2BB;
    constexpr Direction Up = (Me == WHITE) ? UP : DOWN;
    constexpr Direction UpLeft = (Me == WHITE) ? UP_LEFT : DOWN_RIGHT;
    constexpr Direction UpRight = (Me == WHITE) ? UP_RIGHT : DOWN_LEFT;

    Bitboard emptyBB = pos.getEmptyBB();
    Bitboard pinOrtho = pos.pinOrtho();
    Bitboard pinDiag = pos.pinDiag();
    Bitboard checkMask = pos.checkMask();

    Bitboard pawnsCanPromote = source & Rank7;
    if (pawnsCanPromote) {
        // Capture Promotion
        {
            Bitboard pawns = pawnsCanPromote & ~pinOrtho;
            Bitboard capLPromotions = (shift<UpLeft>(pawns & ~pinDiag) | (shift<UpLeft>(pawns & pinDiag) & pinDiag)) & pos.getPiecesBB(Opp);
            Bitboard capRPromotions = (shift<UpRight>(pawns & ~pinDiag) | (shift<UpRight>(pawns & pinDiag) & pinDiag)) & pos.getPiecesBB(Opp);

            if constexpr (InCheck) {
                capLPromotions &= checkMask;
                capRPromotions &= checkMask;
            }

            bitscan_loop(capLPromotions) {
                Square to = bitscan(capLPromotions);
                Square from = to - UpLeft;
                CALL_ENUMERATOR(enumeratePromotions<Me, MGType>(from, to, handler));
            }
            bitscan_loop(capRPromotions) {
                Square to = bitscan(capRPromotions);
                Square from = to - UpRight;
                CALL_ENUMERATOR(enumeratePromotions<Me, MGType>(from, to, handler));
            }
        }

        // Quiet Promotion
        {
            Bitboard pawns = pawnsCanPromote & ~pinDiag;
            Bitboard quietPromotions = (shift<Up>(pawns & ~pinOrtho) | (shift<Up>(pawns & pinOrtho) & pinOrtho)) & emptyBB;

            if constexpr (InCheck) quietPromotions &= checkMask;

            bitscan_loop(quietPromotions) {
                Square to = bitscan(quietPromotions);
                Square from = to - Up;
                CALL_ENUMERATOR(enumeratePromotions<Me, MGType>(from, to, handler));
            }
        }
    }

    return true;
}

template<Side Me, bool InCheck, MoveGenType MGType = ALL_MOVES, typename Handler>
inline bool enumeratePawnEnpassantMoves(const Position &pos, Bitboard source, const Handler& handler) {
    constexpr Side Opp = ~Me;

    Bitboard pinOrtho = pos.pinOrtho();
    Bitboard pinDiag = pos.pinDiag();
    Bitboard checkMask = pos.checkMask();

    // Enpassant
    if (pos.getEpSquare() != SQ_NONE) {
        assert(rankOf(pos.getEpSquare()) == relativeRank(Me, RANK_6));
        assert(pos.isEmpty(pos.getEpSquare()));
        assert(pieceType(pos.getPieceAt(pos.getEpSquare() - pawnDirection(Me))) == PAWN);

        Bitboard epcapture = bb(pos.getEpSquare() - pawnDirection(Me)); // Pawn who will be captured enpassant
        Bitboard enpassants = pawnAttacks(Opp, pos.getEpSquare()) & source & ~pinOrtho;

        if constexpr (InCheck) {
            // Case: 3k4/8/8/4pP2/3K4/8/8/8 w - e6 0 2
            // If we are in check by the pawn who can be captured enpasant don't use the checkMask because it does not contain the EpSquare
            if (!(pos.checkers() & epcapture)) {
                enpassants &= checkMask;
            }
        }

        bitscan_loop(enpassants) {
            Square from = bitscan(enpassants);
            Bitboard epRank = bb(rankOf(from));

            // Case: 2r1k2r/Pp3ppp/1b3nbN/nPPp4/BB2P3/q2P1N2/Pp4PP/R2Q1RK1 w k d6 0 3
            // Pawn is pinned diagonaly and EpSquare is not part of the pin mask
            if ( (bb(from) & pinDiag) && !(bb(pos.getEpSquare()) & pinDiag))
                continue;
            
            // Case: 3k4/8/8/2KpP2r/8/8/8/8 w - - 0 2
            // Case: 8/2p5/8/KP1p2kr/5pP1/8/4P3/6R1 b - g3 0 3
            // Check for the special case where our king is on the same rank than an opponent rook(or queen) and enpassant capture will put us in check
            if ( !((epRank & pos.getPiecesBB(Me, KING)) && (epRank & pos.getPiecesBB(Opp, ROOK, QUEEN)))
                || !(attacks<ROOK>(pos.getKingSquare(Me), pos.getPiecesBB() ^ bb(from) ^ epcapture) & pos.getPiecesBB(Opp, ROOK, QUEEN)) )
            {
                Square to = pos.getEpSquare();
                CALL_HANDLER(makeMove<EN_PASSANT>(from, to));
            }
        }
    }

    return true;
}

template<Side Me, bool InCheck, MoveGenType MGType = ALL_MOVES, typename Handler>
inline bool enumeratePawnNormalMoves(const Position &pos, Bitboard source, const Handler& handler) {
    constexpr Side Opp = ~Me;
    constexpr Bitboard Rank3 = (Me == WHITE) ? Rank3BB : Rank6BB;
    constexpr Bitboard Rank7 = (Me == WHITE) ? Rank7BB : Rank2BB;
    constexpr Direction Up = (Me == WHITE) ? UP : DOWN;
    constexpr Direction UpLeft = (Me == WHITE) ? UP_LEFT : DOWN_RIGHT;
    constexpr Direction UpRight = (Me == WHITE) ? UP_RIGHT : DOWN_LEFT;

    Bitboard emptyBB = pos.getEmptyBB();
    Bitboard pinOrtho = pos.pinOrtho();
    Bitboard pinDiag = pos.pinDiag();
    Bitboard checkMask = pos.checkMask();

    // Single & Double Push
    if constexpr (MGType & QUIET_MOVES) {
        Bitboard pawns = source & ~Rank7 & ~pinDiag;
        Bitboard singlePushes = (shift<Up>(pawns & ~pinOrtho) | (shift<Up>(pawns & pinOrtho) & pinOrtho)) & emptyBB;
        Bitboard doublePushes = shift<Up>(singlePushes & Rank3) & emptyBB;

        if constexpr (InCheck) {
            singlePushes &= checkMask;
            doublePushes &= checkMask;
        }

        bitscan_loop(singlePushes) {
            Square to = bitscan(singlePushes);
            Square from = to - Up;
            CALL_HANDLER(makeMove(from, to));
        }

        bitscan_loop(doublePushes) {
            Square to = bitscan(doublePushes);
            Square from = to - Up - Up;
            CALL_HANDLER(makeMove(from, to));
        }
    }

    // Normal Capture
    if constexpr (MGType & TACTICAL_MOVES) {
        Bitboard pawns = source & ~Rank7 & ~pinOrtho;
        Bitboard capL = (shift<UpLeft>(pawns & ~pinDiag) | (shift<UpLeft>(pawns & pinDiag) & pinDiag)) & pos.getPiecesBB(Opp);
        Bitboard capR = (shift<UpRight>(pawns & ~pinDiag) | (shift<UpRight>(pawns & pinDiag) & pinDiag)) & pos.getPiecesBB(Opp);

        if constexpr (InCheck) {
            capL &= checkMask;
            capR &= checkMask;
        }

        bitscan_loop(capL) {
            Square to = bitscan(capL);
            Square from = to - UpLeft;
            CALL_HANDLER(makeMove(from, to));
        }
        bitscan_loop(capR) {
            Square to = bitscan(capR);
            Square from = to - UpRight;
            CALL_HANDLER(makeMove(from, to));
        }
    }

    return true;
}

template<Side Me, bool InCheck, MoveGenType MGType = ALL_MOVES, typename Handler>
inline bool enumeratePawnMoves(const Position &pos, Bitboard source, const Handler& handler) {
    CALL_ENUMERATOR(enumeratePawnNormalMoves<Me, InCheck, MGType, Handler>(pos, source, handler));
    CALL_ENUMERATOR(enumeratePawnPromotionMoves<Me, InCheck, MGType, Handler>(pos, source, handler));
    
    if constexpr (MGType & TACTICAL_MOVES) {
        CALL_ENUMERATOR(enumeratePawnEnpassantMoves<Me, InCheck, MGType, Handler>(pos, source, handler));
    }

    return true;
}

template<Side Me, typename Handler>
inline bool enumerateCastlingMoves(const Position &pos, const Handler& handler) {
    Square kingSquare = pos.getKingSquare(Me);

    for (auto cr : {Me & KING_SIDE, Me & QUEEN_SIDE}) {
        if (pos.canCastle(cr) && pos.isEmpty(CastlingPath[cr]) && !(pos.checkedSquares() & CastlingKingPath[cr])) {
            Square from = kingSquare;
            Square to = CastlingKingTo[cr];
            CALL_HANDLER(makeMove<CASTLING>(from, to));
        }
    }

    return true;
}

template<Side Me, MoveGenType MGType = ALL_MOVES, typename Handler>
inline bool enumerateKingMoves(const Position &pos, Square from, const Handler& handler) {
    Bitboard dest = attacks<KING>(from) & ~pos.getPiecesBB(Me) & ~pos.checkedSquares();

    if constexpr (MGType == TACTICAL_MOVES) dest &= pos.getPiecesBB(~Me);
    if constexpr (MGType == QUIET_MOVES) dest &= ~pos.getPiecesBB(~Me);

    bitscan_loop(dest) {
        Square to = bitscan(dest);
        CALL_HANDLER(makeMove(from, to));
    }

    return true;
}

template<Side Me, bool InCheck, MoveGenType MGType = ALL_MOVES, typename Handler>
inline bool enumerateKnightMoves(const Position &pos, Bitboard source, const Handler& handler) {
    Bitboard pieces = source & ~(pos.pinDiag() | pos.pinOrtho());

    bitscan_loop(pieces) {
        Square from = bitscan(pieces);
        Bitboard dest = attacks<KNIGHT>(from) & ~pos.getPiecesBB(Me);

        if constexpr (InCheck) dest &= pos.checkMask();
        if constexpr (MGType == TACTICAL_MOVES) dest &= pos.getPiecesBB(~Me);
        if constexpr (MGType == QUIET_MOVES) dest &= ~pos.getPiecesBB(~Me);

        bitscan_loop(dest) {
            Square to = bitscan(dest);
            CALL_HANDLER(makeMove(from, to));
        }
    }

    return true;
}

template<Side Me, bool InCheck, MoveGenType MGType = ALL_MOVES, typename Handler>
inline bool enumerateBishopSliderMoves(const Position &pos, Bitboard source, const Handler& handler) {
    Bitboard pieces;
    Bitboard oppPiecesBB = pos.getPiecesBB(~Me);

    // Non-pinned Bishop & Queen
    pieces = source & ~pos.pinOrtho() & ~pos.pinDiag();
    bitscan_loop(pieces) {
        Square from = bitscan(pieces);
        Bitboard dest = attacks<BISHOP>(from, pos.getPiecesBB()) & ~pos.getPiecesBB(Me);

        if constexpr (InCheck) dest &= pos.checkMask();
        if constexpr (MGType == TACTICAL_MOVES) dest &= oppPiecesBB;
        if constexpr (MGType == QUIET_MOVES) dest &= ~oppPiecesBB;

        bitscan_loop(dest) {
            Square to = bitscan(dest);
            CALL_HANDLER(makeMove(from, to));
        }
    }

    // Pinned Bishop & Queen
    pieces = source & ~pos.pinOrtho() & pos.pinDiag();
    bitscan_loop(pieces) {
        Square from = bitscan(pieces);
        Bitboard dest = attacks<BISHOP>(from, pos.getPiecesBB()) & ~pos.getPiecesBB(Me) & pos.pinDiag();

        if constexpr (InCheck) dest &= pos.checkMask();
        if constexpr (MGType == TACTICAL_MOVES) dest &= oppPiecesBB;
        if constexpr (MGType == QUIET_MOVES) dest &= ~oppPiecesBB;

        bitscan_loop(dest) {
            Square to = bitscan(dest);
            CALL_HANDLER(makeMove(from, to));
        }
    }

    return true;
}

template<Side Me, bool InCheck, MoveGenType MGType = ALL_MOVES, typename Handler>
inline bool enumerateRookSliderMoves(const Position &pos, Bitboard source, const Handler& handler) {
    Bitboard pieces;
    Bitboard oppPiecesBB = pos.getPiecesBB(~Me);

    // Non-pinned Rook & Queen
    pieces = source & ~pos.pinDiag() & ~pos.pinOrtho();
    bitscan_loop(pieces) {
        Square from = bitscan(pieces);
        Bitboard dest = attacks<ROOK>(from, pos.getPiecesBB()) & ~pos.getPiecesBB(Me);

        if constexpr (InCheck) dest &= pos.checkMask();
        if constexpr (MGType == TACTICAL_MOVES) dest &= oppPiecesBB;
        if constexpr (MGType == QUIET_MOVES) dest &= ~oppPiecesBB;

        bitscan_loop(dest) {
            Square to = bitscan(dest);
            CALL_HANDLER(makeMove(from, to));
        }
    }

    // Pinned Rook & Queen
    pieces = source & ~pos.pinDiag() & pos.pinOrtho();
    bitscan_loop(pieces) {
        Square from = bitscan(pieces);
        Bitboard dest = attacks<ROOK>(from, pos.getPiecesBB()) & ~pos.getPiecesBB(Me) & pos.pinOrtho();

        if constexpr (InCheck) dest &= pos.checkMask();
        if constexpr (MGType == TACTICAL_MOVES) dest &= oppPiecesBB;
        if constexpr (MGType == QUIET_MOVES) dest &= ~oppPiecesBB;

        bitscan_loop(dest) {
            Square to = bitscan(dest);
            CALL_HANDLER(makeMove(from, to));
        }
    }

    return true;
}

// Used only for Position::isLegal
template<Side Me, bool InCheck, MoveGenType MGType = ALL_MOVES, typename Handler>
inline bool enumerateQueenSliderMoves(const Position &pos, Bitboard source, const Handler& handler) {
    CALL_ENUMERATOR(enumerateBishopSliderMoves<Me, InCheck, MGType, Handler>(pos, source, handler));
    CALL_ENUMERATOR(enumerateRookSliderMoves<Me, InCheck, MGType, Handler>(pos, source, handler));
    return true;
}

template<Side Me, MoveGenType MGType = ALL_MOVES, typename Handler>
inline bool enumerateLegalMoves(const Position &pos, const Handler& handler) {
    assert(pos.nbCheckers() < 3);

    switch(pos.nbCheckers()) {
        case 0:
            CALL_ENUMERATOR(enumeratePawnMoves<Me, false, MGType, Handler>(pos, pos.getPiecesBB(Me, PAWN), handler));
            CALL_ENUMERATOR(enumerateKnightMoves<Me, false, MGType, Handler>(pos, pos.getPiecesBB(Me, KNIGHT), handler));
            CALL_ENUMERATOR(enumerateBishopSliderMoves<Me, false, MGType, Handler>(pos, pos.getPiecesBB(Me, BISHOP, QUEEN), handler));
            CALL_ENUMERATOR(enumerateRookSliderMoves<Me, false, MGType, Handler>(pos, pos.getPiecesBB(Me, ROOK, QUEEN), handler));
            if constexpr (MGType & QUIET_MOVES) CALL_ENUMERATOR(enumerateCastlingMoves<Me, Handler>(pos, handler));
            CALL_ENUMERATOR(enumerateKingMoves<Me, MGType, Handler>(pos, pos.getKingSquare(Me), handler));

            return true;
        case 1:
            CALL_ENUMERATOR(enumeratePawnMoves<Me, true, ALL_MOVES, Handler>(pos, pos.getPiecesBB(Me, PAWN), handler));
            CALL_ENUMERATOR(enumerateKnightMoves<Me, true, ALL_MOVES, Handler>(pos, pos.getPiecesBB(Me, KNIGHT), handler));
            CALL_ENUMERATOR(enumerateBishopSliderMoves<Me, true, ALL_MOVES, Handler>(pos, pos.getPiecesBB(Me, BISHOP, QUEEN), handler));
            CALL_ENUMERATOR(enumerateRookSliderMoves<Me, true, ALL_MOVES, Handler>(pos, pos.getPiecesBB(Me, ROOK, QUEEN), handler));
            CALL_ENUMERATOR(enumerateKingMoves<Me, ALL_MOVES, Handler>(pos, pos.getKingSquare(Me), handler));

            return true;
        default: //case 2:
            // If we are in double check only king moves are allowed
            CALL_ENUMERATOR(enumerateKingMoves<Me, ALL_MOVES, Handler>(pos, pos.getKingSquare(Me), handler));

            return true;
    }
}

template<MoveGenType MGType = ALL_MOVES, typename Handler>
inline bool enumerateLegalMoves(const Position &pos, const Handler& handler) {
    return pos.getSideToMove() == WHITE 
            ? enumerateLegalMoves<WHITE, MGType, Handler>(pos, handler)
            : enumerateLegalMoves<BLACK, MGType, Handler>(pos, handler);
}

inline void generateLegalMoves(const Position &pos, MoveList &moves) {
    enumerateLegalMoves(pos, [&](Move m) {
        moves.push_back(m); return true;
    });
}

} /* namespace bchess */

#include <cstdint>

namespace bchess {

using MoveScore = int32_t;

using PartialMoveList = fixed_vector<Move, 32, uint8_t>;

class MoveHistory {
public:
    MoveHistory(): counterMoves{}, killerMoves{}, history{} { }

    inline void clearKillers(int ply) {
        assert(ply >= 0 && ply < MAX_PLY + 1);
        killerMoves[ply][0] = killerMoves[ply][1] = MOVE_NONE;
    }

    template<int K> inline Move getKiller(int ply) const {
        assert(ply >= 0 && ply < MAX_PLY);
        static_assert(K == 0 || K == 1);
        return killerMoves[ply][K];
    }

    inline Move getCounter(const Position& pos) const {
        Move prevMove = pos.previousMove();
        if (!isValidMove(prevMove)) return MOVE_NONE;

        return counterMoves[pos.getPieceAt(moveTo(prevMove))][moveTo(prevMove)];
    }

    template<Side Me>
    inline MoveScore getHistory(Move m) const {
        return history[Me][moveFromTo(m)];
    }

    template<Side Me>
    inline void update(const Position& pos, Move bestMove, int ply, int depth, const PartialMoveList& quietMoves) {
        if (!pos.isTactical(bestMove)) {
            updateKiller(bestMove, ply);
            updateCounter(pos, bestMove);

            MoveScore bonus = historyBonus(depth);
            updateHistoryEntry(history[Me][moveFromTo(bestMove)], bonus);

            for (auto m : quietMoves) {
                updateHistoryEntry(history[Me][moveFromTo(m)], -bonus);
            }
        }
    }
private:
    Move counterMoves[NB_PIECE][NB_SQUARE];
    Move killerMoves[MAX_PLY+1][2];
    MoveScore history[NB_SIDE][NB_SQUARE*NB_SQUARE];

    inline MoveScore historyBonus(int depth) {
        return std::min(1536, 8*depth*depth);
    }

    inline void updateKiller(Move move, int ply) {
        assert(ply >= 0 && ply < MAX_PLY);

        if (killerMoves[ply][0] != move) {
            killerMoves[ply][1] = killerMoves[ply][0];
            killerMoves[ply][0] = move;
        }
    }

    inline void updateCounter(const Position& pos, Move move) {
        Move prevMove = pos.previousMove();
        if (isValidMove(prevMove))
            counterMoves[pos.getPieceAt(moveTo(prevMove))][moveTo(prevMove)] = move;
    }

    inline void updateHistoryEntry(MoveScore &entry, MoveScore bonus) {
        entry += bonus - entry * std::abs(bonus) / 8192;
    }
};

} /* namespace bchess */

#include <cstdint>
#include <algorithm>

#include <cstdlib>
#include <cstdint>
#include <tuple>

namespace bchess {

constexpr size_t TT_DEFAULT_SIZE = 1024*1024*16;

constexpr int TT_ENTRIES_PER_BUCKET = 3;

enum Bound {
    BOUND_NONE = 0,
    BOUND_LOWER = 1,
    BOUND_UPPER = 2,
    BOUND_EXACT = BOUND_LOWER | BOUND_UPPER
};

class TTEntry {
public:
    static constexpr uint8_t AGE_MASK   = 0b11111000;
    static constexpr uint8_t PV_MASK    = 0b00000100;
    static constexpr uint8_t BOUND_MASK = 0b00000011;
    static constexpr int AGE_DELTA = 0x8;
    static constexpr int AGE_CYCLE = 0xFF + AGE_DELTA;

    inline bool empty() const { return hash16 == 0; }
    inline bool hashEquals(uint64_t hash) const { return hash16 == (uint16_t)hash; }
    inline Move move() const { return (Move)move16; }
    inline Score eval() const { return eval16; }
    inline Score score(int ply) const {
        return score16 == SCORE_NONE ? SCORE_NONE
             : score16 >=  SCORE_MATE_MAX_PLY ? score16 - ply
             : score16 <= -SCORE_MATE_MAX_PLY ? score16 + ply 
             : score16;
    }
    inline void score(Score s, int ply) {
        score16 = (int16_t) (s ==  SCORE_NONE   ? SCORE_NONE
                           : s >=  SCORE_MATE_MAX_PLY ? s + ply
                           : s <= -SCORE_MATE_MAX_PLY ? s - ply : s);
    }
    inline int depth() const { return depth8; }
    inline uint8_t age() const { return ageFlags8 & AGE_MASK; }
    inline bool isPv() const { return bool(ageFlags8 & PV_MASK); }
    inline Bound bound() const { return Bound(ageFlags8 & BOUND_MASK); }
    inline bool isExactBound() const { return bound() & BOUND_EXACT; }
    inline bool isLowerBound() const { return bound() & BOUND_LOWER; }
    inline bool isUpperBound() const { return bound() & BOUND_UPPER; }
    inline bool canCutoff(Score score, Score beta) { return score != SCORE_NONE && (bound() & (score >= beta ? BOUND_LOWER : BOUND_UPPER)); }

    inline void refresh(uint8_t age) { ageFlags8 = age | (ageFlags8 & (PV_MASK | BOUND_MASK)); }

    inline bool isBetterToKeep(const TTEntry &other, uint8_t age) const {
        return this->depth8 - ((AGE_CYCLE + age - this->ageFlags8) & AGE_MASK)
             > other.depth8 - ((AGE_CYCLE + age - other.ageFlags8) & AGE_MASK);
    }
private:
    friend class TranspositionTable;

    uint16_t hash16;
    Move move16;
    int16_t eval16;
    int16_t score16;
    uint8_t depth8;
    uint8_t ageFlags8;
}; // 10 Bytes

class TranspositionTable {
public:
    using TTResult = std::tuple<bool, TTEntry *>;

    TranspositionTable(size_t defaultSize = TT_DEFAULT_SIZE);
    ~TranspositionTable();

    void resize(size_t size);
    void clear();
    void newSearch();

    TTResult get(uint64_t hash);
    void set(TTEntry *tte, uint64_t hash, int depth, int ply, Bound bound, Move move, Score eval, Score score, bool pv);

    inline void prefetch(uint64_t hash) const { __builtin_prefetch(&buckets[index(hash)]); }

    size_t usage() const;
    inline size_t size() const { return nbBuckets; }

private:
    struct TTBucket {
        TTEntry entries[TT_ENTRIES_PER_BUCKET];
        uint16_t padding;

        inline TTEntry *begin() { return &entries[0]; }
        inline TTEntry *end() { return &entries[TT_ENTRIES_PER_BUCKET]; }
    }; // 32 Bytes

    TTBucket *buckets;
    size_t nbBuckets;
    uint8_t age;

    inline uint64_t index(uint64_t hash) const { return ((unsigned __int128)hash * (unsigned __int128)nbBuckets) >> 64; }
};

extern TranspositionTable tt;

} /* namespace bchess */

namespace bchess {

constexpr MoveScore PieceThreatenedValue[NB_PIECE_TYPE] = {
    0, 0, 15000, 15000, 25000, 50000, 0
};

struct ScoredMove {
    ScoredMove() { };

    inline ScoredMove(Move move_, MoveScore score_) 
    : score(score_), move(move_) { }

    MoveScore score;
    Move move;
};

using ScoredMoveList = fixed_vector<ScoredMove, MAX_MOVE, uint8_t>;

enum MovePickerType {
    MAIN,
    QUIESCENCE
};

class MovePicker {
public:
    MovePicker(): pos(nullptr), moveHistory(nullptr) { }
    MovePicker(const Position &pos_, Move ttMove_ = MOVE_NONE)
    : pos(&pos_),  moveHistory(nullptr), ttMove(ttMove_), refutations{}
    { }

    MovePicker(const Position &pos_, Move ttMove_, const MoveHistory* moveHistory_, int ply_)
    : pos(&pos_), moveHistory(moveHistory_), ttMove(ttMove_),
      refutations{moveHistory->getKiller<0>(ply_), moveHistory->getKiller<1>(ply_), moveHistory->getCounter(pos_)}
    {
        assert(refutations[0] != refutations[1] || refutations[0] == MOVE_NONE);
    }

    template<MovePickerType Type, Side Me, typename Handler>
    inline bool enumerate(const Handler &handler);

    template<MovePickerType Type, typename Handler>
    inline bool enumerate(const Handler &handler) { return pos->getSideToMove() == WHITE ? enumerate<Type, WHITE, Handler>(handler) : enumerate<Type, BLACK, Handler>(handler); }

private:
    const Position* const pos;
    const MoveHistory* const moveHistory;
    Move ttMove;
    Move refutations[3];

    template<Side Me> inline MoveScore scoreEvasion(Move m);
    template<Side Me> inline MoveScore scoreTactical(Move m);
    template<Side Me> inline MoveScore scoreQuiet(Move m);
};

template<MovePickerType Type, Side Me, typename Handler>
bool MovePicker::enumerate(const Handler &handler) {
    assert(pos != nullptr);
    assert(pos->getSideToMove() == Me);
    
    bool skipQuiets = false;

    tt.prefetch(pos->getHashAfter(ttMove));

    // TT Move
    if (pos->isLegal<Me>(ttMove)) {
        CALL_HANDLER(ttMove, skipQuiets);
    }
    
    ScoredMoveList moves;
    ScoredMove *current, *endBadTacticals, *beginQuiets, *endBadQuiets;

    auto compare = [](const ScoredMove& a, const ScoredMove& b) { return a.score > b.score; };

    // Evasions
    if (pos->inCheck()) {
        enumerateLegalMoves<Me, ALL_MOVES>(*pos, [&](Move m) {
            if (m == ttMove) return true; // continue;

            tt.prefetch(pos->getHashAfter(m));

            ScoredMove newMove = ScoredMove(m, scoreEvasion<Me>(m));
            moves.insert_sorted(newMove, compare);
            
            return true;
        });

        for (auto m : moves) {
            CALL_HANDLER(m.move, skipQuiets);
        }

        return true;
    }

    // Tacticals
    enumerateLegalMoves<Me, TACTICAL_MOVES>(*pos, [&](Move m) {
        if (m == ttMove) return true; // continue;
        
        if (moves.size() < 16)
            tt.prefetch(pos->getHashAfter(m));

        ScoredMove newMove = ScoredMove(m, scoreTactical<Me>(m));
        moves.insert_sorted(newMove, compare);

        return true;
    });

    for (current = endBadTacticals = moves.begin(); current != moves.end(); current++) {
        if constexpr(Type == MAIN) { // For quiescence prunning of bad captures is done in search
            if (!pos->see(current->move, -50)) { // Allow Bishop takes Knight
                *endBadTacticals++ = *current;
                continue;
            }
        }

        CALL_HANDLER(current->move, skipQuiets);
    }

    // Stop here for Quiescence
    if constexpr(Type == QUIESCENCE) return true;

    if (moveHistory != nullptr) [[likely]] {
        tt.prefetch(pos->getHashAfter(refutations[0]));
        tt.prefetch(pos->getHashAfter(refutations[1]));
        tt.prefetch(pos->getHashAfter(refutations[2]));
        
        // Killer 1
        if (refutations[0] != ttMove && !pos->isTactical(refutations[0]) && pos->isLegal<Me>(refutations[0])) {
            CALL_HANDLER(refutations[0], skipQuiets);
        }

        // Killer 2
        if (refutations[1] != ttMove && !pos->isTactical(refutations[1]) && pos->isLegal<Me>(refutations[1])) {
            CALL_HANDLER(refutations[1], skipQuiets);
        }

        // Counter
        if (refutations[2] != ttMove && !pos->isTactical(refutations[2]) && refutations[2] != refutations[0] && refutations[2] != refutations[1] && pos->isLegal<Me>(refutations[2])) {
            CALL_HANDLER(refutations[2], skipQuiets);
        }
    }

    // Quiets
    moves.resize(endBadTacticals - moves.begin()); // Keep only bad tacticals
    beginQuiets = endBadTacticals;

    enumerateLegalMoves<Me, QUIET_MOVES>(*pos, [&](Move m) {
        if (m == ttMove) return true; // continue;
        if (refutations[0] == m || refutations[1] == m || refutations[2] == m) return true; // continue

        if (moves.size() < 48)
            tt.prefetch(pos->getHashAfter(m));

        ScoredMove newMove = ScoredMove(m, scoreQuiet<Me>(m));
        moves.insert_sorted(newMove, compare);
        
        return true;
    });

    // Good quiets
    for (current = endBadQuiets = beginQuiets; current != moves.end() && !skipQuiets; current++) {
        if (current->score < -4000) {
            *endBadQuiets++ = *current;
            continue;
        }

        CALL_HANDLER(current->move, skipQuiets);
    }

    // Bad tacticals
    for (current = moves.begin(); current != endBadTacticals; current++) {
        tt.prefetch(pos->getHashAfter(current->move));
        CALL_HANDLER(current->move, skipQuiets);
    }

    // Bad quiets
    for (current = beginQuiets; current != endBadQuiets && !skipQuiets; current++) {
        tt.prefetch(pos->getHashAfter(current->move));
        CALL_HANDLER(current->move, skipQuiets);
    }

    return true;
}

template<Side Me>
MoveScore MovePicker::scoreEvasion(Move m) {
    if (pos->isTactical(m)) {
        return scoreTactical<Me>(m) + 1000000;
    } else {
        if (moveHistory != nullptr) [[likely]]
            return moveHistory->getHistory<Me>(m);
    }

    return 0;
}

template<Side Me>
MoveScore MovePicker::scoreTactical(Move m) {
    return PieceValue<MG>(pos->getPieceAt(moveTo(m))) - (int)pieceType(pos->getPieceAt(moveFrom(m))); // MVV-LVA
}

template<Side Me>
MoveScore MovePicker::scoreQuiet(Move m) {
    assert(movePromotionType(m) != QUEEN);

    Square from = moveFrom(m), to = moveTo(m);
    PieceType pt = pieceType(pos->getPieceAt(from));
    MoveScore score = NB_PIECE_TYPE-(int)pt;

    if (moveType(m) == PROMOTION) [[unlikely]]
        return -10000;

    Bitboard threatened = pos->threatsFor(pt);
    score += ((threatened & from) && !(threatened & to)) * PieceThreatenedValue[pt];

    if (moveHistory != nullptr) [[likely]]
        score += moveHistory->getHistory<Me>(m);

    // TODO: refactor this!
    switch (pt) {
        case PAWN:
            if (pawnAttacks<Me>(bb(to)) & pos->getPiecesBB(~Me, KING)) {
                score += 10000;
            }
            break;
        case KNIGHT:
            if (attacks<KNIGHT>(to, pos->getPiecesBB()) & pos->getPiecesBB(~Me, KING)) {
                score += 10000;
            }
            break;
        case BISHOP:
            if (attacks<BISHOP>(to, pos->getPiecesBB()) & pos->getPiecesBB(~Me, KING)) {
                score += 10000;
            }
        case ROOK:
            if (attacks<ROOK>(to, pos->getPiecesBB()) & pos->getPiecesBB(~Me, KING)) {
                score += 10000;
            }
        case QUEEN:
            if (attacks<QUEEN>(to, pos->getPiecesBB()) & pos->getPiecesBB(~Me, KING)) {
                score += 10000;
            }
        default:
            break;
    }

    return score;
}

} /* namespace bchess */

namespace bchess {

struct SearchLimits {
    TimeMs timeLeft[NB_SIDE] = {0};
    TimeMs increment[NB_SIDE] = {0};
    int movesToGo = 0;
    int maxDepth = 0;
    size_t maxNodes = 0;
    TimeMs maxTime = 0;
    MoveList searchMoves;
};

struct Node {
    Score staticEval;
    MoveList pv;
};

struct SearchEvent {
    SearchEvent(int depth_, const MoveList &pv_, Score bestScore_, size_t nbNode_, TimeMs elapsed_, size_t hashfull_): 
        depth(depth_), pv(pv_), bestScore(bestScore_), nbNodes(nbNode_), elapsed(elapsed_), hashfull(hashfull_) { }

    int depth;
    const MoveList &pv;
    Score bestScore;
    size_t nbNodes;
    TimeMs elapsed;
    size_t hashfull;
};

enum class NodeType {
    Root,
    PV,
    NonPV
};

class Engine {
public:
    static void init();
    
    Engine() = default;
    virtual ~Engine() = default;

    inline Position &position() { return rootPosition; }
    inline const Position &position() const { return rootPosition; }

    void search(const SearchLimits &limits);
    void stop();
    void waitForSearchFinish();
    inline bool isSearching() { return searching.test(std::memory_order_relaxed); }
    inline bool searchAborted() { return aborted.test(std::memory_order_relaxed); }
    inline void setHashSize(size_t size) { tt.resize(size); }
    inline void newGame() { tt.clear(); }

protected:
    virtual void onSearchProgress(const SearchEvent &event) = 0;
    virtual void onSearchFinish(const SearchEvent &event) = 0;

private:
    static int LMRTable[MAX_PLY][MAX_MOVE];

    Position rootPosition;

    inline void idSearch() { rootPosition.getSideToMove() == WHITE ? idSearch<WHITE>() : idSearch<BLACK>(); }
    template<Side Me> void idSearch();

    template<Side Me, NodeType NT> Score negaMax(Score alpha, Score beta, int depth, int ply, bool cutNode);

    template<Side Me, NodeType NT> Score qSearch(Score alpha, Score beta, int depth, int ply);

    
    std::atomic_flag aborted = ATOMIC_FLAG_INIT;
    std::atomic_flag searching = ATOMIC_FLAG_INIT;

    size_t nbNodes;
    SearchLimits limits;

    TimeMs startTime;
    TimeMs lastCheck;
    TimeMs softTimeLimit;
    TimeMs hardTimeLimit;

    MoveHistory moveHistory;

    Node nodes[MAX_PLY+1];

    
    void initAllocatedTime(){
        int64_t moves = limits.movesToGo > 0 ? limits.movesToGo + 5 : 30;
        Side stm = rootPosition.getSideToMove();

        hardTimeLimit = 0.49 * limits.timeLeft[stm];
        softTimeLimit = std::min<TimeMs>(hardTimeLimit, limits.timeLeft[stm] / moves + 0.9 * limits.increment[stm]);
    }

    inline TimeMs getElapsed() { return now() - startTime; }
    
    inline bool useTournamentTime() { return !!(limits.timeLeft[WHITE] | limits.timeLeft[WHITE]); }
    inline bool useFixedTime() { return limits.maxTime > 0; }
    inline bool useTimeLimit() { return useTournamentTime() || useTimeLimit(); }
    inline bool useNodeCountLimit() { return limits.maxNodes > 0; }

    inline bool shouldStop() { 
        TimeMs elapsed = now() - startTime;

        if (useTournamentTime() && elapsed >= hardTimeLimit)
            return true;
        if (useFixedTime() && (elapsed > limits.maxTime))
            return true;
        if (useNodeCountLimit() && nbNodes >= limits.maxNodes)
            return true;
        
        return false;
    }

    inline bool shouldStopSoft() {
        TimeMs elapsed = now() - startTime;
        
        if (useTournamentTime() && elapsed >= softTimeLimit)
            return true;

        return false;
    }

    inline Node& get_node(int ply) { assert(ply >= 0 && ply < MAX_PLY); return nodes[ply]; }
};

} /* namespace bchess */

#define VERSION "0.0.1"

namespace bchess {

typedef std::ostream& (*Manipulator) (std::ostream&);

// Used to log stdin & stdout to a file for debugging
class Console {
public:
    Console() = default;
    Console(const Console &) = delete;
    ~Console();
    Console &operator=(const Console &) = delete;

    void setLogFile(const std::string &filename);
    std::istream& getline(std::string& str);

    template <class T> friend Console& operator<<(Console& console, const T& x);
    friend Console& operator<<(Console& console, Manipulator manip);
private:
    template <class T> Console &log(const T& x, bool isInput = false);

    std::stringstream buffer;
    std::ofstream *file;
};

extern Console console;

inline Console& operator<<(Console& console, Manipulator x) {
    std::cout << x;
    console.log(x);
    return console;
}

template <class T>
inline Console& operator<<(Console& console, const T& x) { 
    std::cout << x;
    console.log(x);
    return console;
}

template <class T> Console& Console::log(const T& x, bool isInput) {
    if (file != nullptr) {
        if (buffer.rdbuf()->in_avail() == 0) {
            auto now = time(nullptr);
            buffer << "[" << std::put_time(std::localtime(&now), "%F %T") << "] " << (isInput ? "<< " : ">> ");
        }
        buffer << x;

        if (buffer.str().ends_with('\n')) {
            (*file) << buffer.str();
            file->flush();
            buffer.str(std::string()); // clear buffer
        }
        
    }

    return *this;
}

class UciEngine : public Engine {
protected:
    virtual void onSearchProgress(const SearchEvent &event);
    virtual void onSearchFinish(const SearchEvent &event);
};

class Uci {
public:
    Uci();
    ~Uci() = default;
    void loop(int argc, char* argv[]);

    Move parseMove(std::string str) const;

    static Square parseSquare(std::string str);
    static std::string formatSquare(Square sq);
    static std::string formatMove(Move m);
    static std::string formatScore(Score s);
    
private:
    typedef bool (Uci::*UciCommandHandler)(std::istringstream& is);
    struct CaseInsensitiveComparator {
        bool operator() (const std::string&s1, const std::string&s2) const {
            return std::lexicographical_compare(s1.begin(), s1.end(), s2.begin(), s2.end(), [](char c1, char c2) { 
                return tolower(c1) < tolower(c2);
            });
        }
    };

    std::map<std::string, UciOption, CaseInsensitiveComparator> options;
    std::map<std::string, UciCommandHandler> commands;
    UciEngine engine;

    bool cmdUci(std::istringstream& is);
    bool cmdIsReady(std::istringstream& is);
    bool cmdUciNewGame(std::istringstream& is);
    bool cmdSetOption(std::istringstream& is);
    bool cmdPosition(std::istringstream& is);
    bool cmdGo(std::istringstream& is);
    bool cmdStop(std::istringstream& is);
    bool cmdQuit(std::istringstream& is);

    bool cmdDebug(std::istringstream& is);
    bool cmdEval(std::istringstream& is);
    bool cmdPerft(std::istringstream& is);
    bool cmdPerftmp(std::istringstream& is);
    bool cmdTest(std::istringstream& is);
    bool cmdBench(std::istringstream& is);
};

} /* namespace bchess */

namespace bchess::Test {

void run();

} /* namespace bchess::Test */

namespace bchess {

template<bool Div> size_t perft(Position &pos, int depth);
void perft(Position &pos, int depth);

} /* namespace bchess */

using namespace bchess;

int main(int argc, char* argv[])
{
    Engine::init();
    BB::init();
    Zobrist::init();

    Uci uci;
    uci.loop(argc, argv);

    return 0;
}

namespace bchess {

UciOption::UciOption(OnUpdate onUpdate_) : type("button"), onUpdate(onUpdate_) { }

UciOption::UciOption(bool defaultValue_, OnUpdate onUpdate_) : type("check"), onUpdate(onUpdate_)
{ 
    defaultValue = value = (defaultValue_ ? "true" : "false");
}

UciOption::UciOption(const std::string &defaultValue_, OnUpdate onUpdate_) : type("string"), defaultValue(defaultValue_), value(defaultValue_), onUpdate(onUpdate_) { }
UciOption::UciOption(const char *defaultValue_, OnUpdate onUpdate_) : type("string"), defaultValue(defaultValue_), value(defaultValue_), onUpdate(onUpdate_) { }

UciOption::UciOption(int defaultValue_, int min_, int max_, OnUpdate onUpdate_) : type("spin"), min(min_), max(max_), onUpdate(onUpdate_)
{ 
    defaultValue = value = std::to_string(defaultValue_);
}

UciOption::UciOption(const std::string &defaultValue_, const OptionValues &allowedValues_, OnUpdate onUpdate_) : defaultValue(defaultValue_), value(defaultValue_), allowedValues(allowedValues_), onUpdate(onUpdate_)
{
    assert(allowedValues.count(value) == 1);
}

std::ostream &operator<<(std::ostream &s, const UciOption &option) {
    if (option.isButton()) {
        s << "type button";
    } else if (option.isCheck()) {
        s << "type check default " << "false";
    } else if (option.isString()) {
        s << "type string default " << option.defaultValue;
    } else if (option.isSpin()) {
        s << "type spin default " << option.defaultValue << " min " << option.min << " max " << option.max;
    } else if (option.isCombo()) {   
        s << "type combo default " << option.defaultValue;
        for(auto const &v : option.allowedValues) {
            s << " var " << v;
        }
    }

    return s;
}

UciOption& UciOption::operator=(const std::string& newValue)
{
    if (!isButton() && newValue.empty()) return *this;
    if (isCheck() && newValue != "true" && newValue != "false") return *this;
    if (isSpin() && parseInt(newValue) < min && parseInt(newValue) > max) return *this;
    if (isCombo() && this->allowedValues.count(newValue) != 1) return *this;

    if (!isButton())
        value = newValue;

    if (onUpdate != nullptr)
        onUpdate(*this);

    return *this;
}

} /* namespace bchess */

#include <iostream>

namespace bchess {

Bitboard PAWN_ATTACK[NB_SIDE][NB_SQUARE];
Bitboard KNIGHT_MOVE[NB_SQUARE];
Bitboard KING_MOVE[NB_SQUARE];
PextEntry ROOK_MOVE[NB_SQUARE];
PextEntry BISHOP_MOVE[NB_SQUARE];

Bitboard BETWEEN_BB[NB_SQUARE][NB_SQUARE];

Bitboard ROOK_DATA[0x19000];
Bitboard BISHOP_DATA[0x1480];

namespace BB {

template<Direction Dir>
inline Bitboard slidingRay(Square sq, Bitboard occupied) 
{
    Bitboard attacks = 0;
    Bitboard b = (1ULL << sq);

    do {
        attacks |= (b = shift<Dir>(b));
    } while(b & ~occupied);

    return attacks;
}

template<PieceType Pt>
Bitboard slidingAttacks(Square sq, Bitboard occupied) 
{
    if constexpr (Pt == ROOK)
        return slidingRay<UP>(sq, occupied) 
            | slidingRay<DOWN>(sq, occupied) 
            | slidingRay<RIGHT>(sq, occupied) 
            | slidingRay<LEFT>(sq, occupied);
    else
        return slidingRay<UP_RIGHT>(sq, occupied) 
            | slidingRay<UP_LEFT>(sq, occupied) 
            | slidingRay<DOWN_RIGHT>(sq, occupied) 
            | slidingRay<DOWN_LEFT>(sq, occupied);
}

template<PieceType Pt>
void init_pext(Bitboard table[], PextEntry magics[]) {
    int size = 0;

    for (int i=0; i<NB_SQUARE; i++) {
        Square s = Square(i);

        Bitboard edges = ((Rank1BB | Rank8BB) & ~bb(rankOf(s))) | ((FileABB | FileHBB) & ~bb(fileOf(s)));

        PextEntry& m = magics[s];
        m.mask  = slidingAttacks<Pt>(s, 0) & ~edges;
        m.data = s == SQ_A1 ? table : magics[s - 1].data + size;

        size = 0;
        Bitboard b = 0;
        do {
            m.data[pext(b, m.mask)] = slidingAttacks<Pt>(s, b);

            size++;
            b = (b - m.mask) & m.mask;
        } while (b);
    }
}

void debug(Bitboard bb)
{
	std::cout << "Bitboard:" << std::endl;
	int sq = 0;
	for (; sq != 64; ++sq) {
		if (sq && !(sq & 7))
			std::cout << std::endl;

		if ((1ULL << (sq ^ 56)) & bb)
			std::cout << "X  ";
		else
			std::cout << "-  ";
	}
	std::cout << std::endl;
}

void init()
{
    for (Square s=SQ_A1; s<NB_SQUARE; ++s) {
        Bitboard b = bb(s);

        PAWN_ATTACK[WHITE][s] = shift<UP_LEFT>(b) | shift<UP_RIGHT>(b);
        PAWN_ATTACK[BLACK][s] = shift<DOWN_RIGHT>(b) | shift<DOWN_LEFT>(b);

        KING_MOVE[s] = shift<UP>(b) | shift<DOWN>(b) | shift<RIGHT>(b) | shift<LEFT>(b)
                     | shift<UP_RIGHT>(b) | shift<UP_LEFT>(b) | shift<DOWN_RIGHT>(b) | shift<DOWN_LEFT>(b);

        KNIGHT_MOVE[s] = shift<UP_LEFT>( shift<UP>(b) ) | shift<UP_RIGHT>( shift<UP>(b) )
                       | shift<UP_RIGHT>( shift<RIGHT>(b) ) | shift<DOWN_RIGHT>( shift<RIGHT>(b) )
                       | shift<DOWN_RIGHT>( shift<DOWN>(b) ) | shift<DOWN_LEFT>( shift<DOWN>(b) )
                       | shift<DOWN_LEFT>( shift<LEFT>(b) ) | shift<UP_LEFT>( shift<LEFT>(b) )
                       ;

        for(Square s2=SQ_A1; s2<NB_SQUARE; ++s2) {
            Bitboard b2 = bb(s2);

            if (slidingAttacks<ROOK>(s, 0) & s2) {
                BETWEEN_BB[s][s2] |= slidingAttacks<ROOK>(s, b2) & slidingAttacks<ROOK>(s2, b);
            } else if (slidingAttacks<BISHOP>(s, 0) & s2) {
                BETWEEN_BB[s][s2] = slidingAttacks<BISHOP>(s, b2) & slidingAttacks<BISHOP>(s2, b);
            }
        }
    }

    init_pext<ROOK>(ROOK_DATA, ROOK_MOVE);
    init_pext<BISHOP>(BISHOP_DATA, BISHOP_MOVE);
}

} // namespace Bitboard

} /* namespace bchess */
#include <sstream>
#include <cstring>

namespace bchess {

const std::string PIECE_TO_CHAR(" PNBRQK  pnbrqk");

char pieceToChar(Piece p) {
    assert(p>= NO_PIECE && p<= NB_PIECE);
    return PIECE_TO_CHAR[p];
}
Piece charToPiece(char c) {
    return Piece(PIECE_TO_CHAR.find(c));
}

Position::Position() {
    setFromFEN(STARTPOS_FEN);
}

Position::Position(const Position &other) {
    std::memcpy((void*)this, &other, sizeof(Position));
    this->state = this->history + (other.state - other.history);
}

Position& Position::operator=(const Position &other) {
    std::memcpy((void*)this, &other, sizeof(Position));
    this->state = this->history + (other.state - other.history);

    return *this;
}

void Position::reset() {
    state = &history[0];

    state->fiftyMoveRule = 0;
    state->halfMoves = 0;
    state->epSquare = SQ_NONE;
    state->castlingRights = NO_CASTLING;
    state->move = MOVE_NONE;
    for(int i=0; i<NB_PIECE_TYPE; i++) state->threatsFor[i] = EmptyBB;

    for(int i=0; i<NB_SQUARE; i++) pieces[i] = NO_PIECE;
    for(int i=0; i<NB_PIECE; i++) piecesBB[i] = EmptyBB;
    //for(int i=0; i<NB_PIECE_TYPE; i++) typeBB[i] = EmptyBB;
    //typeBB[ALL_PIECES] = EmptyBB;
    sideBB[WHITE] = sideBB[BLACK] = EmptyBB;
    sideToMove = WHITE;
}

void Position::setCastlingRights(CastlingRight cr) {
    Side s = (cr & WHITE_CASTLING) ? WHITE : BLACK;

    if (getKingSquare(s) == relativeSquare(s, SQ_E1) && getPieceAt(CastlingRookFrom[cr]) == piece(s, ROOK)) {
        state->castlingRights |= cr;
    }
}

std::string Position::fen() const {
    std::ostringstream ss;

    for (int r = RANK_8; r >= RANK_1; --r) {
        for (int f = FILE_A; f <= FILE_H; ++f) {
            int nbEmpty;

            for (nbEmpty = 0; f <= FILE_H && isEmpty(square(File(f), Rank(r))); ++f)
                ++nbEmpty;

            if (nbEmpty)
                ss << nbEmpty;

            if (f <= FILE_H)
                ss << PIECE_TO_CHAR[getPieceAt(square(File(f), Rank(r)))];
        }

        if (r > RANK_1)
            ss << '/';
    }

    ss << (sideToMove == WHITE ? " w " : " b ");

    if (canCastle(WHITE_KING_SIDE)) ss << 'K';
    if (canCastle(WHITE_QUEEN_SIDE)) ss << 'Q';
    if (canCastle(BLACK_KING_SIDE)) ss << 'k';
    if (canCastle(BLACK_KING_SIDE)) ss << 'q';
    if (!canCastle(ANY_CASTLING)) ss << '-';

    ss << (getEpSquare() == SQ_NONE ? " - " : " " + Uci::formatSquare(getEpSquare()) + " ");
    ss << getFiftyMoveRule() << " " << getFullMoves();

    return ss.str();
}

bool Position::setFromFEN(const std::string &fen) {
    reset();

    std::istringstream parser(fen);
    std::string token;

    // Pieces
    parser >> std::skipws >> token;
    int f=0, r=7;
    for(auto c : token) {
        if (c == '/') {
            r--; f=0;
        } else if (c >= '0' && c <= '9') {
            f += c-'0';
        } else {
            Piece p = charToPiece(c);
            Square sq = square(File(f), Rank(r));

            if (!isValidPiece(p) || !isValidSq(sq)) {
                reset();
                return false;
            }

            side(p) == WHITE ? setPiece<WHITE>(sq, p) : setPiece<BLACK>(sq, p);
            f++;
        }
    }

    // Side to Move
    parser >> std::skipws >> token;
    if (token[0] != 'w' && token[0] != 'b') {
        reset();
        return false;
    }
    sideToMove = ((token[0] == 'w') ? WHITE : BLACK);

    // Casteling
    parser >> std::skipws >> token;
    for(auto c : token) {
        if (c == 'K') setCastlingRights(WHITE_KING_SIDE);
        else if (c == 'Q') setCastlingRights(WHITE_QUEEN_SIDE);
        else if (c == 'k') setCastlingRights(BLACK_KING_SIDE);
        else if (c == 'q') setCastlingRights(BLACK_QUEEN_SIDE);
        else if (c == '-') continue;
        else {
            reset();
            return false;
        }
    }

    // En passant
    parser >> std::skipws >> token;
    state->epSquare = Uci::parseSquare(token);
    if (state->epSquare != SQ_NONE && !isValidSq(state->epSquare)) {
        reset();
        return false;
    }
    if (state->epSquare != SQ_NONE && !(
        pawnAttacks(~sideToMove, state->epSquare) && getPiecesBB(sideToMove, PAWN)
        && (getPiecesBB(~sideToMove, PAWN) & (state->epSquare + pawnDirection(~sideToMove)))
        && !(getPiecesBB() & (state->epSquare | (state->epSquare + pawnDirection(sideToMove))))
    )) {
        state->epSquare = SQ_NONE;
    }

    // Rule 50 half move
    parser >> std::skipws >> state->fiftyMoveRule;
    if (state->fiftyMoveRule < 0) {
        reset();
        return false;
    }

    // Full move
    parser >> std::skipws >> state->halfMoves;
    state->halfMoves = std::max(2 * (state->halfMoves - 1), 0) + (sideToMove == BLACK);
    if (state->halfMoves < 0) {
        reset();
        return false;
    }

    updateBitboards();
    this->state->hash = computeHash();

    return true;
}

std::ostream& operator<<(std::ostream& os, const Position& pos) {
    os << std::endl << " +---+---+---+---+---+---+---+---+" << std::endl;

    for (int r = RANK_8; r >= RANK_1; --r) {
        for (int f = FILE_A; f <= FILE_H; ++f)
            os << " | " << pieceToChar(pos.getPieceAt(square(File(f), Rank(r))));

        os << " | " << (1 + r) << std::endl << " +---+---+---+---+---+---+---+---+" << std::endl;
    }

    os << "   a   b   c   d   e   f   g   h" << std::endl << std::endl;
    os << "Side to move: " << (pos.getSideToMove() == WHITE ? "White" : "Black") << std::endl;
    os << "FEN: " << pos.fen() << std::endl;
    os << "Hash: " << pos.computeHash() << std::endl;
    os << "Checkers:";
    Bitboard checkers = pos.checkers(); 
    bitscan_loop(checkers) {
        Square sq = bitscan(checkers);
        os << " " << Uci::formatSquare(sq);
    }
    os << std::endl;

    if (pos.isFiftyMoveDraw())
        os << "[Draw by fifty move rule]" << std::endl;
    if (pos.isRepetitionDraw())
        os << "[Draw by 3-fold repetition]" << std::endl;

    return os;
}

std::string Position::debugHistory() {
    std::stringstream ss;

    assert(this->state - this->history < MAX_HISTORY && this->state - this->history >= 0);

    for(State *s = this->state; s > this->history; s--) {
        ss << Uci::formatMove(s->move) << " ";
    }

    return ss.str();
}

/*
WIP:
inline bool Position::givesCheck(Move m) {
    Side Me = getSideToMove();
    Side Opp = ~Me;
    Square ksq = getKingSquare(Opp);

    Bitboard bb[NB_PIECE_TYPE] = {
        getPiecesBB(),
        getPiecesBB(Me, PAWN),
        getPiecesBB(Me, KNIGHT),
        getPiecesBB(Me, BISHOP),
        getPiecesBB(Me, ROOK),
        getPiecesBB(Me, QUEEN)
    };

    switch(moveType(m)) {
        case NORMAL:
        case CASTLING:
        case PROMOTION:
        case EN_PASSANT:
        break;
    }

    return ((pawnAttacks(Me, ksq) & bb[PAWN])
        || (attacks<KNIGHT>(ksq) & bb[KNIGHT])
        || (attacks<BISHOP>(ksq, bb[ALL_PIECES]) & (bb[BISHOP] | bb[QUEEN]))
        || (attacks<ROOK>(ksq, bb[ALL_PIECES]) & (bb[ROOK] | bb[QUEEN]))
    );
}


*/

template<Side Me>
bool Position::isLegal(Move move) const {
    if (!isValidMove(move)) return false;

    Square from = moveFrom(move);
    Square to = moveTo(move);
    if (from == to) return false;

    Piece pc = getPieceAt(from);
    if (pc == NO_PIECE || side(pc) != Me) return false;

    Piece capture = getPieceAt(to);
    if (pieceType(capture) == KING) return false;
    if (capture != NO_PIECE && side(capture) != ~Me) return false;

    if (checkers()) {
        return capture != NO_PIECE ? isLegal<Me, true, true>(move, pc) : isLegal<Me, true, false>(move, pc);
    }

    return capture != NO_PIECE ? isLegal<Me, false, true>(move, pc) : isLegal<Me, false, false>(move, pc);
}

template bool Position::isLegal<WHITE>(Move move) const;
template bool Position::isLegal<BLACK>(Move move) const;

template<Side Me, bool InCheck, bool IsCapture>
inline bool Position::isLegal(Move move, Piece pc) const {
    constexpr MoveGenType Type = IsCapture ? TACTICAL_MOVES : QUIET_MOVES;

    Square from = moveFrom(move);
    Bitboard src = bb(from);
    PieceType pt = pieceType(pc);

    switch(moveType(move)) {
        case NORMAL:
            if (nbCheckers() > 1 && pt != KING) return false; // Only king move when in double check

            switch(pt) {
                case PAWN:
                    return !enumeratePawnNormalMoves<Me, InCheck, Type>(*this, src, [move](Move m){ return move != m; });
                case KNIGHT:
                    return !enumerateKnightMoves<Me, InCheck, Type>(*this, src, [move](Move m){ return move != m; });
                case BISHOP:
                    return !enumerateBishopSliderMoves<Me, InCheck, Type>(*this, src, [move](Move m){ return move != m; });
                case ROOK:
                    return !enumerateRookSliderMoves<Me, InCheck, Type>(*this, src, [move](Move m){ return move != m; });
                case QUEEN:
                    return !enumerateQueenSliderMoves<Me, InCheck, Type>(*this, src, [move](Move m){ return move != m; });
                case KING:
                    return !enumerateKingMoves<Me, Type>(*this, from, [move](Move m){ return move != m; });
                default:
                    return false;
            }
        case CASTLING:
            if (nbCheckers() > 0) return false; // No castling when in check

            return !enumerateCastlingMoves<Me>(*this, [move](Move m) { return move != m; });
        case PROMOTION:
            if (nbCheckers() > 1) return false; // Only king move when in double check
            if (pt != PAWN) return false;

            return !enumeratePawnPromotionMoves<Me, InCheck, ALL_MOVES>(*this, src, [move](Move m) { return (move != m); });
        case EN_PASSANT:
            if (nbCheckers() > 1) return false; // Only king move when in double check
            if (getEpSquare() == SQ_NONE || getEpSquare() != moveTo(move)) return false;
            if (pt != PAWN) return false;

            return !enumeratePawnEnpassantMoves<Me, InCheck>(*this, src, [move](Move m){ return move != m; });
        default: break;
    }

    return false;
}

template<Side Me>
inline void Position::setPiece(Square sq, Piece p) {
    Bitboard b = bb(sq);
    pieces[sq] = p;
    //typeBB[ALL_PIECES] |= b;
    //typeBB[pieceType(p)] |= b;
    sideBB[Me] |= b;
    piecesBB[p] |= b;
}
template<Side Me>
inline void Position::unsetPiece(Square sq) {
    Bitboard b = bb(sq);
    Piece p = pieces[sq];
    pieces[sq] = NO_PIECE;
    //typeBB[ALL_PIECES] &= ~b;
    //typeBB[pieceType(p)] &= ~b;
    sideBB[Me] &= ~b;
    piecesBB[p] &= ~b;
}
template<Side Me>
inline void Position::movePiece(Square from, Square to) {
    Bitboard fromTo = from | to;
    Piece p = pieces[from];

    pieces[to] = p;
    pieces[from] = NO_PIECE;
    //typeBB[ALL_PIECES] ^= fromTo;
    //typeBB[pieceType(p)] ^= fromTo;
    sideBB[Me] ^= fromTo;
    piecesBB[p] ^= fromTo;
}

template<Side Me, MoveType Mt>
void Position::doMove(Move m) {
    assert(isValidMove(m));
    assert(getSideToMove() == Me);

    const Square from = moveFrom(m), to = moveTo(m);
    const Piece p = getPieceAt(from);
    const Piece capture = getPieceAt(to);

    assert(side(getPieceAt(from)) == Me);
    assert(capture == NO_PIECE || side(capture) == ~Me);
    assert(pieceType(capture) != KING);
    assert(to == getEpSquare() || Mt != EN_PASSANT);

    uint64_t h = state->hash;

    // Reset epSquare (branchless)
    h ^= Zobrist::enpassantKeys[fileOf(state->epSquare) + NB_FILE*(state->epSquare == SQ_NONE)];
    /*if (state->epSquare != SQ_NONE) {
        h ^= Zobrist::enpassantKeys[fileOf(state->epSquare)];
    }*/

    State *oldState = state++;
    state->epSquare = SQ_NONE;
    state->castlingRights = oldState->castlingRights;
    state->fiftyMoveRule = oldState->fiftyMoveRule + 1;
    state->halfMoves = oldState->halfMoves + 1;
    state->capture = capture;
    state->move = m;

    if constexpr (Mt == NORMAL) {
        // TODO: Try to remove branching using xor
        if (capture != NO_PIECE) {
            h ^= Zobrist::keys[capture][to];
            unsetPiece<~Me>(to);
            state->fiftyMoveRule = 0;
        }
        
        h ^= Zobrist::keys[p][from] ^ Zobrist::keys[p][to];
        movePiece<Me>(from, to);

        // Update castling right (no branching)
        h ^= Zobrist::castlingKeys[state->castlingRights];
        state->castlingRights &= ~(CastlingRightsMask[from] | CastlingRightsMask[to]);
        h ^= Zobrist::castlingKeys[state->castlingRights];

        if (p == piece(Me, PAWN)) {
            state->fiftyMoveRule = 0;

            // Set epSquare on double push
            if ((int(from) ^ int(to)) == int(UP+UP)) {
                const Square epsq = to - pawnDirection(Me);

                // Only if opponent pawn can do enpassant
                if (pawnAttacks(Me, epsq) & getPiecesBB(~Me, PAWN)) {
                    h ^= Zobrist::enpassantKeys[fileOf(epsq)];
                    state->epSquare = epsq;
                }
            }
        }
    } else if constexpr (Mt == CASTLING) {
        CastlingRight cr = Me & (to > from ? KING_SIDE : QUEEN_SIDE);
        const Square rookFrom = CastlingRookFrom[cr];
        const Square rookTo = CastlingRookTo[cr];

        assert(pieceType(getPieceAt(from)) == KING);
        assert(getPieceAt(rookFrom) == piece(Me, ROOK));
        assert(isEmpty(CastlingPath[cr]));

        h ^= Zobrist::keys[piece(Me, KING)][from] ^ Zobrist::keys[piece(Me, KING)][to];
        h ^= Zobrist::keys[piece(Me, ROOK)][rookFrom] ^ Zobrist::keys[piece(Me, ROOK)][rookTo];
        movePiece<Me>(from, to);
        movePiece<Me>(rookFrom, rookTo);

        // Update castling right
        h ^= Zobrist::castlingKeys[state->castlingRights];
        state->castlingRights &= ~(CastlingRightsMask[from] | CastlingRightsMask[to]);
        h ^= Zobrist::castlingKeys[state->castlingRights];
    } else if constexpr (Mt == PROMOTION) {
        const PieceType promotionType = movePromotionType(m);
        assert(pieceType(getPieceAt(from)) == PAWN);
        assert(promotionType != NO_PIECE_TYPE && promotionType != PAWN);

        // TODO: Try to remove branching using xor
        if (capture != NO_PIECE) {
            h ^= Zobrist::keys[capture][to];
            unsetPiece<~Me>(to);
        }

        h ^= Zobrist::keys[piece(Me, PAWN)][from] ^ Zobrist::keys[piece(Me, promotionType)][to];
        unsetPiece<Me>(from);
        setPiece<Me>(to, piece(Me, promotionType));
        state->fiftyMoveRule = 0;

        // Update castling right
        h ^= Zobrist::castlingKeys[state->castlingRights];
        state->castlingRights &= ~(CastlingRightsMask[from] | CastlingRightsMask[to]);
        h ^= Zobrist::castlingKeys[state->castlingRights];
    } else if constexpr (Mt == EN_PASSANT) {
        assert(pieceType(getPieceAt(from)) == PAWN);

        const Square epsq = to - pawnDirection(Me);

        h ^= Zobrist::keys[piece(~Me, PAWN)][epsq];
        h ^= Zobrist::keys[piece(Me, PAWN)][from] ^ Zobrist::keys[piece(Me, PAWN)][to];
        unsetPiece<~Me>(epsq);
        movePiece<Me>(from, to);

        state->fiftyMoveRule = 0;
    }

    sideToMove = ~Me;
    h ^= Zobrist::sideToMoveKey;

    state->hash = h;
    assert(computeHash() == hash());
    
    updateBitboards<~Me>();
}

template void Position::doMove<WHITE, NORMAL>(Move m);
template void Position::doMove<WHITE, PROMOTION>(Move m);
template void Position::doMove<WHITE, EN_PASSANT>(Move m);
template void Position::doMove<WHITE, CASTLING>(Move m);
template void Position::doMove<BLACK, NORMAL>(Move m);
template void Position::doMove<BLACK, PROMOTION>(Move m);
template void Position::doMove<BLACK, EN_PASSANT>(Move m);
template void Position::doMove<BLACK, CASTLING>(Move m);

template<Side Me, MoveType Mt>
void Position::undoMove(Move m) {
    assert(getSideToMove() == ~Me);

    const Square from = moveFrom(m);
    const Square to = moveTo(m);
    const Piece capture = state->capture;

    state--;
    sideToMove = Me;

    if constexpr (Mt == NORMAL) {
        movePiece<Me>(to, from);

        if (capture != NO_PIECE) {
            setPiece<~Me>(to, capture);
        }
    } else if constexpr (Mt == CASTLING) {
        const CastlingRight cr = Me & (to > from ? KING_SIDE : QUEEN_SIDE);
        const Square rookFrom = CastlingRookFrom[cr];
        const Square rookTo = CastlingRookTo[cr];

        movePiece<Me>(to, from);
        movePiece<Me>(rookTo, rookFrom);
    } else if constexpr (Mt == PROMOTION){
        unsetPiece<Me>(to);
        setPiece<Me>(from, piece(Me, PAWN));

        if (capture != NO_PIECE) {
            setPiece<~Me>(to, capture);
        }
    } else if constexpr (Mt == EN_PASSANT) {
        movePiece<Me>(to, from);

        const Square epsq = to - pawnDirection(Me);
        setPiece<~Me>(epsq, piece(~Me, PAWN));
    }
}

template void Position::undoMove<WHITE, NORMAL>(Move m);
template void Position::undoMove<WHITE, PROMOTION>(Move m);
template void Position::undoMove<WHITE, EN_PASSANT>(Move m);
template void Position::undoMove<WHITE, CASTLING>(Move m);
template void Position::undoMove<BLACK, NORMAL>(Move m);
template void Position::undoMove<BLACK, PROMOTION>(Move m);
template void Position::undoMove<BLACK, EN_PASSANT>(Move m);
template void Position::undoMove<BLACK, CASTLING>(Move m);

template<Side Me> void Position::doNullMove() {
    assert(!inCheck());
    assert(getSideToMove() == Me);
    
    uint64_t h = state->hash;

    // Reset epSquare (branchless)
    h ^= Zobrist::enpassantKeys[fileOf(state->epSquare) + NB_FILE*(state->epSquare == SQ_NONE)];

    State *oldState = state++;
    state->epSquare = SQ_NONE;
    state->castlingRights = oldState->castlingRights;
    state->fiftyMoveRule = oldState->fiftyMoveRule + 1;
    state->halfMoves = oldState->halfMoves + 1;
    state->capture = NO_PIECE;
    state->move = MOVE_NULL;

    sideToMove = ~Me;
    h ^= Zobrist::sideToMoveKey;

    state->hash = h;
    assert(computeHash() == hash());

    updateThreatenedSquares<~Me>();
    state->checkers = EmptyBB; // Null move cannot gives check
    updatePinsAndCheckMask<~Me, false>();
}

template void Position::doNullMove<WHITE>();
template void Position::doNullMove<BLACK>();

template<Side Me> void Position::undoNullMove() {
    state--;
    sideToMove = Me;
}

template void Position::undoNullMove<WHITE>();
template void Position::undoNullMove<BLACK>();

inline void Position::updateBitboards() { 
    sideToMove == WHITE ? updateBitboards<WHITE>() : updateBitboards<BLACK>(); 
}

template<Side Me>
inline void Position::updateBitboards() {
    updateThreatenedSquares<Me>();
    updateCheckers<Me>();
    checkers() ? updatePinsAndCheckMask<Me, true>() : updatePinsAndCheckMask<Me, false>();
}

template<Side Me>
inline void Position::updateThreatenedSquares() {
    constexpr Side Opp = ~Me;

    assert(state->threatsFor[PAWN] == EmptyBB);

    // Pawns
    Bitboard threatened = pawnAttacks<Opp>(getPiecesBB(Opp, PAWN));
    state->threatsFor[BISHOP] = state->threatsFor[KNIGHT] = threatened;

    // Knights
    Bitboard enemies = getPiecesBB(Opp, KNIGHT);
    bitscan_loop(enemies) {
        threatened |= attacks<KNIGHT>(bitscan(enemies), getPiecesBB());
    }

    Bitboard occupied = getPiecesBB() ^ getPiecesBB(Me, KING); // x-ray through king

    // Bishops
    enemies = getPiecesBB(Opp, BISHOP);
    bitscan_loop(enemies) {
        threatened |= attacks<BISHOP>(bitscan(enemies), occupied); // x-ray through king
    }
    state->threatsFor[ROOK] = threatened;

    // Rooks
    enemies = getPiecesBB(Opp, ROOK);
    bitscan_loop(enemies) {
        threatened |= attacks<ROOK>(bitscan(enemies), occupied);
    }
    state->threatsFor[QUEEN] = threatened;

    // Queens
    enemies = getPiecesBB(Opp, QUEEN);
    bitscan_loop(enemies) {
        Square enemy = bitscan(enemies);
        threatened |= attacks<BISHOP>(enemy, occupied);
        threatened |= attacks<ROOK>(enemy, occupied);
    }

    // King
    threatened |= attacks<KING>(getKingSquare(Opp));

    state->threatsFor[KING] = threatened;
}

template<Side Me, bool InCheck>
inline void Position::updatePinsAndCheckMask() {
    constexpr Side Opp = ~Me;
    Square ksq = getKingSquare(Me);
    Bitboard pd = EmptyBB, po = EmptyBB, cm;

    if constexpr (InCheck) {
        cm = (pawnAttacks(Me, ksq) & getPiecesBB(Opp, PAWN)) | (attacks<KNIGHT>(ksq) & getPiecesBB(Opp, KNIGHT));
    }

    Bitboard pinners = (attacks<BISHOP>(ksq, getPiecesBB(Opp)) & getPiecesBB(Opp, BISHOP, QUEEN));
    bitscan_loop(pinners) {
        Square s = bitscan(pinners);
        Bitboard b = betweenBB(ksq, s);

        switch(popcount(b & getPiecesBB(Me))) {
            case 0: if constexpr (InCheck) cm |= b | bb(s); break;
            case 1: pd |= b | bb(s);
        }
    }

    pinners = (attacks<ROOK>(ksq, getPiecesBB(Opp)) & getPiecesBB(Opp, ROOK, QUEEN));
    bitscan_loop(pinners) {
        Square s = bitscan(pinners);
        Bitboard b = betweenBB(ksq, s);

        switch(popcount(b & getPiecesBB(Me))) {
            case 0: if constexpr (InCheck) cm |= b | bb(s); break;
            case 1: po |= b | bb(s);
        }
    }

    state->pinDiag = pd;
    state->pinOrtho = po;
    if constexpr (InCheck) state->checkMask = cm;
}

template<Side Me>
inline void Position::updateCheckers() {
    Square ksq = getKingSquare(Me);
    constexpr Side Opp = ~Me;

    state->checkers = ((pawnAttacks(Me, ksq) & getPiecesBB(Opp, PAWN))
        | (attacks<KNIGHT>(ksq) & getPiecesBB(Opp, KNIGHT))
        | (attacks<BISHOP>(ksq, getPiecesBB()) & getPiecesBB(Opp, BISHOP, QUEEN))
        | (attacks<ROOK>(ksq, getPiecesBB()) & getPiecesBB(Opp, ROOK, QUEEN))
    );
}

uint64_t Position::computeHash() const {
    uint64_t h = 0;

    for (Square sq=SQ_FIRST; sq<NB_SQUARE; ++sq) {
        Piece p = getPieceAt(sq);
        if (p == NO_PIECE) continue;

        h ^= Zobrist::keys[p][sq];
    }

    h ^= Zobrist::castlingKeys[getCastlingRights()];
    if (getEpSquare() != SQ_NONE) h ^= Zobrist::enpassantKeys[fileOf(getEpSquare())];
    if (getSideToMove() == BLACK) h ^= Zobrist::sideToMoveKey;

    return h;
}

// Static exchange evaluation. Algorithm from stockfish
bool Position::see(Move move, int threshold) const {
    assert(isValidMove(move));
    assert(getSideToMove() == side(getPieceAt(moveFrom(move))));

    Square from = moveFrom(move);
    Square to = moveTo(move);

    Score value = PieceValue<MG>(getPieceAt(to)) - threshold;
    if (value < 0) return false;

    value = PieceValue<MG>(getPieceAt(from)) - value;
    if (value <= 0) return true;

    Bitboard occupied = getPiecesBB() ^ from ^ to;
    Side me = ~getSideToMove();
    Bitboard allAttackers = getAttackers(to, occupied) & occupied;
    int result = 1;

    while (true) {
        Bitboard myAttackers = allAttackers & getPiecesBB(me);
        if (!myAttackers) break; // No more attackers

        // TODO: remove pinned pieces

        result ^= 1;

        Bitboard nextAttacker;

        if ((nextAttacker = myAttackers & getPiecesTypeBB(PAWN))) { // Pawn
            value = PieceValue<MG>(PAWN) - value;
            if (value < result) break;

            occupied ^= bitscan(nextAttacker);
            allAttackers |= attacks<BISHOP>(to, occupied) & getPiecesTypeBB(BISHOP, QUEEN); // Add X-Ray attackers
        } else if ((nextAttacker = myAttackers & getPiecesTypeBB(KNIGHT))) { // Knight
            value = PieceValue<MG>(KNIGHT) - value;
            if (value < result) break;

            occupied ^= bitscan(nextAttacker);
        } else if ((nextAttacker = myAttackers & getPiecesTypeBB(BISHOP))) { // Bishop
            value = PieceValue<MG>(BISHOP) - value;
            if (value < result) break;

            occupied ^= bitscan(nextAttacker);
            allAttackers |= attacks<BISHOP>(to, occupied) & getPiecesTypeBB(BISHOP, QUEEN); // Add X-Ray attackers
        } else if ((nextAttacker = myAttackers & getPiecesTypeBB(ROOK))) { // Rook
            value = PieceValue<MG>(ROOK) - value;
            if (value < result) break;

            occupied ^= bitscan(nextAttacker);
            allAttackers |= attacks<ROOK>(to, occupied) & getPiecesTypeBB(ROOK, QUEEN); // Add X-Ray attackers
        } else if ((nextAttacker = myAttackers & getPiecesTypeBB(QUEEN))) { // Queen
            value = PieceValue<MG>(QUEEN) - value;
            if (value < result) break;

            occupied ^= bitscan(nextAttacker);
            allAttackers |= (attacks<BISHOP>(to, occupied) & getPiecesTypeBB(BISHOP, QUEEN)) // Add X-Ray attackers
                         |  (attacks<ROOK>(to, occupied) & getPiecesTypeBB(ROOK, QUEEN));
        } else { // King
            // King cannot capture if opponent still has attackers
            return (allAttackers & ~getPiecesBB(me)) ? result ^ 1 : result;
        }

        me = ~me;
        allAttackers &= occupied;
    }

    return (bool)result;
}

} /* namespace bchess */


namespace bchess {

template<Side Me, Phase P>
Score evaluateMaterial(const Position &pos) {
    static_assert(P == MG || P == EG);
    constexpr Side Opp = ~Me;
    Score score = 0;

    score += PieceValue<P>(PAWN)*pos.nbPieces(Me, PAWN) - PieceValue<P>(PAWN)*pos.nbPieces(Opp, PAWN);
    score += PieceValue<P>(KNIGHT)*pos.nbPieces(Me, KNIGHT) - PieceValue<P>(KNIGHT)*pos.nbPieces(Opp, KNIGHT);
    score += PieceValue<P>(BISHOP)*pos.nbPieces(Me, BISHOP) - PieceValue<P>(BISHOP)*pos.nbPieces(Opp, BISHOP);
    score += PieceValue<P>(ROOK)*pos.nbPieces(Me, ROOK) - PieceValue<P>(ROOK)*pos.nbPieces(Opp, ROOK);
    score += PieceValue<P>(QUEEN)*pos.nbPieces(Me, QUEEN) - PieceValue<P>(QUEEN)*pos.nbPieces(Opp, QUEEN);

    return score;
}

template<Side Me, Phase P>
Score evaluatePSQT(const Position &pos) {
    static_assert(P == MG || P == EG);
    constexpr Side Opp = ~Me;
    Score score = 0;

    Bitboard myPieces = pos.getPiecesBB(Me);
    bitscan_loop(myPieces) {
        Square sq = bitscan(myPieces);
        PieceType pt = pieceType(pos.getPieceAt(sq));
        score += PSQT[pt][P][relativeSquare(Me, sq)];
    }

    Bitboard oppPieces = pos.getPiecesBB(Opp);
    bitscan_loop(oppPieces) {
        Square sq = bitscan(oppPieces);
        PieceType pt = pieceType(pos.getPieceAt(sq));
        score -= PSQT[pt][P][relativeSquare(Opp, sq)];
    }

    return score;
}

template<Side Me, Phase P>
Score evaluate(const Position &pos) {
    Score score = 0;
    score += evaluateMaterial<Me, P>(pos);
    score += evaluatePSQT<Me, P>(pos);

    return score;
}

template<Side Me>
Score evaluate(const Position &pos) {
    Score mg = evaluate<Me, MG>(pos);
    Score eg = evaluate<Me, EG>(pos);

    int phase = 4 * pos.nbPieceTypes(QUEEN)
              + 2 * pos.nbPieceTypes(ROOK)
              + 1 * pos.nbPieceTypes(KNIGHT)
              + 1 * pos.nbPieceTypes(BISHOP);

    Score score = (mg*phase +  eg*(PHASE_TOTAL - phase)) / PHASE_TOTAL;
    score += Tempo;

    return score;
}

template Score evaluate<WHITE>(const Position &pos);
template Score evaluate<BLACK>(const Position &pos);

} /* namespace bchess */
#include <iostream>

namespace bchess {

std::ostream& operator<<(std::ostream& os, const MoveList& moves) {
    bool first = true;

    for(Move m : moves) {
        if (!first) os << " ";
        os << Uci::formatMove(m);
        first = false;
    }

    return os;
}

} /* namespace bchess*/
#include <cstring>
#include <stdexcept>

namespace bchess {

// Global Transposition Table
TranspositionTable tt;

TranspositionTable::TranspositionTable(size_t defaultSize): buckets(nullptr), nbBuckets(0), age(0) {
    resize(defaultSize);
}

TranspositionTable::~TranspositionTable(){
    if (buckets != nullptr)
        std::free(buckets);
}

void TranspositionTable::resize(size_t size){
    if (buckets != nullptr) {
        std::free(buckets);
        buckets = nullptr;
    }

    nbBuckets = size / sizeof(TTBucket);

    if (nbBuckets > 0) {
        buckets = static_cast<TTBucket *>(std::malloc(sizeof(TTBucket) * nbBuckets));
        if (!buckets) throw std::runtime_error("failed to allocate memory for transposition table");
    }

    clear();
}

void TranspositionTable::clear() {
    std::memset(buckets, 0, nbBuckets * sizeof(TTBucket));
    age = 0;
}

void TranspositionTable::newSearch() {
    age += TTEntry::AGE_DELTA;
}

size_t TranspositionTable::usage() const {
    const size_t sampleSize = 1000;
    size_t count = 0;
    
    for (size_t i = 0; i < sampleSize; i++) {
        for (size_t j = 0; j < TT_ENTRIES_PER_BUCKET; j++) {
            const TTEntry &tte = buckets[i].entries[j];
            count += !tte.empty() && tte.age() == age;
        }
    }

    return 1000 * count / (sampleSize * TT_ENTRIES_PER_BUCKET);
}

std::tuple<bool, TTEntry *> TranspositionTable::get(uint64_t hash) {
    TTBucket *bucket = &buckets[index(hash)];

    for (TTEntry *entry = bucket->begin(); entry < bucket->end(); entry++) {
        if (entry->hashEquals(hash) || entry->empty()) {
            entry->refresh(age);

            return TTResult(!entry->empty(), entry);
        }
    }

    TTEntry *toReplace = bucket->begin();

    for (TTEntry *entry = bucket->begin() + 1; entry < bucket->end(); entry++) {
        if (toReplace->isBetterToKeep(*entry, age)) {
            toReplace = entry;
        }
    }

    return TTResult(false, toReplace);
}

// Update TTEntry with fresh informations. Logic is greatly inspired from stockfish
void TranspositionTable::set(TTEntry *tte, uint64_t hash, int depth, int ply, Bound bound, Move move, Score eval, Score score, bool pv) {
    assert(depth >= 0);
    assert(tte != nullptr);
    assert(move != MOVE_NULL);

    if (move != MOVE_NONE || !tte->hashEquals(hash)) {
        tte->move16 = move;
    }

    if (bound == BOUND_EXACT || !tte->hashEquals(hash) || (depth + 2*pv + 2 > tte->depth())) {
        tte->hash16 = (uint16_t)hash;
        tte->eval16 = (int16_t)eval;
        tte->score(score, ply);
        tte->depth8 = (uint8_t)depth;
        tte->ageFlags8 = (uint8_t)(age | (pv << 2) | bound);
    }
}

} /* namespace bchess */
#include <iostream>
#include <thread>
#include <cmath>

namespace bchess {

int Engine::LMRTable[MAX_PLY][MAX_MOVE];

void Engine::init() {
    for (int d=1; d<MAX_PLY; d++) {
        for (int m=1; m<MAX_PLY; m++) {
            LMRTable[d][m] = int(0.25 + 0.46 * std::log(d) * std::log(m));
        }
    }
}

void updatePv(MoveList &pv, Move move, const MoveList &childPv) {
    pv.clear();
    pv.push_back(move);
    pv.insert(childPv.begin(), childPv.end());
}

void Engine::waitForSearchFinish() {
    searching.wait(true);
}

// Search entry point
void Engine::search(const SearchLimits &limits) {
    if (searching.test()) return;

    startTime = now();
    initAllocatedTime();
    aborted.clear(); // false
    searching.test_and_set(); // true
    nbNodes = 0;
    this->limits = limits;
    
    tt.newSearch();
    std::thread th([&] { 
        this->idSearch();
    });
    th.detach();
}

void Engine::stop() {
    aborted.test_and_set();
}

// Iterative deepening loop
template<Side Me>
void Engine::idSearch() {
    MoveList bestPv;
    Score bestScore;
    int depth = 1, searchDepth, completedDepth = 0;

    do {
        Score alpha = -SCORE_INFINITE, beta = SCORE_INFINITE;
        Score delta = 0, score = -SCORE_INFINITE;

        searchDepth = depth;

        // Aspiration window
        if (depth > 4) {
            delta = 16 + std::abs(bestScore)/100;
            alpha = std::max(-SCORE_INFINITE, bestScore - delta);
            beta  = std::min( SCORE_INFINITE, bestScore + delta);
        }

        while (true) {
            if (alpha < -1000) alpha = -SCORE_INFINITE;
            if (beta > 1000) beta = SCORE_INFINITE;
            //std::cout << "  depth=" << searchDepth << " d=" << delta << std::endl;
            score = negaMax<Me, NodeType::Root>(alpha, beta, searchDepth, 0, false);

            if (score <= alpha) { // Fail low
                //std::cout << "  Fail Low: a=" << alpha << " b=" << beta << " score=" << score << std::endl;
                beta = (alpha + beta) / 2;
                alpha = std::max(score - delta, -SCORE_INFINITE);
                searchDepth = depth;
            } else if (score >= beta) { // Fail high
                //std::cout << "  Fail High: a=" << alpha << " b=" << beta << " score=" << score << std::endl;
                beta = std::min(score + delta, SCORE_INFINITE);
                searchDepth -= (std::abs(score) < 1000);
            } else {
                break;
            }

            delta += delta / 2;
        }

        bestPv = get_node(0).pv;
        bestScore = score;
        completedDepth = depth;

        onSearchProgress(SearchEvent(depth, bestPv, bestScore, nbNodes, getElapsed(), tt.usage()));

        if (limits.maxDepth > 0 && depth >= limits.maxDepth) break;

        if (shouldStopSoft() || searchAborted() ) break;

        depth += 2;
    } while ( depth < MAX_PLY );

    SearchEvent event(depth, bestPv, bestScore, nbNodes, getElapsed(), tt.usage());

    if (depth != completedDepth) {
        event.depth = completedDepth;
        onSearchProgress(event);
    }

    onSearchFinish(event);

    searching.clear();
    searching.notify_one();
}

// Negamax search
template<Side Me, NodeType NT>
Score Engine::negaMax(Score alpha, Score beta, int depth, int ply, bool cutNode) {
    constexpr bool PvNode = (NT != NodeType::NonPV);
    constexpr bool RootNode = (NT == NodeType::Root);
    constexpr NodeType QNodeType = PvNode ? NodeType::PV : NodeType::NonPV;

    // Quiescence
    if (depth <= 0) {
        return qSearch<Me, QNodeType>(alpha, beta, depth, ply);
    }

    // Mate distance pruning
    if (!RootNode) {
        alpha = std::max(alpha, -SCORE_MATE + ply);
        beta  = std::min(beta, SCORE_MATE - ply - 1);

        if (alpha >= beta) return alpha;
    }

    Node& node = get_node(ply);
    Score alphaOrig = alpha;
    Score bestScore = -SCORE_INFINITE;
    Move bestMove = MOVE_NONE;
    Position &pos = rootPosition;
    bool inCheck = pos.inCheck();
    Score eval;
    bool improving = false;

    if (RootNode) {
        node.pv.clear();
    }

    if (pos.isFiftyMoveDraw() || pos.isRepetitionDraw()) {
        // "Random" either -1 or 1, avoid blindness to 3-fold repetitions
        return 1-(nbNodes & 2);
        //return SCORE_DRAW;
    }

    if (ply >= MAX_PLY) [[unlikely]] {
        return evaluate<Me>(pos); // TODO: verify if we are in check ?
    }

    // Query Transposition Table
    auto&&[ttHit, tte] = tt.get(pos.hash());
    Score ttScore = tte->score(ply);
    bool ttPv = PvNode || (ttHit && tte->isPv());
    Move ttMove = ttHit ? tte->move() : MOVE_NONE;
    bool ttTactical = ttHit ? pos.isTactical(ttMove) : false;

    // Transposition Table cutoff
    if (!PvNode && ttHit && tte->depth() >= depth && tte->canCutoff(ttScore, beta)) {
        return ttScore;
    }

    // Static eval
    if (!inCheck) {
        if (ttHit) {
            node.staticEval = eval = (tte->eval() != SCORE_NONE ? tte->eval() : evaluate<Me>(pos));

            // Use score instead of eval if available. 
            if (tte->canCutoff(ttScore, eval)) {
                eval = tte->score(ply);
            }
        } else {
            node.staticEval = eval = evaluate<Me>(pos);
            tt.set(tte, pos.hash(), 0, ply, BOUND_NONE, MOVE_NONE, eval, SCORE_NONE, ttPv);
        }

        // Improving
        if (ply >= 2 && get_node(ply - 2).staticEval != SCORE_NONE)
            improving = (node.staticEval > get_node(ply - 2).staticEval);
        else if (ply >= 4 && get_node(ply - 4).staticEval != SCORE_NONE)
            improving = (node.staticEval > get_node(ply - 4).staticEval);
    } else {
        node.staticEval = eval = SCORE_NONE;
    }

    moveHistory.clearKillers(ply+1);

    int nbMoves = 0;
    MovePicker mp(pos, ttMove, &moveHistory, ply);
    PartialMoveList quietMoves;
    
    mp.enumerate<MAIN, Me>([&](Move move, bool& skipQuiets) -> bool {
        nbMoves++;

        bool moveIsTactical = pos.isTactical(move);

        // Late move pruning
        if (!RootNode && bestScore > -SCORE_MATE_MAX_PLY) {
            // Move count pruning
            skipQuiets = (nbMoves >= 3 + depth*depth/(improving ? 1 : 2));

            // SEE Pruning
            if (depth <= 8 && !pos.see(move, moveIsTactical ? -100*depth : -60*depth)) {
                return true; // continue;
            }
        }

        nbNodes++;

        if (PvNode)
            get_node(ply+1).pv.clear();

        // Do move
        pos.doMove<Me>(move);

        Score score;

        // Late move reduction (LMR)
        if (depth >= 2 && nbMoves > 1) {
            int R = LMRTable[depth][nbMoves];

            R -= PvNode;
            R -= pos.inCheck();
            R += !ttPv;
            R += ttTactical;
            R += 2*cutNode;
            R += !improving;
            R -= moveHistory.getHistory<Me>(move) / 2048;

            R = std::min(depth - 1, std::max(1, R));

            // Reduced depth, Zero window
            score = -negaMax<~Me, NodeType::NonPV>(-alpha-1, -alpha, depth-R, ply+1, true);

            if (score > alpha && R != 1) {
                // Full depth, Zero window
                score = -negaMax<~Me, NodeType::NonPV>(-alpha-1, -alpha, depth-1, ply+1, !cutNode);
            }

        } else if (!PvNode || nbMoves > 1) {
            // Zero window (PVS)
            score = -negaMax<~Me, NodeType::NonPV>(-alpha-1, -alpha, depth-1, ply+1, !cutNode);
        }
        else {
            // Full window (PVS)
            score = -negaMax<~Me, NodeType::PV>(-beta, -alpha, depth-1, ply+1, false);
        }

        // Undo move
        pos.undoMove<Me>(move);

        if (searchAborted()) return false; // break

        if (score > bestScore) {
            bestScore = score;
            
            if (bestScore > alpha) {
                bestMove = move;
                alpha = bestScore;
                if (PvNode)
                    updatePv(node.pv, move, get_node(ply+1).pv);

                if (alpha >= beta) {
                    moveHistory.update<Me>(pos, bestMove, ply, depth, quietMoves);
                    return false; // break
                }
            }
        }

        if (move != bestMove && quietMoves.size() < quietMoves.capacity() && !pos.isTactical(move)) {
            quietMoves.push_back(move);
        }

        return true;
    }); if (searchAborted()) return bestScore;

    // Checkmate / Stalemate detection
    if (nbMoves == 0) {
        return inCheck ? -SCORE_MATE + ply : SCORE_DRAW;
    }

    // Update Transposition Table
    Bound ttBound =         bestScore >= beta         ? BOUND_LOWER : 
                    !PvNode || bestScore <= alphaOrig ? BOUND_UPPER : BOUND_EXACT;
    tt.set(tte, pos.hash(), depth, ply, ttBound, bestMove, SCORE_NONE, bestScore, ttPv);

    return bestScore;
}

// Quiescence search
template<Side Me, NodeType NT>
Score Engine::qSearch(Score alpha, Score beta, int depth, int ply) {
    constexpr bool PvNode = (NT != NodeType::NonPV);

    // Default bestScore for mate detection, if InCheck and there is no move this score will be returned
    Score bestScore = -SCORE_MATE + ply;
    Move bestMove = MOVE_NONE;
    Position &pos = position();

    if (pos.isFiftyMoveDraw() || pos.isRepetitionDraw()) {
        // "Random" either -1 or 1, avoid blindness to 3-fold repetitions
        return 1-(nbNodes & 2);
    }

    if (ply >= MAX_PLY) [[unlikely]] {
        return evaluate<Me>(pos); // TODO: check if we are in check ?
    }

    bool inCheck = pos.inCheck();
    Score eval = SCORE_NONE;

    // Query Transposition Table
    auto&&[ttHit, tte] = tt.get(pos.hash());
    bool ttPv = PvNode || (ttHit && tte->isPv());
    int ttDepth = inCheck ? 1 : 0; // If we are in check use depth=1 because when we are in check we go through all moves
    Score ttScore = tte->score(ply);

    // Transposition Table cutoff
    if (!PvNode && ttHit && tte->depth() >= ttDepth && tte->canCutoff(ttScore, beta)) {
        return ttScore;
    }

    // Standing Pat
    if (!inCheck) {
        if (ttHit) {
            eval = (tte->eval() != SCORE_NONE ? tte->eval() : evaluate<Me>(pos));

            // Use score instead of eval if available. 
            if (tte->canCutoff(ttScore, beta)) {
                eval = tte->score(ply);
            }
        } else {
            eval = evaluate<Me>(pos);
            tt.set(tte, pos.hash(), ttDepth, ply, BOUND_NONE, MOVE_NONE, eval, SCORE_NONE, ttPv);
        }

        if (eval >= beta) {
            return eval;
        }

        if (eval > alpha)
            alpha = eval;

        bestScore = eval;
    }

    Move ttMove = tte->move();
    // If ttMove is quiet we don't want to use it past a certain depth to allow qSearch to stabilize
    bool useTTMove = ttHit && isValidMove(ttMove) && (depth >= -7 || pos.inCheck() || pos.isTactical(ttMove));
    MovePicker mp(pos, useTTMove ? ttMove : MOVE_NONE);
    //MovePicker *mp = new (&node.mp) MovePicker(pos, useTTMove ? ttMove : MOVE_NONE);

    mp.enumerate<QUIESCENCE, Me>([&](Move move, /*unused*/bool& skipQuiets) -> bool {
        // SEE Pruning
        if (!pos.see(move, 0)) return true; // continue;
        
        nbNodes++;

        pos.doMove<Me>(move);
        Score score = -qSearch<~Me, NT>(-beta, -alpha, depth-1, ply+1);
        pos.undoMove<Me>(move);

        if (searchAborted()) return false; // break

        if (score > bestScore) {
            bestScore = score;
            
            if (bestScore > alpha) {
                bestMove = move;
                alpha = bestScore;

                if (alpha >= beta) {
                    return false; // break
                }
            }
        }

        return true;
    });

    // Update Transposition Table
    Bound ttBound = bestScore >= beta ? BOUND_LOWER : BOUND_UPPER;
    tt.set(tte, pos.hash(), ttDepth, ply, ttBound, bestMove, eval, bestScore, ttPv);

    return bestScore;
}

} /* namespace bchess */
#include <cassert>
#include <algorithm>
#include <ctime>

#include <vector>
#include <chrono>
#include <thread>
#include <algorithm>

namespace bchess {

constexpr int DEFAULT_BENCH_DEPTH = 15;

std::vector<std::string> BENCH_POSITIONS = {
    // fens from Stormphrax, from alexandria, ultimately from bitgenie
    "r3k2r/2pb1ppp/2pp1q2/p7/1nP1B3/1P2P3/P2N1PPP/R2QK2R w KQkq a6 0 14",
    "4rrk1/2p1b1p1/p1p3q1/4p3/2P2n1p/1P1NR2P/PB3PP1/3R1QK1 b - - 2 24",
    "r3qbrk/6p1/2b2pPp/p3pP1Q/PpPpP2P/3P1B2/2PB3K/R5R1 w - - 16 42",
    "6k1/1R3p2/6p1/2Bp3p/3P2q1/P7/1P2rQ1K/5R2 b - - 4 44",
    "8/8/1p2k1p1/3p3p/1p1P1P1P/1P2PK2/8/8 w - - 3 54",
    "7r/2p3k1/1p1p1qp1/1P1Bp3/p1P2r1P/P7/4R3/Q4RK1 w - - 0 36",
    "r1bq1rk1/pp2b1pp/n1pp1n2/3P1p2/2P1p3/2N1P2N/PP2BPPP/R1BQ1RK1 b - - 2 10",
    "3r3k/2r4p/1p1b3q/p4P2/P2Pp3/1B2P3/3BQ1RP/6K1 w - - 3 87",
    "2r4r/1p4k1/1Pnp4/3Qb1pq/8/4BpPp/5P2/2RR1BK1 w - - 0 42",
    "4q1bk/6b1/7p/p1p4p/PNPpP2P/KN4P1/3Q4/4R3 b - - 0 37",
    "2q3r1/1r2pk2/pp3pp1/2pP3p/P1Pb1BbP/1P4Q1/R3NPP1/4R1K1 w - - 2 34",
    "1r2r2k/1b4q1/pp5p/2pPp1p1/P3Pn2/1P1B1Q1P/2R3P1/4BR1K b - - 1 37",
    "r3kbbr/pp1n1p1P/3ppnp1/q5N1/1P1pP3/P1N1B3/2P1QP2/R3KB1R b KQkq b3 0 17",
    "8/6pk/2b1Rp2/3r4/1R1B2PP/P5K1/8/2r5 b - - 16 42",
    "1r4k1/4ppb1/2n1b1qp/pB4p1/1n1BP1P1/7P/2PNQPK1/3RN3 w - - 8 29",
    "8/p2B4/PkP5/4p1pK/4Pb1p/5P2/8/8 w - - 29 68",
    "3r4/ppq1ppkp/4bnp1/2pN4/2P1P3/1P4P1/PQ3PBP/R4K2 b - - 2 20",
    "5rr1/4n2k/4q2P/P1P2n2/3B1p2/4pP2/2N1P3/1RR1K2Q w - - 1 49",
    "1r5k/2pq2p1/3p3p/p1pP4/4QP2/PP1R3P/6PK/8 w - - 1 51",
    "q5k1/5ppp/1r3bn1/1B6/P1N2P2/BQ2P1P1/5K1P/8 b - - 2 34",
    "r1b2k1r/5n2/p4q2/1ppn1Pp1/3pp1p1/NP2P3/P1PPBK2/1RQN2R1 w - - 0 22",
    "r1bqk2r/pppp1ppp/5n2/4b3/4P3/P1N5/1PP2PPP/R1BQKB1R w KQkq - 0 5",
    "r1bqr1k1/pp1p1ppp/2p5/8/3N1Q2/P2BB3/1PP2PPP/R3K2n b Q - 1 12",
    "r1bq2k1/p4r1p/1pp2pp1/3p4/1P1B3Q/P2B1N2/2P3PP/4R1K1 b - - 2 19",
    "r4qk1/6r1/1p4p1/2ppBbN1/1p5Q/P7/2P3PP/5RK1 w - - 2 25",
    "r7/6k1/1p6/2pp1p2/7Q/8/p1P2K1P/8 w - - 0 32",
    "r3k2r/ppp1pp1p/2nqb1pn/3p4/4P3/2PP4/PP1NBPPP/R2QK1NR w KQkq - 1 5",
    "3r1rk1/1pp1pn1p/p1n1q1p1/3p4/Q3P3/2P5/PP1NBPPP/4RRK1 w - - 0 12",
    "5rk1/1pp1pn1p/p3Brp1/8/1n6/5N2/PP3PPP/2R2RK1 w - - 2 20",
    "8/1p2pk1p/p1p1r1p1/3n4/8/5R2/PP3PPP/4R1K1 b - - 3 27",
    "8/4pk2/1p1r2p1/p1p4p/Pn5P/3R4/1P3PP1/4RK2 w - - 1 33",
    "8/5k2/1pnrp1p1/p1p4p/P6P/4R1PK/1P3P2/4R3 b - - 1 38",
    "8/8/1p1kp1p1/p1pr1n1p/P6P/1R4P1/1P3PK1/1R6 b - - 15 45",
    "8/8/1p1k2p1/p1prp2p/P2n3P/6P1/1P1R1PK1/4R3 b - - 5 49",
    "8/8/1p4p1/p1p2k1p/P2npP1P/4K1P1/1P6/3R4 w - - 6 54",
    "8/8/1p4p1/p1p2k1p/P2n1P1P/4K1P1/1P6/6R1 b - - 6 59",
    "8/5k2/1p4p1/p1pK3p/P2n1P1P/6P1/1P6/4R3 b - - 14 63",
    "8/1R6/1p1K1kp1/p6p/P1p2P1P/6P1/1Pn5/8 w - - 0 67",
    "1rb1rn1k/p3q1bp/2p3p1/2p1p3/2P1P2N/PP1RQNP1/1B3P2/4R1K1 b - - 4 23",
    "4rrk1/pp1n1pp1/q5p1/P1pP4/2n3P1/7P/1P3PB1/R1BQ1RK1 w - - 3 22",
    "r2qr1k1/pb1nbppp/1pn1p3/2ppP3/3P4/2PB1NN1/PP3PPP/R1BQR1K1 w - - 4 12",
    "2r2k2/8/4P1R1/1p6/8/P4K1N/7b/2B5 b - - 0 55",
    "6k1/5pp1/8/2bKP2P/2P5/p4PNb/B7/8 b - - 1 44",
    "2rqr1k1/1p3p1p/p2p2p1/P1nPb3/2B1P3/5P2/1PQ2NPP/R1R4K w - - 3 25",
    "r1b2rk1/p1q1ppbp/6p1/2Q5/8/4BP2/PPP3PP/2KR1B1R b - - 2 14",
    "6r1/5k2/p1b1r2p/1pB1p1p1/1Pp3PP/2P1R1K1/2P2P2/3R4 w - - 1 36",
    "rnbqkb1r/pppppppp/5n2/8/2PP4/8/PP2PPPP/RNBQKBNR b KQkq c3 0 2",
    "2rr2k1/1p4bp/p1q1p1p1/4Pp1n/2PB4/1PN3P1/P3Q2P/2RR2K1 w - f6 0 20",
    "3br1k1/p1pn3p/1p3n2/5pNq/2P1p3/1PN3PP/P2Q1PB1/4R1K1 w - - 0 23",
    "2r2b2/5p2/5k2/p1r1pP2/P2pB3/1P3P2/K1P3R1/7R w - - 23 93"
};

class BenchEngine : public UciEngine {
public:
    size_t nbNodes = 0;
    TimeMs elapsed = 0;

    size_t nps() { return 1000ull * nbNodes / std::max((uint64_t)elapsed, (uint64_t)1); }

private:
    virtual void onSearchProgress(const SearchEvent &event) {
        //UciEngine::onSearchProgress(event);
    }
    virtual void onSearchFinish(const SearchEvent &event) {
        UciEngine::onSearchFinish(event);
        nbNodes += event.nbNodes;
        elapsed += event.elapsed;
    }
};

inline void bench(int depth) {
    BenchEngine engine;

    for (auto fen : BENCH_POSITIONS) {
        SearchLimits limits;
        limits.maxDepth = depth;

        console << "position fen " << fen << std::endl;
        console << "go depth " << depth << std::endl;

        engine.newGame();
        engine.position().setFromFEN(fen);
        engine.search(limits);
        engine.waitForSearchFinish();
    }

    console << std::endl << "-----------------------------" << std::endl;
    console << "Elapsed: " << engine.elapsed << std::endl;
    console << engine.nbNodes << " nodes " << engine.nps() << " nps" << std::endl;
}
    
} /* namespace bchess */

namespace bchess {

Console console;

Console::~Console() {
    if (file != nullptr) delete file;
}

std::istream& Console::getline(std::string& x) {
    std::getline(std::cin, x);
    console.log(x, true).log('\n');
    return std::cin;
}

void Console::setLogFile(const std::string &filename) {
    if (file != nullptr) delete file;
    file = new std::ofstream(filename, std::ios::app);
}

Uci::Uci()  {
    console << "bchess " << VERSION << " by Joseph Huang" << std::endl;
    
    options["Debug Log File"] = UciOption("", [&] (const UciOption &opt) { console.setLogFile(opt); });
    options["Hash"] = UciOption(64, 1, 1048576, [&] (const UciOption &opt) { 
        engine.setHashSize(int64_t(opt)*1024*1024);
    });
    options["Threads"] = UciOption(1, 1, 1);

    commands["uci"] = &Uci::cmdUci;
    commands["isready"] = &Uci::cmdIsReady;
    commands["ucinewgame"] = &Uci::cmdUciNewGame;
    commands["setoption"] = &Uci::cmdSetOption;
    commands["position"] = &Uci::cmdPosition;
    commands["go"] = &Uci::cmdGo;
    commands["stop"] = &Uci::cmdStop;
    commands["quit"] = &Uci::cmdQuit;

    commands["debug"] = &Uci::cmdDebug;
    commands["d"] = &Uci::cmdDebug;
    commands["eval"] = &Uci::cmdEval;
    commands["perft"] = &Uci::cmdPerft;
    commands["test"] = &Uci::cmdTest;
    commands["bench"] = &Uci::cmdBench;
}

Square Uci::parseSquare(std::string str) {
    if (str.length() < 2) return SQ_NONE;

    char col = str[0], row = str[1];
    if ( col < 'a' || col > 'h') return SQ_NONE;
    if ( row < '1' || row > '8') return SQ_NONE;

    return square(File(col - 'a'), Rank(row - '1'));
}

std::string Uci::formatSquare(Square sq) {
    std::string str;
    str += 'a'+fileOf(sq);
    str += '1'+rankOf(sq);

    return str;
}

std::string Uci::formatScore(Score score) {
    std::stringstream ss;

    if (abs(score) >= SCORE_MATE_MAX_PLY) {
        ss << "mate " << (score > 0 ? SCORE_MATE - score + 1 : -SCORE_MATE - score) / 2;
    } else {
        ss << "cp " << score;
    }
    
    return ss.str();
}

std::string Uci::formatMove(Move m) {
    if (m == MOVE_NONE) {
        return "(none)";
    } else if (m == MOVE_NULL) {
        return "(null)";
    }
    
    std::string str = formatSquare(moveFrom(m)) + formatSquare(moveTo(m));
    
    if (moveType(m) == PROMOTION) {
        str += " pnbrqk"[movePromotionType(m)];
    }

    return str;
}

Move Uci::parseMove(std::string str) const {
    if (str.length() == 5) str[4] = char(tolower(str[4]));

    Move move;
    enumerateLegalMoves(engine.position(), [&](Move m) {
        if (str == formatMove(m)) {
            move = m;
            return false;
        }

        return true;
    });

    return move;
}

void Uci::loop(int argc, char* argv[]) {
    if (argc > 1 && std::string(argv[1]) == "bench") {
        int depth = DEFAULT_BENCH_DEPTH;
        if (argc > 2) depth = parseInt(std::string(argv[2]));

        bench(depth);

        return;
    }

    std::string line, token;

    while(console.getline(line)) {
        std::istringstream parser(line);

        if (line.empty()) continue;

        token.clear();
        parser >> std::skipws >> token;

        bool unknownCommand = true, exit = false;
        for (auto const& [cmd, handler] : commands) {
            if (cmd != token) continue;

            unknownCommand = false;

            if (!(this->*handler)(parser)) {
                exit = true;
            }

            break;
        }

        if (unknownCommand) {
            console << "unknown command '" << token << "'" << std::endl;
        }

        if (exit) {
            break;
        }
    }

    // cleanup
    console << "Exiting UCI loop" << std::endl;
}

bool Uci::cmdUci(std::istringstream &is) {
    console << "id name bchess " << VERSION << std::endl;
    console << "id author Joseph Huang" << std::endl;

    console << std::endl;

    for (auto const& [name, option] : options) {
        console << "option name " << name << " " << option << std::endl;
    }

    console << "uciok" << std::endl;

    return true;
}

bool Uci::cmdIsReady(std::istringstream& is) {
    console << "readyok" << std::endl;

    return true;
}

bool Uci::cmdUciNewGame(std::istringstream& is) {
    engine.newGame();
    return true;
}

bool Uci::cmdSetOption(std::istringstream& is) {
    std::string token, name, value;

    // name
    is >> token;
    while (is >> token && token != "value") {
        name += (name.empty() ? "" : " ") + token;
    }

    while (is >> token) {
        value += (value.empty() ? "" : " ") + token;
    }

    if (!options.count(name))
        console << "unknown option '" << name << "'" << std::endl;
        
    options[name] = value;

    return true;
}

bool Uci::cmdPosition(std::istringstream& is) {
    std::string token, fen;

    is >> token;

    if (token == "startpos") {
        fen = STARTPOS_FEN;
    } else if (token == "kiwipete") {
        fen = KIWIPETE_FEN;
    } else if (token == "fen") {
        while (is >> token && token != "moves") {
            fen += token + " ";
        }
    } else {
        return true;
    }

    if (!engine.position().setFromFEN(fen)) {
        console << "Invalid FEN position" << std::endl;
        return true;
    }

    while (is >> token) {
        Move m = parseMove(token);
        
        if (m == MOVE_NONE) continue;

        engine.position().doMove(m);
    }

    return true;
}

bool Uci::cmdGo(std::istringstream& is) {
    std::string token;
    SearchLimits params;

    while (is >> token) {
        if (token == "perft") {
            return cmdPerft(is);
        } else if (token == "searchmoves") {
            while (is >> token) {
                Move m = Uci::parseMove(token);
                params.searchMoves.push_back(m);
            }
        } else if (token == "ponder") {
            // unsupported
        } else if (token == "wtime") {
            is >> token;
            params.timeLeft[WHITE] = parseInt(token);
        } else if (token == "btime") {
            is >> token;
            params.timeLeft[BLACK] = parseInt(token);
        } else if (token == "winc") {
            is >> token;
            params.increment[WHITE] = parseInt(token);
        } else if (token == "binc") {
            is >> token;
            params.increment[BLACK] = parseInt(token);
        } else if (token == "movestogo") {
            is >> token;
            params.movesToGo = parseInt(token);
        } else if (token == "depth") {
            is >> token;
            params.maxDepth = parseInt(token);
        } else if (token == "nodes") {
            is >> token;
            params.maxNodes = parseInt(token);
        } else if (token == "mate") {
            // unsupported
        } else if (token == "movetime") {
            is >> token;
            params.maxTime = parseInt(token);
        } else if (token == "infinite") {
             
        }
    }

    engine.search(params);
    return true;
}

bool Uci::cmdDebug(std::istringstream& is) {
    std::string token;
    is >> token;

    if (token == "moves") {
        enumerateLegalMoves(engine.position(), [&] (Move m) {
            console << Uci::formatMove(m) << std::endl;
            return true;
        });
    } else if (token == "movepicker") {
        MovePicker mp(engine.position());

        mp.enumerate<MAIN>([&] (Move m, bool& skipQuiets) {
            console << Uci::formatMove(m) << std::endl;
            return true;
        });
    } else if (token == "see") {
        is >> token;
        Move m = Uci::parseMove(token);

        if (m == MOVE_NONE) {
            console << "Invalid move " << token << std::endl;
            return true;
        }

        is >> token;
        int threshold = parseInt(token);

        console << Uci::formatMove(m) << "/" << threshold << " => " << (engine.position().see(m, threshold) ? "PASS" : "FAIL") << std::endl;
    } else {
        console << engine.position() << std::endl;
    }
    
    return true;
}

bool Uci::cmdEval(std::istringstream& is) {
    console << "Static eval: " << evaluate(engine.position()) << std::endl;
    return true;
}

bool Uci::cmdPerft(std::istringstream& is) {
    int depth = 1;
    is >> depth;

    perft(engine.position(), depth);

    return true;
}

bool Uci::cmdStop(std::istringstream& is) {
    engine.stop();
    return true;
}

bool Uci::cmdQuit(std::istringstream& is) {
    return false;
}

bool Uci::cmdTest(std::istringstream& is) {
    Test::run();
    
    return true;
}

bool Uci::cmdBench(std::istringstream& is) {
    int depth = DEFAULT_BENCH_DEPTH;
    is >> depth;

    bench(depth);
    
    return true;
}

void UciEngine::onSearchProgress(const SearchEvent &event) {
    console << "info"
        << " depth " << event.depth 
        << " multipv " << 1
        << " score " << Uci::formatScore(event.bestScore)
        << " nodes " << event.nbNodes
        << " nps " << (int)((float)event.nbNodes / std::max<std::common_type_t<int, TimeMs>>(1, event.elapsed) * 1000.0f)
        << " time " << event.elapsed
        << " hashfull " << event.hashfull;

    if (!event.pv.empty()) 
        console << " pv " << event.pv;
    
    console << std::endl;
}

void UciEngine::onSearchFinish(const SearchEvent &event) {
    Move bestMove = MOVE_NONE;
    if (!event.pv.empty()) bestMove = event.pv.front();

    console << "bestmove " << Uci::formatMove(bestMove) << std::endl;
}

} /* namespace bchess */
#include <map>
#include <vector>
#include <string>
#include <iostream>

namespace bchess::Test {

struct TestCase {
    std::string fen;
    int depth;
    size_t nbNodes;
};

std::vector<TestCase> ALL_TESTS = {
    {"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 6, 119060324},             // Startpos
    {"r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 5, 193690690}, // Kiwipete
    {"8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -", 6, 11030083},                                 // En passant & pins
    {"r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 6, 706045033},     // Castling
    {"8/r3P1K1/3P4/8/8/2k5/1p6/B1N5 w - - 0 1", 6, 24491602},                               // Promotion & pins
    {"rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8", 5, 89941194},             
    {"r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10", 5, 164075551},
    {"2kq4/8/b5b1/1N6/2PNB3/r1BK1R1r/2R1N3/1b3b2 w - - 0 1", 6, 302150595},                 // Pins !
    {"3r1b2/1k6/8/1PpP4/3K4/8/1R6/8 w - c6 0 2", 7, 167705139},                             // En passant
    {"3k4/8/8/2KpP2r/8/8/8/8 w - - 0 2", 6, 1441479}                                        // En passant
};

void run() {
    Position pos;
    int i = 1, nbTest = ALL_TESTS.size(), nbFailed = 0;

    for(auto t : ALL_TESTS) {
        console << "[Test " << i << "/" << nbTest << "] \"" << t.fen << "\"" << std::endl;

        pos.setFromFEN(t.fen);
        size_t result = perft<false>(pos, t.depth);

        if (result == t.nbNodes) {
            console << "  SUCCESS - " << t.nbNodes << " == " << result << std::endl;
        } else {
            console << "  FAILED! - " << t.nbNodes << " != " << result << std::endl;
            nbFailed++;
        }

        i++;
    }

    console << std::endl << std::endl;

    if (nbFailed > 0) {
        console << "##############################" << std::endl;
        console << "/!\\ Some tests failed :( /!\\" << std::endl;
        console << "##############################" << std::endl;
    } else {
        console << "----------------------------------------" << std::endl;
        console << " Congratulations! All tests succeeded ! " << std::endl;
        console << "----------------------------------------" << std::endl;
    }
    
}

} /* namespace bchess::Test */
#include <iostream>

namespace bchess {

/**
 * Perft using the legal move generator
 */
template<bool Div, Side Me>
size_t perft(Position &pos, int depth) {
    size_t total = 0;
    MoveList moves;
    
    if (!Div && depth <= 1) {
        enumerateLegalMoves<Me>(pos, [&](Move m) {
            total += 1;
            return true;
        });

        return total;
    }
    
    enumerateLegalMoves<Me>(pos, [&](Move move) {
        size_t n = 0;

        if (Div && depth == 1) {
            n = 1;
        } else {
            pos.doMove<Me>(move);
            n = (depth == 1 ? 1 : perft<false, ~Me>(pos, depth - 1));
            pos.undoMove<Me>(move);
        }

        total += n;

        if (Div && n > 0)
            console << Uci::formatMove(move) << ": " << n << std::endl;

        return true;
    });

    return total;
}

template size_t perft<true, WHITE>(Position &pos, int depth);
template size_t perft<false, WHITE>(Position &pos, int depth);
template size_t perft<true, BLACK>(Position &pos, int depth);
template size_t perft<false, BLACK>(Position &pos, int depth);

template<bool Div>
size_t perft(Position &pos, int depth) {
    return pos.getSideToMove() == WHITE ? perft<Div, WHITE>(pos, depth) : perft<Div, BLACK>(pos, depth);
}

template size_t perft<true>(Position &pos, int depth);
template size_t perft<false>(Position &pos, int depth);

void perft(Position &pos, int depth) {
    console << "perft depth=" << depth << std::endl;
    auto begin = now();
    size_t n = perft<true>(pos, depth);
    auto end = now();

    auto elapsed = end - begin;
    console << std::endl << "Nodes: " << n << std::endl;
	console << "NPS: " << size_t(n * 1000 / elapsed) << std::endl;
	console << "Time: " << elapsed << "ms" << std::endl;
}

} /* namespace bchess */
