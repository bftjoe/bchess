#pragma once

#include "chess.h"


#include <cstdint>
#include "zobrist.h"


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
