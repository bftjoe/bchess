#include <iostream>
#include "perft.h"
#include "movegen.h"
#include "uci.h"
#include "utils.h"
#include "movepicker.h"

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