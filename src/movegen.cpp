#include <iostream>
#include "movegen.h"
#include "uci.h"

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