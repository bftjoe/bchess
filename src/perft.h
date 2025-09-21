#pragma once

#include "chess.h"
#include "position.h"

namespace bchess {

template<bool Div> size_t perft(Position &pos, int depth);
void perft(Position &pos, int depth);

} /* namespace bchess */

