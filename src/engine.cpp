#include <iostream>
#include <thread>
#include <cmath>
#include "engine.h"
#include "movegen.h"
#include "evaluate.h"
#include "movepicker.h"

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

void SearchData::initAllocatedTime() {
    int64_t moves = limits.movesToGo > 0 ? limits.movesToGo + 5 : 30;
    Side stm = position.getSideToMove();

    hardTimeLimit = 0.49 * limits.timeLeft[stm];
    softTimeLimit = std::min<TimeMs>(hardTimeLimit, limits.timeLeft[stm] / moves + 0.9 * limits.increment[stm]);
}

void Engine::waitForSearchFinish() {
    // TODO: use condition variable ?
    while (isSearching()) {
        std::this_thread::sleep_for(std::chrono::milliseconds(1));
    }
}

// Search entry point
void Engine::search(const SearchLimits &limits) {
    if (searching) return;

    sd = std::make_unique<SearchData>(position(), limits);
    aborted = false;
    searching = true;
    
    tt.newSearch();

    std::thread th([&] { 
        this->idSearch();
    });
    th.detach();
}

void Engine::stop() {
    aborted = true;
}

// Iterative deepening loop
template<Side Me>
void Engine::idSearch() {
    MoveList bestPv;
    Score bestScore;
    int depth, searchDepth, completedDepth = 0;

    for ( depth = 1; depth < MAX_PLY; depth += 2 ) {
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
            score = pvSearch<Me, NodeType::Root>(alpha, beta, searchDepth, 0, false);

            if (searchAborted()) break;

            if (score <= alpha) { // Fail low
                //std::cout << "  Fail Low: a=" << alpha << " b=" << beta << " score=" << score << std::endl;
                beta = (alpha + beta) / 2;
                alpha = std::max(score - delta, -SCORE_INFINITE);
                searchDepth = depth;
            } else if (score >= beta) { // Fail high
                //std::cout << "  Fail High: a=" << alpha << " b=" << beta << " score=" << score << std::endl;
                beta = std::min(score + delta, SCORE_INFINITE);
                //searchDepth = std::max(std::max(1, depth - 4), searchDepth - 1);
                searchDepth -= (std::abs(score) < 1000);
            } else {
                break;
            }

            delta += delta / 2;
        }

        if (depth > 1 && searchAborted()) break;

        bestPv = sd->node(0).pv;
        bestScore = score;
        completedDepth = depth;

        onSearchProgress(SearchEvent(depth, bestPv, bestScore, sd->nbNodes, sd->getElapsed(), tt.usage()));

        if (sd->limits.maxDepth > 0 && depth >= sd->limits.maxDepth) break;

        if (sd->shouldStopSoft()) break;
    }

    SearchEvent event(depth, bestPv, bestScore, sd->nbNodes, sd->getElapsed(), tt.usage());

    if (depth != completedDepth) {
        event.depth = completedDepth;
        onSearchProgress(event);
    }

    onSearchFinish(event);

    searching = false;
}

// Negamax search
template<Side Me, NodeType NT>
Score Engine::pvSearch(Score alpha, Score beta, int depth, int ply, bool cutNode) {
    constexpr bool PvNode = (NT != NodeType::NonPV);
    constexpr bool RootNode = (NT == NodeType::Root);
    constexpr NodeType QNodeType = PvNode ? NodeType::PV : NodeType::NonPV;

    // Quiescence
    if (depth <= 0) {
        return qSearch<Me, QNodeType>(alpha, beta, depth, ply);
    }

    // Check if we should stop according to limits
    if (!RootNode && sd->shouldStop()) [[unlikely]] {
        stop();
    }

    // If search has been aborted (either by the gui or by reaching limits) exit here
    if (!RootNode && searchAborted()) [[unlikely]] {
        return -SCORE_INFINITE;
    }

    // Mate distance pruning
    if (!RootNode) {
        alpha = std::max(alpha, -SCORE_MATE + ply);
        beta  = std::min(beta, SCORE_MATE - ply - 1);

        if (alpha >= beta) return alpha;
    }

    Node& node = sd->node(ply);
    Score alphaOrig = alpha;
    Score bestScore = -SCORE_INFINITE;
    Move bestMove = MOVE_NONE;
    Position &pos = sd->position;
    bool inCheck = pos.inCheck();
    Score eval;
    bool improving = false;

    if (RootNode) {
        node.pv.clear();
    }

    if (pos.isFiftyMoveDraw() || pos.isRepetitionDraw()) {
        // "Random" either -1 or 1, avoid blindness to 3-fold repetitions
        return 1-(sd->nbNodes & 2);
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
        if (ply >= 2 && sd->node(ply - 2).staticEval != SCORE_NONE)
            improving = (node.staticEval > sd->node(ply - 2).staticEval);
        else if (ply >= 4 && sd->node(ply - 4).staticEval != SCORE_NONE)
            improving = (node.staticEval > sd->node(ply - 4).staticEval);
    } else {
        node.staticEval = eval = SCORE_NONE;
    }

    sd->moveHistory.clearKillers(ply+1);

    int nbMoves = 0;
    MovePicker mp(pos, ttMove, &sd->moveHistory, ply);
    PartialMoveList quietMoves;
    
    mp.enumerate<MAIN, Me>([&](Move move, bool& skipQuiets) -> bool {
        // Honor UCI searchmoves
        if (RootNode && sd->limits.searchMoves.size() > 0 && !sd->limits.searchMoves.contains(move))
            return true; // continue

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

        sd->nbNodes++;

        if (PvNode)
            sd->node(ply+1).pv.clear();

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
            R -= sd->moveHistory.getHistory<Me>(move) / 2048;

            R = std::min(depth - 1, std::max(1, R));

            // Reduced depth, Zero window
            score = -pvSearch<~Me, NodeType::NonPV>(-alpha-1, -alpha, depth-R, ply+1, true);

            if (score > alpha && R != 1) {
                // Full depth, Zero window
                score = -pvSearch<~Me, NodeType::NonPV>(-alpha-1, -alpha, depth-1, ply+1, !cutNode);
            }

        } else if (!PvNode || nbMoves > 1) {
            // Zero window (PVS)
            score = -pvSearch<~Me, NodeType::NonPV>(-alpha-1, -alpha, depth-1, ply+1, !cutNode);
        }

        if (PvNode && (nbMoves == 1 || (score > alpha && (RootNode || score < beta)))) {
            // Full window (PVS)
            score = -pvSearch<~Me, NodeType::PV>(-beta, -alpha, depth-1, ply+1, false);
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
                    updatePv(node.pv, move, sd->node(ply+1).pv);

                if (alpha >= beta) {
                    sd->moveHistory.update<Me>(pos, bestMove, ply, depth, quietMoves);
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
    Position &pos = sd->position;
    //Node& node = sd->node(ply);

    if (pos.isFiftyMoveDraw() || pos.isRepetitionDraw()) {
        // "Random" either -1 or 1, avoid blindness to 3-fold repetitions
        return 1-(sd->nbNodes & 2);
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
        
        sd->nbNodes++;

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