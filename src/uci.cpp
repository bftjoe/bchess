#include <cassert>
#include <algorithm>
#include <ctime>
#include "uci.h"
#include "movegen.h"
#include "test.h"
#include "perft.h"
#include "utils.h"
#include "movepicker.h"
#include "bench.h"

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
    commands["perftmp"] = &Uci::cmdPerftmp;
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

        bool unknowCommand = true, exit = false;
        for (auto const& [cmd, handler] : commands) {
            if (cmd != token) continue;

            unknowCommand = false;

            if (!(this->*handler)(parser)) {
                exit = true;
            }

            break;
        }

        if (unknowCommand) {
            console << "Unknow command '" << token << "'" << std::endl;
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
    console << "id author Vincent Bab" << std::endl;

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
        console << "Unknow option '" << name << "'" << std::endl;
        
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
        } else if (token == "perftmp") {
            return cmdPerftmp(is);
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

bool Uci::cmdPerftmp(std::istringstream& is) {
    int depth = 1;
    is >> depth;

    perftmp(engine.position(), depth);

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
