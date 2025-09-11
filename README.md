# bchess
A UCI-compatible chess engine written in C++. bchess is based on [Belette by Vincent Bab](https://github.com/vincentbab/Belette).

For now it only supports CPUs with AVX2 and BMI2 instructions. Support for other architectures might be added in the future.

## Compiling

Tested on Windows and Linux with GCC >= 12. Might also work with CLang

```sh
make release
```
Executable will be in `./build/Release/bin/bchess[.exe]`

## UCI Options

### Debug Log File
Log every input and output of the engine to the specified file

### Hash
Specify the hash table size in megabytes

### Threads
For now this option doesn't do anything. It's only for compatibility.

## Internals

### Board & Move generation
 - Bitboard
 - Zobrist hashing
 - Fast legal move enumeration inspired by [Gigantua](https://github.com/Gigantua/Gigantua)
```
perft 7, start position, Core i7 12700k

Nodes: 3195901860
NPS: 925007774
Time: 3455ms
```

### Search
 - Iterative deepening
 - Aspiration window
 - Negamax
 - Transpositation Table
 - Check extension
 - Null move pruning (NMP)
 - Reverse futility pruning (RFP)
 - Internal iterative reduction (IIR)
 - Late move reduction (LMR)
 - Late move pruning (LMP)
 - SEE pruning
 - Quiescence

 ### Move ordering
  - Hash move (TT Move)
  - MVV-LVA
  - Killer moves
  - Counter move
  - Threats
  - Checks
  - Butterfly history heuristic
  - Staged move generation (good captures, good quiets, bad captures, bad quiets)

### Evaluation
 - Tapered
 - Material
 - PSQT ([PeSTO](https://www.chessprogramming.org/PeSTO%27s_Evaluation_Function))