# [BUT FIT 2020] DKA-2-MKA
FLP (Functional and Logic Programming) course project no.1:
Minimization of deterministic finite automaton

## Dependencies
```
GHC 8.6.5
```

## Usage
Simply compile with make (gmake) and run the program as follows:
dka-2-mka (-i | -t) [inputFile]
    -i            output internal representation of parsed DFA
    -t            output minimized input DFA
    [inputFile]   input file containing valid DFA. DFA is read
                  from standard input if omitted

## Implementation
Program expects valid DFA on input, possibly with unreachable states.
Detailed syntax for input DFA was specified by the assignment paper.
Additionally, this implementation ignores empty leading and trailing 
input lines and stricly enforces specified syntax, i.e.,
the order of each non-empty line matters and lines must not contain
any additional non-whitespace characters or trailing commas.

The algorithms for eliminiation of unreachable states and minimization
itself were taken from TIN (Theoretical Computer Science) course materials.
The whole transformation process consists of several steps:
1. Input is parsed into internal DFA data structure
2. Unreachable states are eliminated from DFA
3. Sink state is added to the DFA in order to get a complete DFA
4. Complete DFA is minimized by the aforementioned algorithm

The entire program is split into 4 files (modules):
    FiniteAutomaton.hs: contains various type declarations,
        DFA data structure declaration and all relevant transformation functions
    Parser.hs: implements simple DFA parser
    Utils.hs: contains various auxiliary functions used during parsing/transformations.
    Main.hs: implements main entry point function, handles argument parsing and file reads

## Authors:

* **Ondřej Pavela**
