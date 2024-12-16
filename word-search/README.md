# Word Search Program

This repository contains a word search program built with Haskell, designed to execute in either sequential or parallel modes. The program processes input files and can be configured for multi-threading.

## Building and running the program

### Prerequisites
Ensure that you have [Stack](https://docs.haskellstack.org/) installed to build and run the project.

### Build the program
1. Clean the project (optional):
    ```bash
   stack clean
2. Build the project:
   ```bash
   stack build
   
### Run the program
You can run the program using the following command:
```bash
stack exec word-search-exe -- <path-to-input-file> <solution-method> <optional: solution parameter>
```
The input file must be a .txt file with two lines. The first line must contain a representation of the game board and the second line must contain a list of target strings to search for in the game board. We provide examples in word-search/test_data.

The solution method can be one of four options:
1. "sequential"
2. "parallelwords"
3. "paralleldepth" - requires the additional solution parameter indicating depth limit to run in parallel until
4. "parallelsubgrids" - requires the additional solution parameter indicating square root of number of subgrids to split the board into

For any of the parallel options, you can specify threading options using +RTS to control the number of threads to be used for parallel execution:
```bash
stack exec word-search-exe -- <path-to-input-file> parallelsubgrids 4 +RTS -N8
```
