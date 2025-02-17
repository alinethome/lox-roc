# LoxRoc

This is a Roc implementation of the Lox language described in [Crafting Interpreters](https://craftinginterpreters.com/). Currently, only the scanner is implemented. 

## Usage

LoxRoc requires the [0.0.0-alpha2-* release of the Roc language](https://github.com/roc-lang/roc/releases/tag/0.0.0-alpha2-rolling). It can be compiled from source by running `roc build main.roc` inside the project's root directory. 

Once compiled, `./main <path to source file>` will run the program. Ex: `./main ex.lox`. 

As only the scanner has been implemented, executing the compiler with a lox file will only produce a representation of the tokens found in the file. 

## To-Do

For the scanner: 
    * Implement better error handling with line numbers included. 
    * Clean up some of the migrated code (from the migration to 0.0.0-alpha2) for more consistent styling (especially around 'try'). 
    * Implement tests. 

In general:
    * Implement parsing and evaluating. 
    * Implement REPL functionality. 

