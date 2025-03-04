# FORTH Interpreter in Haskell

This project is a FORTH interpreter implemented in Haskell. It is designed as an assignment template to help you learn Haskell, the Cabal build system, and unit/functional testing. The interpreter supports arithmetic operations, string manipulation, and a few built-in functions like `EMIT`, `CR`, `STR`, `CONCAT2`, and `CONCAT3`.

## Table of Contents

- [Installation](#installation)
- [Usage](#usage)
  - [Building the Project](#building-the-project)
  - [Running the Interpreter](#running-the-interpreter)
  - [Running Unit Tests](#running-unit-tests)
  - [Functional Tests](#functional-tests)
- [Project Structure](#project-structure)
- [Features](#features)
- [Notes and Issues](#notes-and-issues)
- [Bonus](#bonus)
- [Author Notes](#author-notes)

## Installation

Make sure you are inside the `FORTH` directory and install the required packages by running:

```bash
cabal install
cabal install hbase
```

## Usage

### Building the Project

To compile the existing code, run:

```bash
cabal build
```

### Running Unit Tests

Unit tests are written using HSpec. To run the unit tests, use the `runhaskell` command:

```bash
runhaskell ValSpec.hs
runhaskell EvalSpec.hs
runhaskell InterpretSpec.hs
```

### Functional Tests

The project includes 10 functional test files (`t1.4TH` to `t10.4TH`) and corresponding output files (`t1.out` to `t10.out`). These tests ensure that the interpreter's output matches the expected output exactly. To run any functional test, for example:

```bash
cabal run FORTH -- tests/t1.4TH
```

## Project Structure

- **Main.hs**: Entry point of the interpreter. It reads a test file and, if the stack is not empty at the end of execution, prints a warning and the stack content.
- **Eval.hs**: Contains the core evaluation logic for built-in FORTH functions and operators.
- **Val.hs**: Defines the `Val` data type and helper functions to convert strings to values.
- **Utils.hs**: Provides utility functions like `trim` and `stripQuotes`.
- **Interpret.hs**: Contains the interpreter function that processes a FORTH program.
- **Test Files**: Files named `t*.4TH` serve as functional tests. Their expected outputs are defined in the corresponding `*.out` files.
- **Unit Test Files**: Files such as `ValSpec.hs`, `EvalSpec.hs`, and `InterpretSpec.hs` provide unit tests.

## Features

- **Arithmetic Operators**: Supports addition, subtraction, multiplication, division, and power operations.
- **String Manipulation**: Functions to convert values to strings (`STR`), emit ASCII characters (`EMIT`), and concatenate strings (`CONCAT2` and `CONCAT3`).
- **Stack Management**: Includes operations like duplication (`DUP`) and printing (`.`).
- **Built-in Formatting**: `CR` for newline formatting.
- **Error Handling**: Produces appropriate errors (e.g., stack underflow, division by zero).

## Notes and Issues

- **Stack Order**: The order in which tokens are processed (and thus the order on the stack) is critical for correct evaluation. Ensure your test files are written with the correct order.
- **Test Files**: When running functional tests, verify that the output matches exactly the expected output in the corresponding `.out` files.
- **Debugging**: If you encounter errors (e.g., stack underflow), check that each operator is used with the proper number of arguments.

## Bonus

For extra credit, user-defined functions have been partially implemented. See the bonus section in the source code for details on how to define and use them. Additional tests have been provided to cover bonus functionality.

## Author Notes

- Assignment by Zeeshaan Dayam
- UFID: 85934599
