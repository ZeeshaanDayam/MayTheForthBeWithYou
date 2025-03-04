# Assignment 2 PLP

This project implements a FORTH interpreter in Haskell. It serves as an assignment template to help you get comfortable with Haskell, the Cabal build system, and both unit and functional testing. The interpreter handles basic arithmetic, string operations, and includes built-in functions like `EMIT`, `CR`, `STR`, `CONCAT2`, and `CONCAT3`.

## Table of Contents

- [Installation](#installation)
- [Usage](#usage)
  - [Building the Project](#building-the-project)
  - [Running Unit Tests](#running-unit-tests)
  - [Functional Tests](#functional-tests)
- [Project Structure](#project-structure)
- [Features](#features)
- [Notes and Issues](#notes-and-issues)
- [Bonus](#bonus)
- [Author Notes](#author-notes)

## Installation

Before you start, navigate to the `FORTH` directory and install the required packages by running:

```bash
cabal install
cabal install hbase
```

## Usage

### Building the Project

To compile the code, simply execute:

```bash
cabal build
```

### Running Unit Tests

Unit tests are created with HSpec. Run them using:

```bash
runhaskell ValSpec.hs
runhaskell EvalSpec.hs
runhaskell InterpretSpec.hs
```

### Functional Tests

The project provides 10 functional test files (`t1.4TH` to `t10.4TH`) along with corresponding output files (`t1.out` to `t10.out`). These tests ensure the interpreter's output exactly matches the expected results. To run a functional test, for example:

```bash
cabal run FORTH -- tests/t1.4TH
```

## Project Structure

- **Main.hs**: The entry point. It loads a test file and, if the stack isn’t empty after execution, prints a warning along with the remaining stack.
- **Eval.hs**: Contains the evaluation logic for FORTH’s built-in functions and operators.
- **Val.hs**: Defines the `Val` data type and functions to convert strings into values.
- **Utils.hs**: Provides helper functions such as `trim` and `stripQuotes`.
- **Interpret.hs**: Implements the interpreter that processes FORTH programs.
- **Test Files**: Files named `t*.4TH` are used for functional tests. Their expected outputs are in the matching `.out` files.
- **Unit Test Files**: Files like `ValSpec.hs`, `EvalSpec.hs`, and `InterpretSpec.hs` contain the unit tests.

## Features

- **Arithmetic Operations**: Supports addition, subtraction, multiplication, division, and exponentiation.
- **String Operations**: Includes functions to convert values to strings (`STR`), output ASCII characters (`EMIT`), and concatenate strings (`CONCAT2` and `CONCAT3`).
- **Stack Operations**: Offers operations such as duplication (`DUP`) and printing (`.`).
- **Formatting**: Provides a newline function (`CR`) for formatted output.
- **Error Handling**: Detects and reports errors (for example, stack underflow or division by zero).

## Notes and Issues

- **Stack Order**: The order in which tokens are processed (and therefore the stack order) is crucial. Ensure that your test files list tokens in the correct order.
- **Test Files**: When executing functional tests, confirm that the output exactly matches what’s specified in the corresponding `.out` files.
- **Debugging**: If you encounter errors (like stack underflow), verify that each operator receives the correct number of arguments.

## Bonus

For extra credit, partial support for user-defined functions has been implemented. Check the bonus section in the source code for details on how to define and use them. Additional tests are provided to cover this functionality.

## Author Notes

- **Assignment by:** Zeeshaan Dayam  
- **UFID:** 85934599