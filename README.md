# Compiler Frontend

A simple compiler frontend with typechecking written with `yacc` for a turing complete language. A project for Rutgers CS Spring '22.

The file SourceLangugeBNF.txt lists the context free grammar for the project. These rules
have been specified in file parse.y given to you.

## Usage

The compiler reads in the program through stdin, like so:

```console
./codegen < testcases/demo1
```
