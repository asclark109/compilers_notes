# Module 1

__Compilers__ are _translators_
* translate between representations of program code
* typically: from high-level source language to machine language (object code)
![alt text](pics/compiler.JPG "Title")
* __NOT__ all compilers translate to machine code
  * Java compiler: translate java code to interpretable JVM bytecode
  * Java JIT: bytecode to machine code

---

* compilers are __NOT__ necessary to execution of all program code
  * programs can be simulated using an __interpreter__: a program that reads an executable program and produces the results of executing that program.
![alt text](pics/p2.JPG "Title")

* in certain instances, interpreters execute instructions more slowly than compiled code, depending on the type of program
![alt text](pics/p3.JPG "Title")

* executable files are typically written in either a machine code or a bytecode
  * __Machine Code__: targets a specific processor architecture (`ARM64`, `x86_64`, `SPARC`, ...); the case for `C`, `C++`, `Golang`, ...
    * one portion of the executible file contains the translated code that is not represented as binary that is run directly by the process
    * Machine code normally results in <span style="color:blue">better performance (+)</span> at the cost that it won't run on other architectures (<span style="color:red">not portable (-)</span>)
  * __Bytecode__: targets a specific processor architecture (`ARM64`, `x86_64`, `SPARC`, ...); the case for `Java`, `C#`, ...
    * reqires a virtual machine to run the code on a specific architecture
    * as long as there is a virtual machine for that specific architecture then you can execute that code. This makes it <span style="color:blue">more portable (+)</span> at the cost of <span style="color:red">performance (-)</span> since there is a layer between the code and the processor

---

## Compiler Input: soure code

source code is optimized for human readability
* uses human notations of grammar to be more <span style="color:blue">_readable_, _expressible_ (+)</span>
* Redundant to help avoid programming errors
* Final result may not be fully determined by the code (for example, code that defines a function describes logic given an input)
![alt text](pics/p4.JPG "Title")

## Compiler Output: Assembly/Machine Code

Assembly / Machine Code is optimized for hardware
* ambiguity is reduced <span style="color:blue">(+)</span> 
* reasoning about what the code is doing is lost <span style="color:red">(-)</span> 
![alt text](pics/p5.JPG "Title")

## Compiler Output: Optimized vs Unoptimized

* the compiler also makes attempts to shorten the lines of assembly / machine code
* this is an attempt to improve performance
![alt text](pics/p6.JPG "Title")

---

## Compiler Translations

how does a compiler effectively translate high-level source code to low-level machine code?
* at the lowest level, a computer only knows about the binary encodings of 1s and 0s, which represent hardware instructions and data
* a compiler translates the original source code into different program representations known as __intermediate representations__
  * these representations are designed to support the necessary program manipulations:
    * __type checking__
    * __static analysis__
    * __optimization__
    * __code generation__

![alt text](pics/p7.JPG "Title")

---

## Anatomy of a Compiler

At a high level, a compiler contains two parts:
* __front end: analysis__
  * analyze the source code and determine its structure and meaning to generate an intermediate representation
* __back end: synthesis__
  * generate low-level code for the target platform

![alt text](pics/p8.JPG "Title")

A compiler must
* recognize legal (and complain about illegal) programs
* must generate correct code
  * it may attempt to improve ("optimize") code, but must not change a code's behavior ("meaning")
* must manage storage of all variables (and code)
* must agree with OS & linker on format for object code

Each phase of the compiler uses __intermediate representation(s) IR__ to pass along results from its phase to another
* Front end maps source into an IR
* Back end maps IR to target machine code
* Compilers do often have multiple IRs - higher at first, lower level in later phases

### Front End

![alt text](pics/p9.JPG "Title")
The front end of a compiler contains two parts:
* __Lexer__: (also known as a __Scanner__) maps character stream into a token stream (keywords, operators, variables, constants, etc)
  * also removes all white space and comments
* __Parser__: reads in the tokens from the token stream and generates an IR
  * also performs semantic analysis to check for type errors, etc.

Both components can be automatically generated
* define a formal grammar to specify the source language
* an existing software tool will read the grammar and generate a scanner and parser (e.g. __ANTLR__ for C/C++, Java, Python, Go, or __flex/bison__ for C/C++)

### Scanner

Example:
* input source code: `if (x==y) x=45`
* character stream:  `i, f,  , (, x, =, =, y, ),  , x, =, 45, ;`
* token stream:      `IF, LPAREN, ID(x), EQ, ID(y), RPAREN, ID(x), ASSIGN, INT(45), SCOLON`

* __Tokens__ are distinct objects that can carry associated data with them (e.g. numeric value, variable name, line, column information, etc.)
* whitespace and comments are not tokens

### Parser

Responsible for taking the token stream and producing an IR output that captures the meaning of the program
* most common output is `Abstract Syntax Tree (AST)`
  * contains meaning of program without syntactic noise
  * internal nodes are operations, and leafs are operands
  * known as the _"natural" IR_
* AST is not the only possible output
  * __Parse Tree__ / __Syntax Tree__ is possible but usually contains additional information that is not needed

Example:
* token stream:      `IF, LPAREN, ID(x), EQ, ID(y), RPAREN, ID(x), ASSIGN, INT(45), SCOLON`
* abstract syntax tree:
![alt text](pics/p10.JPG "Title")

### Static Semantic Analysis

A step in the compiler that happens during parsing or directly after parsing to ensure a program is valid
* performs type checking
* verifies code adheres to language semantics (e.g., correct variable declarations)
* performs __code shape__ to determine many properties of the resulting program

Collects additional information for the back end like the __symbol table(s)__ (a __symbol table__ maps names to types)

### Back End

Responsibilities
* Translate IR into target machine code
* Choose instructions to implement each IR operation
* Decide which value to keep in registers
* Ensure conformance with system interfaces

Tries to produce the most "optimal" code
* optimal = fast, compact, low power (can't have them all)

Automation has been less sucessful in the back end

![alt text](pics/p11.JPG "Title")

Example:
![alt text](pics/p12.JPG "Title")

### Three-Part Compiler

Code Improvement (or Optimization)
* Analyzes IR and rewrites (or transforms) IR
* Primary goal is to reduce running time of the compiled code
* may also improve space, power consumption, ...
* must preserve the "meaning" of the code
* measured by values of named variables

![alt text](pics/p13.JPG "Title")

### Optimizer

Typical Transformations
* Discover and propagate some constant value
* Discover a redundant computation and remove it
* Remove useless or unreachable code

Tradeoffs in optimization
* Ordering of optimization phases
* what works for some programs can be bad for others

Modern optimizers are structures as a series of passes
![alt text](pics/p14.JPG "Title")

### Creating an executable

many compilers include the `assembler` and `linker` as part of the compiler
![alt text](pics/p15.JPG "Title")

### Why study Compilers?

* Compilers are responsible for many aspects of system performance
* attaining performance has become more difficult over time
  * compiler has become a prime determiner of performance

* makes you a better programmer
  * provides insight into interaction between languages, compilers, and hardware
  * allows you to understand how code maps to hardware
  * provides better intuition about what your code does
  * understanding how compilers optimize code helps you write code that is easier to optimize
    * helps with not writing pointless code that "optimizes" the performance when a compiler can do it better

