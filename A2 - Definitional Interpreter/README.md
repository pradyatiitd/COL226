# Problem Statement

In this assignment, you will define the **abstract syntax** (data type **exp**) and a **definitional interpreter** eval for a simple arithmetic and boolean calculation language.<br/>

The expressions in the language are of the following forms<br/>

1. Integer constants<br/> 
2. Unary arithmetic operations: abs, (and any other sensible ones you can think of)<br/>
3. Identifiers, represented as (alphanumeric) strings<br/>
4. binary operations: + (addition), - (subtraction), * (multiplication), div, mod, ^ (exponentiation)<br/>
5. Boolean constants: T and F<br/>
6. Unary boolean operation: not<br/>
7. binary boolean operations:  /\ (and), \/ (or), -> (implies)<br/>
8. Comparison operators: = (equal) , > (greater than), < (less than) , >= (greater or equal), <= (less or equal) on integer expressions<br/>
9. n-tuples for each n > 2<br/>
10. Projection operators **proj**(i,n) which project the ith element of an n-tuple.<br/>

Assume all inputs are of proper type (we will study type-checking later).  Define a suitable data type **answer**.<br/>

eval: **exp -> answer**.<br/>

Next, define a suitable set of opcodes for a stack-and-table machine to evaluate this language and define a compiler for this language to sequences of these opcodes.<br/>

compile: **exp -> opcode list**</br>

Third, define the stack machine execution functions, which takes a sequence of opcodes and executes them starting from a given stack and table.<br/>

execute: **stack * table * opcode list -> answer**

Provide enough examples.
