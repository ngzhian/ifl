## Overview

Implement a small functional language called *Core language*,
easy to implement but allows for rich features.

Core language then translated into a machine language by a compiler.
Machine language interpreted by a machine interpreter.
Will be building 4 different machine interpreters.

Notably, core language lacks local function definition.
A technique called *lambda lifting* can turn local function definition
into global ones.

This language is tedious to directly program in,
but is suitable as a translation target, the back-end, for a higher level language.

## Core language

```
main = double 21
double x = x + x
```

### Supercombinator and Function

Core program is made up of *supercombinator definitions*, `main` is a special one.

`main` is a supercombinator with no arguments, also called a *constant applicative form* (CAF).

### `let` and `letrec` expressions

```
main = double 21
quad x = let twice = x + x
         in twice + twice
```

`let` expressions are allowed in definition body.

Use `letrec` for recursive definitions.
This separation allows for simpler implementation, as `let` is easier to implement than `letrec`.

`let` and `letrec` cannot define functions.
Functions must be defined as supercombinators at the top level.
The *left-hand side* (lhs) of `let` and `letrec` is a simple variable.

### Lambda abstractions

```
double_list xs = map (\ x . 2 * x) xs
```

The lambda abstraction `(\ x . 2 * x)` is an anonymous function that doubles its argument.
Through lambda-lifting, this can be moved into a top-level supercombinator.

### Algebraic data types

```
colour ::= Red | Green | Blue
complex ::= Rect num num | Polar num num
numPair ::= MkNumPair num num
tree * ::= Leaf * | Branch (tree *) (tree *)
```

The lhs introduces a *type*, such as `colour`, and the *right-hand side* (rhs) introduces
one or more *constructors*, such as `Red`, `Green`.

`tree` is a *parameterised* algebraic data type, it is parameterised with respect to
a *type variable* `*`.

A structured value is built using the constructors:

```
Green
Rect 3 4
```

A structured value is taken apart using *pattern matching*:

```
isRed Red = True
isRed Green = True
isRed Blue = True

first (MkNumPair n1 n2) = n1
```

We can represent and manipulate structured types by:

1. Using a simple and uniform representation for constructors
2. Transform pattern matching into `case` expressions

### Representing constructors

Provide a single family of constructors `Pack{`*tag*,*arity*`}`,
where *tag* is an integer which identifies the constructor,
and *arity* is the number of argument it takes.

```
Red   = Pack{1,0}
Green = Pack{2,0}
Blue  = Pack{3,0}

Rect  = Pack{4,2}
Polar = Pack{5,2}
```

The tag is used for differentiating between constructors of a type.

In a well-typed program, constructors of different types will not be mixed up,
so we can reset the tag for each data type.

```
Red   = Pack{1,0}
Green = Pack{2,0}
Blue  = Pack{3,0}

Rect  = Pack{1,2}
Polar = Pack{2,2}
```

### `case` expressions

Modern programming languages have complex pattern matching constructs.

In Core, pattern matching is achieved by `case` expression,
which only matches on tag and a number of variables,
which matches the arity of the constructor:

```
isRed c = case c of
            <1> -> True;
            <2> -> False;
            <3> -> False

depth t = case t of
            <1> n -> 0;
            <2> t1 t2 -> 1 + max (depth t1) (depth t2)
```

`case` expressions act like multi-way jumps.

## Data types for Core language

We represent the Core language in code.
Our implementation language is OCaml.

```ocaml
```

`expr` is parameterised by `'a`, which represents the *binders*.
A binder is the name used at the binding occurrence of a variable,
i.e. the lhs of a `let(rec)` definition.
We shall use `name` as the type of a binder.

```ocaml
```

## Pretty print

A pretty printing facility using string concatenation is slow.

Consider separating into two pars:

1. What operations to perform,
2. What's an efficient way to perform them

We build up the intended pretty print output using abstract data types,
then find a way to actually output efficiently.

## Parser for Core

Parsing split into three stages:

1. Reading from a file
2. Lexical analysis to break input sequence into *tokens*
3. Syntax analysis to group tokens into syntax

Lexical analysis by examining each character and breaking sequences into tokens.
The main tokens are numbers, variable names, whitespace, comments.
Special case for new lines to keep track of line number for error reporting.

Build a big parser by glueing together smaller parsers.

A parser takes a list of tokens, and returns a list of parsed result
with the remaining unconsumed tokens.
The list represents ambiguous parsing,
where tokens can be parsed in multiple ways.

### Infix operators

The precedence of infix operator is implicit in the grammar.
One way to make this explicit is to have several sorts of expressions:

```
expr1 -> expr2 + expr1
      |  expr2
expr2 -> expr3 - expr2
      |  expr3
expr3 -> expr4 relop expr4
      |  expr4
expr4 -> expr5 * expr4
      |  expr5
expr5 -> expr6 / expr5
      |  expr6
...
```

The lower ones have higher precedence.
This also expresses that `+` and `-` are right associative,
whereas `relop` is non-associative.

A direct parser implementation based on these rules is inefficient.
A parser will parse `expr2`, then look for `+`,
if it fails to find `+`, it will parse `expr2` again.
We can try to *share* this parsing of `expr2` by splitting the production:

```
expr1  -> expr2 expr1c
expr1c -> | expr1
          | empty
```

Where `empty` is the empty string.

We need the type of the parser result to match up, but `expr1c` is not the correct type.
So define a new data type to encapsulate this partial expression
and assemble it with `expr2` in the production of `expr1`.
