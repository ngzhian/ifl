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

## Template instantiation

Graph reducer based on *template instantiation*

### Reducible expressions

A program is run by evaluating an expression,
an expression is represented as a graph,
and evaluation is done by *reductions*.

Reduction happens on a *reducible expression*, redex.

When there are no more redexes, the expression is in normal form.

When there are more than one redex, choosing any order or reducing them
will get us to normal form. However some reduction sequences fail to terminate.

### Normal order reduction

If there are terminating reduction sequences that terminates,
the *normal order reduction*, choosing the outermost redex,
will be one of them.

By convention, the first expression to be evaluated is the supercombinator named `main`.

`main` has no arguments and thus is a redex, it reduces to its body.

When reducing function application, the root of the redex is replaced with
the body of the function, substituting a pointer to the argument
for each formal parameter.

### Reduction steps

Graph reduction consists of repeating 3 steps until a normal form is reached:

1. Find the next redex
2. Reduce the redex
3. Update the root of the redex with result

There are two sorts of redexes:

1. Supercombinator, it is a redex, reduce it
2. Built in primitives, if the arguments are evaluated, it is a redex, reduce it;
otherwise it is not a redex, so evaluate the arguments first

### Spine unwinding to find function application

To find the next reduction, we can first find the outermost function application
(which might not be reducible).

Start from the root and follow the left branch of application nodes until
a supercombinator or built-in primitive is reached.
A stack is used to remember the address of the nodes going down.
This left-branching chain is called the *spine* of the expression.

Then, check how many arguments the function takes, and move up the graph
that number of nodes: the current node is now the root of the outermost
function application.

If the result of an evaluation is a partial application,
e.g. a function of 3 arguments applied to only 2,
this expression is said to reach *weak head normal form*, WHNF.
The subexpressions might contain redexes.
If the program has been appropriately typechecked, this will not happen.

Check if this function application is a redex.
If it is a supercombinator, it is. If it is a primitive, like `*`,
it depends on whether the arguments are evaluated.
If they are, this is a redex, otherwise we need to evaluate the arguments.

This can be done using the same reduction steps with a new stack.
However the old stack needs to be put away for later,
so we end up storing this stack of stacks in a *dump*.

### Reducing a supercombinator redex

A supercombinator redex is reduced by replacing the redex with an instance of the
supercombinator body,
and substituting the occurrence of formal parameters
with pointers to the actual arguments:
the arguments are shared, not copied.

### Updating a redex

After reduction, the root of the redex needs to be updated,
so that the reduction is only done once if the redex is shared.

This updating is the essence of *lazy evaluation*:
a redex will only be evaluated, if at all, once.

An *indirection node* is used to update the root of a redex
to point to the result of the reduction.

### State transition systems

Graph reductions can be implemented using state transition systems.

State transition systems describe the behavior of a sequential machine.
At any time, the machine is in some *state*, starting from an *initial state*.
If the machine's state matches one of the *state transition rules*,
the rule fires and specifies a new state for the machine to transition into.
If no rules match, execution stops.
If more than one rule matches, one is chosen arbitrarily,
and the system is called *non-deterministic*.

### Mark 1

The state of the template instantiation graph reduction machine is a quadruple
`(stack, dump, heap, globals)`, or `(s, d, h, f)`

The *stack* is a stack of *addresses*, identifying nodes in the heap.
The nodes form the spine of the expression being evaluated.

The *dump* records the state of the spine stack prior to evaluation of an argument
of a strict primitive.

The *heap* is a collection of tagged *nodes*.
You can retrieve a node in the heap using an address.

For each supercombinator, *globals* gives the address of heap node representing itself.

A heap node can be one of three forms:

1. `NAp a1 a2` represents application of node at address `a1` to node at address `a2`
2. `NSupercomb args body` represents supercombinator with arguments `args` and body `body`
3. `NNum n` represents the number `n`

There are two state transition rules for this machine.

Rule 1 describes spine unwinding:

```
          a :: s, d, h[a : NAp a1 a2], f
=>  a1 :: a :: s, d, h[a : NAp a1 a2], f
```

If the current address on the stack points to a `NAp` node in the heap,
we need to put the `a1` onto the stack first.
This is equivalent to evaluating the function before applying it to its arguments.
Effectively, this rule unwinds the entire spine of the expression onto the stack,
until the top node is no longer a `NAp` node.

Rule 2 describes supercombinator reduction:

```
    a0 :: a1 :: ... :: an :: s, d, h[a0 : NSupercomb [x1; ...; xn] body], f
=>                     ar :: s, d, h'                                   , f
where (h', ar) = instantiate body h f[x1 -> a1; ...; xn -> an]
```

If the current address on the stack points to a `NSupercomb` node in the heap,
we update the stack and the heap in this manner:
we pop the stack once to remove the address of the current supercombinator,
and we pop as many items off the stack as there are arguments to the supercombinator,
then we push the result of instantiating the body of the supercombinator,
which is an expression in our core language,
onto the stack,
this result is the address that points to the newly constructed node;
initializing the body of the supercombinator may cause the heap to change,
such as allocating a new number,
so the old heap is updated to this new one.

`instantiate` takes an expression, `body`, a heap,
and a global mapping of names to heap addresses, augmented (adding)
the mapping of argument names to their addresses obtained from the stack,
i.e. with the formal arguments bound to values popped off the stack.

`instantiate` returns a new heap and the address of the newly constructed instance.
The root of the redex is not updated by this rule.

To run a program, we parse the source into our Core AST,
compile it into a machine state,
evaluate the state stepwise,
and finally print the results and statistics.

#### Compiler

The compiler builds an initial machine state from a Core program.

All the supercombinator definitions in the program and prelude
needs to be available on the heap, and accessible by name
via globals.
We have to walk through all the definitions,
and allocate each definition on the heap.
With each allocation, we get back the address of the allocated node,
and a new heap containing this allocation.
This new heap then has to be passed to the next allocation,
and the address has to added to the globals, and
the globals need to be passed to the next allocation as well.
At the end of these allocations,
we will have a heap with all the definitions allocated,
and a globals containing all the definitions.

The initial stack contains just the address of `main`,
which is the designated entry point.

#### Evaluator

The evaluator starts from an initial machine step,
and runs the machine one step at a time,
remembering all the states it has gone through.

The evaluator runs until it reaches a final state,
which happens when the stack contains a single object that is a data node.
A data node is a number or constructor.

At each step, the evaluator looks at the top of the stack,
find out what node, there are three types, it points to,
and performs appropriate action:

1. number node, it's an error, because a number cannot
be applied, if the number node was the only thing on the stack
evaluation would not have occurred at all.
2. application node, perform stack unwinding by pushing the function
to be applied onto the stack (rule 1)
3. supercombinator node, bind argument names to argument addresses on the stack,
instantiate the body of the supercombinator with an environment
containing arguments and globals,
pop arguments and supercombinator node off the stack,
push the result address of instantiating the body onto the stack,
and update the heap with the result of instantiating the body (rule 2).

The goal of the instantiation is to create a new node that represents
the body of the supercombinator.
To instantiate the body, it needs the body expression,
a heap on which newly allocated nodes will go,
an environment, where address supercombinator and arguments can be found using names,
and will return the new updated heap, and address of allocated instance.


