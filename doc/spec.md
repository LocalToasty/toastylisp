# ToastyLisp Specification #

## Attention ##

This document is a draft and may contain major errors.


## Table of Contents ##

- [Comments](#comments)
- [Data Types](#data-types)
  - [Numbers](#numbers)
  - [Booleans](#booleans)
  - [Lambdas](#lambdas)
  - [Pairs](#pairs)
  - [Nil](#nil)
- [Syntactic Forms](#syntactic-forms)
  - [Quote](#quote)
  - [if](#if)
  - [cond](#cond)
  - [let](#let)
  - [define](#define)
  - [lambda](#lambda)
- [Built in Procedures](#built-in-procedures)
  - [Arithmetic Operations](#arithmetic-operations)
  - [Relations](#relations)
  - [Logical Junctors](#logical-junctors)
  - [List Operations](#list-operations)
  - [Predicates](#predicates)
  - [Miscellaneous](#miscellaneous)


## Comments ##

Comments are are parts of the source code which are ignored by the interpreter.
They are commonly used to explain parts of the code.
The begin of a comment is indicated by a `;`.
Whenever the interpreter encouters a `;`, it ignores everything until the end of the line.

```lisp
(define meaning-of-life 42)  ; the answer to life, the universe and everything
(+ meaning-of-life 1295)
```


## Data Types ##

### Numbers ###

A lisp number is a 32 bit signed integer.
A number literal consists of an optional sign `+` or `-` followed by a digit which is not 0 zero or more digits.


### Booleans ###

A boolean is a truth value; it can be either true or false.
Boolean values are represented by the `#true` and `#false` literals in lisp.


### Lambdas ###

A Lambda is an anonymous function object.
It can be created using the lambda for.


### Pairs ###

A Pair is a compound data type which holds two other values.
The first element of a pair is called its head, the other its tail.
Lisp lists are represented as nested pairs:
The first element of the pair points to its first value, the second to a list containing the remainder of the list.
The empty list is represented by `#nil`.


### Nil ###

The nil value represents nothing.
It can be created using the `#nil` literal.


## Syntactic Forms ##

### Quote ###

'*expr*

The quotation form returns the expression it quotes when it is evaluated.

```lisp
'foo -> foo
'(+ 2 3) -> (+ 2 3)
```


### if ###

(**if** *pred* *cons* *alt*)

`pred` has to evaluate to a boolean.
If `pred` evaluates to `#true`, the result of `cons` will be returned.
Otherwise the result of `alt` is returned.

```lisp
(if (< 3 5)
    '(3 is less than 5)
  '(something went wrong))
-> (3 is less than 5)
```


### cond ###

(**cond** (*pred_1* *cons_1*)
          (*pred_2* *cons_2*)
           ...
          (*pred_n* *cons_n*))

The cond form is an extension of the if form; instead of only one arm, there are multiple, each with its own predicate.
The cond clause evaluates to the result of the first consequence `cons_i` for which the predicate `pred_i` evaluates to `#true`.
Each reachable `pred_i` has to evaluate to a boolean.
If none of the predicates evaluate to `#true`, the program terminates.

The following expression returns `negative` if x is less than 0, `positive` if x is greater than 0 and `zero` otherwise.

```lisp
(cond ((< x 0) 'negative))
      ((> x 0) 'positive)
      (#true   'zero)
```


### let ###

(**let** ((*symb_1* *expr_1*)
          (*symb_2* *expr_2*)
           ...
          (*symb_n* *expr_n*))
  *body*)

The let form defines a set of symbols for the expriession `body`.
The definitions are done in order, meaning that the *n*-th definition can refer to every symbol `symb_i` with *i* < *n*.
Each symbol must only be defined once; redefinition is forbidden within a let form.

```lisp
(let ((x 2)
      (y 5))
  (+ x y))
-> 7
```


### define ###

(**define** *symb* *expr*)

Defines the symbol `symb` as `expr` within the current scope, and returns the evaluated expression.
Each symbol may only be defined once per scope.

```
(define x 3)
(* x x)
-> 9
```


### lambda ###

(**lambda** (*param_1* ... *param_n*) *body*)

Returns an anonymous function object with the parameters `param` and the body `body`.
If the lambda is invoked with the correct amount of arguments, the functions body is evaluated with `param_1` defined as the first argument, `param_2` with the second argument, and so forth.
Lambdas *capture* the symbols in their environment.
This means that symbols which are defined outside of the lambda can still be referred to in a lambda if they go out of scope.
Anonymous functions are often used in conjunction with a define or let statement to bind them to a name.


The following expression defines square as a function which takes an argument and retuns its square:

```lisp
(define square (lambda (x) (* x x)))
```

```lisp
(define capturing
  (let ((a '(I am captured)))
    (lambda () a)))

;; a is out of scope here, but can still be returned by the lambda
(capturing) -> (I am captured)
```

This function calculates the greatest common divisor of two numbers `a` and `b` using the euclidean algorithm:

```lisp
(define gcd
  (lambda (a b)
    (if (= b 0)
        a
      (gcd b (mod a b)))))
```


### Function Application ###

(*proc* *arg_1* ... *arg_n*)

Applies the arguments `arg_1` ... `arg_n` to the procedure `proc`.
If the number of arguments matches the number of the procedure's parameters, the procedure's body is evaluated with its parameters `param_i` defined as `arg_i`.
If too few arguments are supplied, the function is *curried*, meaning that a new procedure with the first *n* parameters already bound to `arg_1` to `arg_n` is returned.
The number of arguments must not surpass the number of the procedures parameters.

```lisp
(+ 1 2) -> 3

(define square (lambda (x) (* x x)))
(square 4) -> 14
```

If too few arguments are supplied, the function is curried:

```lisp
(define double (* 2))
```

Now `double` is bound to `(lambda (x) (* 2 x))`, a procedure which only takes one argument instead of two.

```lisp
(double 10) -> 20
```

## Built in Procedures ##

### Arithmetic Operations ###

#### + ####

(**+** *number_1* *number_2*)

Adds two numbers.

```lisp
(+ 2 3) -> 5
```


#### - ####

(**-** *number_1* *number_2*)

Subtracts two numbers.

```lisp
(- 2 3) -> -1
```


#### * ####

(**&#42;** *number_1* *number_2*)

Multiplies two numbers.

```lisp
(* 2 3) -> 6
```


#### / ####

(**/** *number_1* *number_2*)

Performs an integer division between two numbers.
If the first argument is not evenly divisible by the second, the result is rounded towards zero.

```lisp
(/ 5 2) -> 2
(/ -5 2) -> -2
```


#### mod ####

(**mod** *number_1* *number_2*)

Calculates the remainder of a division.

```lisp
(mod 5 2) -> 1
```

### Relations ###

#### = ####

(**=** *expr_1* *expr_2*)

Checks if two values are equal.

```lisp
(= 3 3) -> #true
(= 3 5) -> #false
(= 'foo 'foo) -> #true
(= '(1 2 3) '(1 2 3)) -> #true
```


#### < ####

(**<** *number_1* *number_2*)

Returns #true if and only if the first argument is less than the second.

```lisp
(< 1 2) -> #true
(< 2 1) -> #false
```


#### > ####

(**>** *number_1* *number_2*)

Returns `#true` if first argument is greater than the second, and `#false` otherwise.

```lisp
(> 1 2) -> #false
(> 2 1) -> #true
```


### Logical Junctors ###

#### and ####

(**and** *boolean_1* *boolean_2*)

Returns `#true` if and only if both arguments are `#true`.

```lisp
(and (> 5 3) (< 2 5)) -> #true
(and (< 5 3) (< 2 5)) -> #false
(and (< 5 3) (> 2 5)) -> #false
(and 1 0) -> Type error: expected 'boolean', found 'number'
```


#### or ####

(**or** *boolean_1* *boolean_2*)

Returns `#true` if at least one of the arguments is `#true`.

```lisp
(or (> 5 3) (< 2 5)) -> #true
(or (< 5 3) (< 2 5)) -> #true
(or (< 5 3) (> 2 5)) -> #false
(or 1 0) -> Type error: expected 'boolean', found 'number'
```


#### not ####

(**not** *boolean*)

Returns `#true` if the argument is `#false`, `#false` otherwise.

```lisp
(not (< 5 3)) -> #true
(not (> 5 3)) -> #false
```

### List Operations ###

#### cons ####

(**cons** *expr_1* *expr_2*)

Constructs a new pair.
Lists are represented as nested pairs, with the first element of the pair being the head of the list, and the second containing the tail (i.e. the remainder of the list).

```lisp
(cons 'left 'right) -> (left . right)
(cons 1 (cons 2 (cons 3 #nil))) -> (1 2 3)
```


#### head ####

Retrieves the first element of a pair.
If the argument is a list, its first element is returned.

```lisp
(head (cons 'left 'right)) -> left
(head '(1 2 3)) -> 1
(head '(1)) -> 1
```


#### tail ####

Retrieves the second element of a pair.
If the argument is a list, the lists tail (i.e. all but the first element) is returned.
If the list only has one element, the tail is the empty list, i.e. #nil.

```lisp
(tail (cons 'left 'right)) -> tail
(tail '(1 2 3)) -> '(2 3)
(tail '(1)) -> #nil
```


### Predicates ###

#### defined? ####

(**defined?** *symbol*)

Checks if *symbol* is defined.

```lisp
(define x 3)
(defined? 'x) -> #true
(defined? 'y) -> #false
(defined? x) -> Type error: Expected a symbol, found a number
```


#### number? ####

(**number?** *expr*)

Checks if *expr* evaluates to a number.

```lisp
(number? 1) -> #true
(define x 3)
(number? x) -> #true
(number? (+ 1 2)) -> #true
(number? '(1)) -> #false
```

#### boolean? ####

(**boolean?** *expr*)

Checks if *expr* evaluates to a boolean.

```lisp
(boolean? #true) -> #true
(boolean? #false) -> #true
(define x #false)
(boolean? x) -> #true
(boolean? (not #false)) -> #true
(boolean? 1) -> #false
(boolean? 0) -> #false
```

#### quote? ####

(**quote?** *expr*)

Checks if *expr* evaluates to a quote.

```lisp
(quote? 'foo) -> #false
(quote? ''foo) -> #true
```


#### lambda? ####

(**lambda?** *expr*)

Checks if *expr* evaluates to a lambda.

```lisp
(lambda? (lambda (x) (* x x))) -> #true
(lambda? +) -> #true
(lambda? (+ 2)) -> #true
(lambda? (+ 2 3)) -> #false
```


#### pair? ####

(**pair?** *expr*)

Checks if *expr* evaluates to a pair.

```lisp
(pair? (cons 1 2)) -> #true
(pair? '(1 2 3)) -> #true
(pair? 42) -> #false
```


#### nil? ####

(**nil?** *expr*)

Checks if *expr* evaluates to #nil.

```lisp
(nil? #nil) -> #true
(nil? (tail '(1))) -> #true
(nil? (head '(1))) -> #false
```


#### print! ####

(**print!** *expr*)

Prints the result of *expr*.

```lisp
(print! 3)             ; prints 3
(print! (* 7 8))       ; prints 56
(print! '(hello world) ; prints (hello world)
(print! (+ 2))         ; prints (lambda (x1) (intrinsic.+ 2 x1))
```


### Miscellaneous ###

#### eval ####

(**eval** *quote*)

Evaluates a quoted expression.

```lisp
(eval '(+ 2 3)) -> 5
```