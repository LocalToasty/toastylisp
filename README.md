[![Build Status](https://travis-ci.org/LocalToasty/toastylisp.svg)](https://travis-ci.org/LocalToasty/toastylisp)

# ToastyLisp - A Pure Lisp Dialect #

Once installed with `cargo install`, the interpreter can be invoked the following way:
```bash
$ toastylisp [-v | --verbose ] <filename>
```

## Features ##

- [x] Currying
- [x] Tail call optimization
- [x] Variadic functions


### Planned Features ###

- [ ] Loading of external files
- [ ] Quasiquotes
- [ ] Strings
- [ ] Floating point numbers
- [ ] Structures
- [ ] Classes
- [ ] Static typing
- [ ] Lazy evaluation
- [ ] Parallelisation
- [ ] REPL


## A Little Tour of ToasyLisp ##

### Simple Arithmetic ###

ToastyLisp, as most other languages, can do simple arithmetic.
There are a few built in functions for performing calculations, for example `+`, `-`, `*`, `/` and `mod`.
Like many lisp dialects, functions applications are done using the polish notation, which means that the function is written before its arguments.
```lisp
(+ 2 3)    -> 5
(- 10 12) -> -2
(* 7 8)   -> 56
(/ 5 3)   -> 1
(mod 5 3) -> 2
```
ToastyLisp only supports integer arithmetic at the moment.
Therefore if a number does not evenly divide another, a division of the two is rouded towards zero.

Function invocations can be nested.
This allows us to do more complicated calculations:
```lisp
(+ (* 7 8) (/ 12 4)) -> 60
(+ 2 (* 2 2))        -> 6
```


### Definitions ###

In almost every programming language there are variables.
This Lisp dialect is a pure language:
once a symbol has been defined, it cannot be redefined.
A constant can be defined as follows:
```lisp
(define meaning-of-life 42)
```

With this snippet of code we have defined the symbol `meaning-of-life` as 42.
Whenever we use `meaning-of-life` now, it will be substituted for its value:
```lisp
(+ meaning-of-life 7) -> 49
```

Define form always evaluates the expression the symbol is going to be bound to:
```
(define half-life-three (* 3 meaning-of-life))
half-life-three -> 126
```

The define form can be used to make computations more readable by naming values.

A symbol defined with the define form will exist until the end of its scope.
Sometimes we only need to define symbols for use in a single expression.
For these cases the let form is used:

```lisp
(let ((x 3)
      (y 4)
      (z (+ x y)))
  (+ (* x y) z)) -> 19
```

All symbols defined in a let form are not accessible outside of it:
```lisp
(let ((x 3))
  (+ x 1)) -> 4

(+ x 2) -> Error: Symbol 'x' is not defined
```

While redefinitions within a scope are forbidden, it is very well allowed to define a variable defined in another scope.
This process is called shadowing.
Consider the following procedure:
```lisp
(define x 1)
(print x)    ; prints 1

(let ((x 2))
  (print x)) ; prints 2

(print x)    ; prints 1 again
```

Here we redefine `x` within the let form, shadowing its old definition.
This new value of `x` is only available within the let form itself.
Once we leave the let form, `x` is back to its old value.


### Functions and Lambdas ###

In most programs there are parts which can be reused.
These parts are usually put into functions, which allow to easily execute the same code multiple times.
If we calculate many squares in our code, we could decide to create a function for squaring numbers:
```lisp
(define square (lambda (x) (* x x))
```

This expression defines the symbol `square`, but instead of binding it to a numeric value like in the previous examples, this time we bind it to a function object, a *lambda*.
In this example, the lambda takes an argument, `x`, and returns the square of it.
Let's see our newly defined function in action:
```lisp
(square 3)  -> 9
(square -2) -> 4
(square meaning-of-life) -> 1764
```

Of course our function doesn't have to take exactly one argument, but can take any amount of arguments, or even no arguments at all.
```lisp
(define sum-of-squares
  (lambda (a b)
    (+ (square a) (square b))))

(sum-of-squares 3 4) -> 25
```

One might wounder why there is need for functions without parameters.
A use case of parameterless functions is created by the closure property of lambdas:
Lambdas may refer to symbols from the environment they were first defined in, even if these symbols are not in scope any more:
```lisp
(define capturing-lambda
  (let ((x 3))
    (lamdba () (+ x 4))))

(capturing-lambda) -> 7
```


### Conditional Branching ###

So far all the procedures we have looked at were stricly sequential.
However it is possible for a program to behave differently depending on the previous computations.
The first way of doing so is the if form, which takes a condition, an expression to execute if the condition is true, and an expression to evaluate otherwise.
The following function uses the if form to calculate the absolute value of a number:
```lisp
(define abs
  (lambda (x)
    (if (< x 0)
        (- 0 x)
      x)))

(abs 5)  -> 5
(abs -6) -> 6
```

The if form checks if the functions argument is less than zero.
If that is the case, the arguments sign is flipped.
Otherwise, the unaltered argument is returned.

If more than one condition is required, the cond form can be used.
The cond form consists of multiple conditions with multiple conditions.
The conditions are evaluated from top to bottom:
The first consequence whose condition is true is evaluated.
If none of the conditions evaluates to true, the progam terminates.
Consider the following function calculating the sign of a number:
```lisp
(define sign
  (lambda (x)
    (cond ((< x 0) -1)
          ((= x 0) 0)
          ((> x 0) 1))))

(sign 10) -> 1
(sign 3)  -> 1
(sign -7) -> -1
(sign 0)  -> 0
```


### Recursion ###

Functional programming languages do usually not have loops.
Instead, recursion, the self-invocation of functions, is used to achieve multiple execution of the same code path.
We will start of with the "hello world" of recursive functions, the calculation of a factorial.

The factorial of 0 is 1.
The factorial of a number is the product of all the integers from 1 to this number, or in other words, the number times the factorial of the number minus one.
To put it into lisp terms:
```lisp
(define fac
  (lambda (n)
    (if (= n 0) 1
      (* n (fac (- n 1))))))

(fac 0) -> 1
(fac 5) -> 120
```

Let's reconstruct step for step, how this function is evaluated:
```lisp
(fac 3)
(* 3 (fac (- 3 1))
(* 3 (fac 2))
(* 3 (* 2 (fac (- 2 1))))
(* 3 (* 2 (fac 1)))
(* 3 (* 2 (* 1 (fac (- 1 1)))))
(* 3 (* 2 (* 1 (fac 0))))
(* 3 (* 2 (* 1 1)))
(* 3 (* 2 1))
(* 3 2)
6
```
We can see how the function calculates the factorial by recursively calculating the factorial of ever smaller numbers.

As a second example we will take a look at an implementation of euclids algorithm to calculate the greatest common divider of two numbers:
```lisp
(define gcd
  (lambda (a b)
    (if (= 0 b) a
      (gcd b (mod a b)))))
```

And once again a look at how it is evaluated:
```lisp
(gcd 72 56)
(gcd 56 (mod 72 56))
(gcd 56 16)
(gcd 16 (mod 56 16))
(gcd 16 8)
(gcd 8 (mod 16 8))
(gcd 8 0)
8
```

It can be seen, that the factorial function gets "wider" as it descends into the recursion tree.
Because of this, it will eventually overflow the stack.
The GCD function however doesn't seem to "widen".
This is because GCD is *tail recursive*.
Tail recursive functions call themselves as their last action, which allows the compiler to keep the heap size constant.
Because of this, it is desirable for functions to be tail recursive, especially if it isn't known how deep the recursion will go.
To illustrate how to make a function tail recursive, we will now rewrite the factorial function from above:
```lisp
(define fac-iter
  (lambda (acc n)
    (if (= n 0) acc
      (fac-iter (* n acc) (- n 1)))))

(define fac (lambda (n) (fac-iter (1 n))))
```

fac-iter is obviously tail recursive, as can be seen if looking at the way it's evaluated:
```lisp
(fac 3)
(fac-iter 1 3)
(fac-iter (* 3 1) (- 3 1))
(fac-iter 3 2)
(fac-iter (* 2 3) (- 2 1))
(fac-iter 6 1)
(fac-iter (* 1 6) (- 1 1))
(fac-iter 6 0)
6
```

While it was possible to convert the factorial function into a tail recursive function, this is not true for the general case.


### Pairs and Lists ###

A pair is a data structure which holds exactly two other values.
Pairs can be constructed using the `cons` function:
```lisp
(cons 1 2) -> (1 . 2)
(cons + -) -> (+ . -)
```

The first element of a pair in toastylisp is called its head, the second one its tail.
They can be accessed using the `head` and `tail` functions respectively:
```lisp
(define p (cons 1 2))
(head p) -> 1
(tail p) -> 2
```

A list is defined as a pair whose head holds the first element of the list, and whose tail contains a list containing all but its first element.
The empty list is defined to be #nil.
With this inductive definition we can now create lists using the `cons` function:
```lisp
(define ls (cons 1 (cons 2 (cons (+ 1 2) #nil))))
(head ls) -> 1
(tail ls) -> (2 3)
```

Most algorithms operating on lists do so recursively.
They look at a finite amount of the lists first elements, perform calculations with them, and then repeat that procedure on the remainder of the list.
One of the most used patterns on lists is the map function:
```lisp
(define map
  (lambda (f xs)
    (if (nil? xs) '()
      (cons (f (head xs)) (map f (tail xs))))))
```

The map function takes a function `f` and a list `xs` and returns a new list, containing the result of f applied to each element.
```lisp
(map square '(2 3 4 5)) -> (4 9 16 25)
(map (lambda (x) (* 2 x)) '(1 2 3)) -> (2 4 6)
```

Another common operation on lists are folds, sometimes also called "accumulations" or "reductions".
A fold function takes a function `f`, and an initial value for the accumulator.
```lisp
(define foldl
  (lambda (f init xs)
    (if (nil? xs) init
      (foldl f (f init (head xs)) (tail xs)))))
```

This function might seem a bit confusing at first, but after a bit of use it quickly becomes apparent how it works.
```lisp
(define sum (lambda (xs) (fold + 0 xs)))
(sum '(1 2 3)) -> 6
```

The foldl function initializes the accumulator with `init` and then recursively updates it by assigning it to `f` applied to each element of the list.
In this example, it is initialized with 0 and each element of the list is added to it, resulting in a function summing the list.
The sum function applied to a list xs with elements *x_1* ... *x_n* is equivalent to (..((0 + *x_1*) + *x_2*) .. + *x_n*).

A close cousin to the `foldl` function is `foldr`:
```lisp
(define foldr
  (lambda (f init xs)
    (if (nil? xs) init
      (f (head xs) (foldr f init (tail xs))))))
```

It works similarly to `foldl`, but performs a right-associative fold on the list.
If we had formulated our sum function with foldr instead of foldl, it would be equivalent to (*x_1* + (*x_2* + (... + (*x_n* + 0)))).
In this special case foldl and foldr can be used interchangably.
If `f` is not commutative however, both functions will yield different results.
Whenever there is a choice, `foldl` should be used, as it is tail recursive.


### Variadic Functions ###

Functions cannot only take a fixed number of arguments, but also be variadic.
To create a variadic function, the last parameter of a function is simply suffixed with a `...`.
Whenever the function is invoked, all but the last parameter is bound as usual, and the last parameter is bound to a list containing the remaining arguments.
It should be noted that this can also be an empty list.

Many of the builtin functions are defined as variadic, for example `+`, `*`, `=`, and the comparison operators `<` and `>`.
```lisp
(+ 1 2 3) -> 6
(* 2 3 4) -> 24
(= 1 1 2) -> #false
(< 1 2 3) -> #true
```

Using variadic functions, we can easily define a list constructor:
```lisp
(define list (lambda (xs...) xs))
(list 1 2 (+ 3 4)) -> (1 2 7)
```

A more often used function would be the one that composes other functions.
Applying the composition of functions *f_1*,..., *f_n* to an argument *x* is equivalent to sucessively applying the functions from right to left: (*f_1* (*f_2* (...(*f_n* *x*)...))).
The attentive reader might have noticed that this is indeed a form of right-associative fold:
```lisp
(define id (lambda (x) x))

(define comp
  (lambda (fs...)
    (foldr (lambda (f g) (lambda (x) (f (g x))))
           id
           fs)))
```


### Currying ###

One of the defining features of ToastyLisp is currying, or partial function application.
Whenever a function is applied to less arguments than required, a new function with the given arguments already applied is created.
This can be used to easily create new functions from already existent ones:
```lisp
(define double (* 2 _))    ; double is now bound to (lambda (x) (* 2 x))
(define increment (+ 1 _)) ; increment is bound to (lambda (x) (+ 1 x))
(define neg (- 0))

(double 10) -> 20
(increment 5) -> 6
(neg 2) -> -2
```

If something other than the first argument should be applied, the placeholder `_` can be used:
```lisp
(define half (/ _ 2))
(define decrement (/ _ 1))
```

This way we can also formulate our `sum` and `fac` functions from earlier more neatly:
```lisp
(define fac (fac-iter 1))
(define sum (foldl + 0))
```

Together with function composition, it is now possible to quickly create powerful functions from already existing ones:
```lisp
(define even? (comp (= 0) (mod _ 2)))
```

-----

The full specification of ToastyLisp can be found [here](doc/spec.md).
