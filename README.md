## Special Forms ##

### Quote `'` ###

```lisp
'*expr*
```

The quotation form returns the expression it quotes when it is evaluated.

#### Examples ####

```lisp
'foo -> foo
'(+ 2 3) -> (+ 2 3)
```


### if ###

```lisp
(if *pred* *cons* *alt*)
```

If `pred` evaluates to a value which is not nil, the result of `cons` will be returned.
Otherwise the result of `alt` is returned.

#### Example ####

```lisp
(if (< 3 5)
    '(3 is less than 5)
  '(something went wrong))
-> (3 is less than 5)
```


### cond ###

```lisp
(cond (*pred_1* *cons_1*)
      (*pred_2* *cons_2*)
       ...
      (*pred_n* *cons_n*)
      [*alt*])
```

The cond form is an extension of the if form; instead of only one arm, there are multiple, each with its own predicate.
The cond clause evaluates to the result of the first consequence `cons_i` for which the predicate `pred_i` evaluates to a value which is not nil.
If none of the predicates evaluates to a value which is not nil, the result of the `alt` expression is returned.
If there is no alternative expression, the program terminates with an error.

A cond clause which posesses the `alt` expression is semantically equivalent to

```lisp
(if *pred_1* *cons_1*
(if *pred_2* *cons_2*
 ...
(if *pred_n* *cons_n*
  *alt*)...))
```

#### Example ####

The following expression returns `negative` if x is less than 0, `positive` if x is greater than 0 and `zero` otherwise.

```lisp
(cond ((< x 0) 'negative))
      ((> x 0) 'positive)
      'zero)
```


### let ###

```lisp
(let ((*symb_1* *expr_1*)
      (*symb_2* *expr_2*)
       ...
      (*symb_n* *expr_n*))
  *body*)
```

The let form defines a set of symbols for the expriession `body`.
The definitions are done in order, meaning that the *n*-th definition can refer to every symbol `symb_i` with *i* < *n*.
Each symbol must only be defined once; redefinition is forbidden within a let form.

#### Example ####

```lisp
(let ((x 2)
      (y 5))
  (+ x y))
-> 7
```


### define ###

```lisp
(define *symb* *expr*)
```

Defines the symbol `symb` as `expr` within the current scope, and returns the evaluated expression.
Each symbol may only be defined once per scope.

#### Example ####

```
(define x 3)
(* x x)
-> 9
```


### lambda ###

```lisp
(lambda (*param_1* ... *param_n*) *body*)
```

Returns an anonymous function object with the parameters `param` and the body `body`.
If the lambda is invoked with the correct amount of arguments, the functions body is evaluated with `param_1` defined as the first argument, `param_2` with the second argument, and so forth.
Lambdas *capture* the symbols in their environment.
This means that symbols which are defined outside of the lambda can still be referred to in a lambda if they go out of scope.
Anonymous functions are often used in conjunction with a define or let statement to bind them to a name.


#### Example ####

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

```lisp
(*proc* *arg_1* ... *arg_n*)
```

Applies the arguments `arg_1` ... `arg_n` to the procedure `proc`.
If the number of arguments matches the number of the procedure's parameters, the procedure's body is evaluated with its parameters `param_i` defined as `arg_i`.
If too few arguments are supplied, the function is *curried*, meaning that a new procedure with the first *n* parameters already bound to `arg_1` to `arg_n` is returned.
The number of arguments must not surpass the number of the procedures parameters.

#### Examples ####

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
