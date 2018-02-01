
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pmatch – A package for Haskell-like pattern matching in R

The goal of the `pmatch` package is to provide structure pattern
matching, similar to Haskell and ML, to R programmers. The package
provide functionality for defining new types and for matching against
the structure of such types.

The idea behind pattern matching is that we define types by how we
create them, and we have ways of matching a pattern of constructors
against a value to pick the one that matches the value.

The simplest example is a type defined just from constants. For example,
we can define the type `enum` to consist of one of `ONE`, `TWO`, or
`THREE`.

``` r
enum := ONE | TWO | THREE
```

Any of these three constants will be created by this command. If you
print them, they just give you their names:

``` r
ONE
#> ONE
TWO
#> TWO
THREE
#> THREE
```

The interesting feature is that we can match against these
constructor-constants. Using the `cases` function we can pick a pattern
that matches a value.

``` r
elements <- list(ONE, TWO, THREE)
for (elm in elements) {
    value <- cases(elm,
                   ONE -> 1,
                   TWO -> 2,
                   THREE -> 3)
    cat("Element", toString(elm), "maps to value", toString(value), "\n")
}
#> Element ONE maps to value 1 
#> Element TWO maps to value 2 
#> Element THREE maps to value 3
```

The `cases` function works by matching its first element–which should be
a value constructed as a type we have defined with `:=`–against a list
of patterns and what `cases` should return if a pattern matches. The
pattern arguments are on the form `pattern -> expression`. The value is
matched against the patterns in turn and the first pattern that matches
will be chosen. The expression to the right of the arrow is then
evaluated and the result is returned.

The patterns do not need to be literal constants. You can also use
variables. These will be bound to the matching value and the expression
that is evaluated will see such variables bound.

``` r
elements <- list(ONE, TWO, THREE)
for (elm in elements) {
    value <- cases(elm,
                   ONE -> 1,
                   v -> v)
    cat("Element", toString(elm), "maps to value", toString(value), "\n")
}
#> Element ONE maps to value 1 
#> Element TWO maps to value TWO 
#> Element THREE maps to value THREE
```

It gets more interesting when we move beyond constants. The `:=`
operator also allows you to define function-constructors. These are
written simply as you would write a function call, but the variables are
interpreted as parameters of the constructor. For example, we could
define:

``` r
zero_one_two_three := ZERO | ONE(x) | TWO(x,y) | THREE(x,y,z)
```

The first constructor, `ZERO`, is just a constant as before, but the
other three takes arguments.

``` r
ONE(1)
#> ONE(x = 1)
TWO(1,2)
#> TWO(x = 1, y = 2)
THREE(1,2,3)
#> THREE(x = 1, y = 2, z = 3)
```

When we use `cases` to match against such patterns, we can bind
variables to the values they contain.

``` r
f <- function(v) {
    cases(v,
          ZERO         -> 0,
          ONE(x)       -> x,
          TWO(x,y)     -> x + y,
          THREE(x,y,z) -> x + y + z)
}

f(ZERO)
#> [1] 0
f(ONE(1))
#> [1] 1
f(TWO(1,2))
#> [1] 3
f(THREE(1,2,3))
#> [1] 6
```

You can nest these patterns to match on more complex values

``` r
f <- function(v) {
    cases(v,
          ZERO               -> 0,
          ONE(x)             -> x,
          TWO(ONE(x),ONE(y)) -> x + y + 42,
          TWO(x,y)           -> x + y,
          THREE(x,y,z)       -> x + y + z)
}

f(TWO(ONE(10),ONE(-10)))
#> [1] 42
```

You have to be careful with the order of expressions, though. If we
flipped the two `TWO` patterns, the first one, `TWO(x,y)` would match
first and we would be trying to add together `ONE(10)` and `ONE(-10)`,
which would result in an error since we do not have an addition operator
defined on these types.

You can match any pattern using the bare keyword `otherwise`:

``` r
cases(42,
      1 -> 1,
      13 -> 13,
      otherwise -> 24)
#> [1] 24
```

When you define function constructors, you can give the arguments types.
You do this by adding `:` and a type name to the argument. For example,
we could define

``` r
one_or_two := ONE(x : numeric) | TWO(x : numeric, y : numeric)
```

We would now get an error if the arguments we provide to the
constructors were not `numeric`:

``` r
ONE(1)
#> ONE(x = 1)
ONE("foo")
#> Error: rlang::is_na(type) || inherits(arg, type) is not TRUE
```

Constructors and pattern matching becomes even more powerful when you
start to define recursive data structures. You can, for example, define
a binary tree like this:

``` r
tree := L(elm : numeric) | T(left : tree, right : tree)
```

A very succinct depth first traversal that collects the leaves of such a
tree can be written like this:

``` r
f <- function(x) {
    cases(x, 
          L(v) -> v, 
          T(left,right) -> c(f(left), f(right)))
}
x <- T(T(L(1),L(2)), T(T(L(3),L(4)),L(5)))
f(x)
#> [1] 1 2 3 4 5
```

For more examples, see below.

## Installation

You can install pmatch from github with:

``` r
# install.packages("devtools")
devtools::install_github("mailund/pmatch")
```

## Examples

To run the examples below, you will need to use the `magrittr` package
for the pipe operator, `%>%`.

``` r
library(magrittr)
```

### Linked lists

``` r
linked_list := NIL | CONS(car, cdr : linked_list)

lst <- CONS(1, CONS(2, CONS(3, NIL)))
```

``` r
reverse_list <- function(lst, acc = NIL) {
  force(acc)
  cases(lst,
        NIL -> acc,
        CONS(car, cdr) -> reverse_list(cdr, CONS(car, acc)))
}

list_length <- function(lst, acc = 0) {
  force(acc)
  cases(lst,
        NIL -> acc,
        CONS(car, cdr) -> list_length(cdr, acc + 1))
}

list_to_vector <- function(lst) {
  n <- list_length(lst)
  v <- vector("list", length = n)
  f <- function(lst, i) {
    force(i)
    cases(lst,
          NIL -> NULL,
          CONS(car, cdr) -> {
            v[[i]] <<- car
            f(cdr, i + 1)
            }
          )
  }
  f(lst, 1)
  v %>% unlist
}

vector_to_list <- function(vec) {
  lst <- NIL
  for (i in seq_along(vec)) {
    lst <- CONS(vec[[i]], lst)
  }
  reverse_list(lst)
}


lst <- vector_to_list(1:5)
list_length(lst)
#> [1] 5
list_to_vector(lst)
#> [1] 1 2 3 4 5
lst %>% reverse_list %>% list_to_vector
#> [1] 5 4 3 2 1
```

### Search trees

``` r
search_tree := E | T(left : search_tree, value, right : search_tree)

insert <- function(tree, x) {
  cases(tree,
        E -> T(E, x, E),
        T(left, val, right) ->
          if (x < val)
            T(insert(left, x), val, right)
          else if (x > val)
            T(left, val, insert(right, x))
          else
            T(left, x, right)
        )
}

member <- function(tree, x) {
  cases(tree,
        E -> FALSE,
        T(left, val, right) -> {
          if (x < val) member(left, x)
          else if (x > val) member(right, x)
          else TRUE
        })
}

tree <- E
for (i in sample(2:4))
  tree <- insert(tree, i)

for (i in 1:6) {
  cat(i, " : ", member(tree, i), "\n")
}
#> 1  :  FALSE 
#> 2  :  TRUE 
#> 3  :  TRUE 
#> 4  :  TRUE 
#> 5  :  FALSE 
#> 6  :  FALSE
```

### Red-black search trees

``` r
colour := R | B
rb_tree := E | T(col : colour, left : rb_tree, value, right : rb_tree)


member <- function(tree, x) {
  cases(tree,
        E -> FALSE,
        T(col, left, val, right) -> {
          if (x < val) member(left, x)
          else if (x > val) member(right, x)
          else TRUE
        })
}

tree <- T(R, E, 2, T(B, E, 5, E))
for (i in 1:6) {
  cat(i, " : ", member(tree, i), "\n")
}
#> 1  :  FALSE 
#> 2  :  TRUE 
#> 3  :  FALSE 
#> 4  :  FALSE 
#> 5  :  TRUE 
#> 6  :  FALSE

insert_rec <- function(tree, x) {
  cases(tree,
        E -> T(R, E, x, E),
        T(col, left, val, right) -> {
          if (x < val)
            balance(T(col, insert_rec(left, x), val, right))
          else if (x > val)
            balance(T(col, left, val, insert_rec(right, x)))
          else
            T(col, left, x, right) # already here
        })
}
insert <- function(tree, x) {
  tree <- insert_rec(tree, x)
  tree$col <- B
  tree
}

balance <- function(tree) {
  cases(tree,
        T(B, T(R, a, x, T(R, b, y, c)), z, d) -> T(R, T(B,a,x,b), y, T(B,c,z,d)),
        T(B, T(R, T(R, a, x, b), y, c), z, d) -> T(R, T(B,a,x,b), y, T(B,c,z,d)),
        T(B, a, x, T(R, b, y, T(R, c, z, d))) -> T(R, T(B,a,x,b), y, T(B,c,z,d)),
        T(B, a, x, T(R, T(R, b, y, c), z, d)) -> T(R, T(B,a,x,b), y, T(B,c,z,d)),
        otherwise -> tree)
}

tree <- E
for (i in sample(2:4))
  tree <- insert(tree, i)
for (i in 1:6) {
  cat(i, " : ", member(tree, i), "\n")
}
#> 1  :  FALSE 
#> 2  :  TRUE 
#> 3  :  TRUE 
#> 4  :  TRUE 
#> 5  :  FALSE 
#> 6  :  FALSE
```
