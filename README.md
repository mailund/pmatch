
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Haskell-like pattern matching in R

[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![lifecycle](http://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Project Status:
Active](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Last-changedate](https://img.shields.io/badge/last%20change-2018--10--19-green.svg)](/commits/master)
[![packageversion](https://img.shields.io/badge/Package%20version-0.1.3.9003-orange.svg?style=flat-square)](commits/master)
[![Travis-CI Build
Status](http://travis-ci.org/mailund/pmatch.svg?branch=master)](https://travis-ci.org/mailund/pmatch)
[![AppVeyor Build
Status](http://ci.appveyor.com/api/projects/status/wvyqe7bfp4a2rm77?svg=true)](https://ci.appveyor.com/project/mailund/pmatch)
[![Coverage
Status](http://img.shields.io/codecov/c/github/mailund/pmatch/master.svg)](https://codecov.io/github/mailund/pmatch?branch=master)
[![Coverage
Status](http://coveralls.io/repos/github/mailund/pmatch/badge.svg?branch=master)](https://coveralls.io/github/mailund/pmatch?branch=master)
[![CRAN
status](http://www.r-pkg.org/badges/version/pmatch)](https://cran.r-project.org/package=pmatch)
[![CRAN
downloads](http://cranlogs.r-pkg.org/badges/grand-total/pmatch)](https://cran.r-project.org/package=pmatch)
[![minimal R
version](https://img.shields.io/badge/R-%E2%89%A53.3-blue.svg)](https://cran.r-project.org/)

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

The `cases` function can match any pattern since it has a generic
algorithm for this. This makes the function slow. Intead of using
`cases` you can create a function that only checks the patterns you
provide it at creating time. For this, you use the `case_func` function.

``` r
g <- case_func(
    ZERO               -> 0,
    ONE(x)             -> x,
    TWO(ONE(x),ONE(y)) -> x + y + 42,
    TWO(x,y)           -> x + y,
    THREE(x,y,z)       -> x + y + z
)

microbenchmark::microbenchmark(
    f(TWO(ONE(10),ONE(-10))),
    g(TWO(ONE(10),ONE(-10)))
)
#> Unit: microseconds
#>                       expr     min       lq      mean  median      uq
#>  f(TWO(ONE(10), ONE(-10))) 812.583 864.5305 1413.7400 913.962 987.142
#>  g(TWO(ONE(10), ONE(-10)))   9.386  12.2300  123.1576  16.962  19.426
#>       max neval
#>  43292.57   100
#>  10681.78   100
```

A variable will match any pattern, so you can use one as a default case.
I prefer to use the `.` variable.

``` r
cases(42, 1 -> 1, 13 -> 13, . -> 24)
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
#> Error in ONE(x = "foo"): The argument x should be of type numeric.
```

Constructors and pattern matching becomes even more powerful when you
start to define recursive data structures. You can, for example, define
a binary tree like this:

``` r
tree := L(elm : numeric) | T(left : tree, right : tree)
```

You can then write a very succinct depth first traversal that collects
the leaves of such a tree like this:

``` r
f <- case_func(
    L(v) -> v, 
    T(left,right) -> c(f(left), f(right))
)
x <- T(T(L(1),L(2)), T(T(L(3),L(4)),L(5)))
f(x)
#> [1] 1 2 3 4 5
```

### Binding local variables

You can also assign to variables in the current namespace using
subscripting on the special object `bind`:

``` r
bind[a,b] <- 1:2
a
#> [1] 1
b
#> [1] 2
```

With the `bind` object, you can match against patterns, as with the
`cases` function, and matched variables will be added to the namespace
where you invoke subscripting on `bind`:

``` r
x
#> T(left = T(left = L(elm = 1), right = L(elm = 2)), right = T(left = T(left = L(elm = 3), right = L(elm = 4)), right = L(elm = 5)))
bind[T(left, right)] <- x
left
#> T(left = L(elm = 1), right = L(elm = 2))
right
#> T(left = T(left = L(elm = 3), right = L(elm = 4)), right = L(elm = 5))
```

### Testing more than one pattern

If you want to test more than one pattern at a time, you can use the
`..` function:

``` r
f <- function(t1,t2) 
    cases(..(t1, t2),
          ..(L(.),L(.)) -> "Both leaves",
          ..(L(.),.)    -> "Left leaf",
          ..(.,L(.))    -> "Right leaf",
          ..(.,.)       -> "None are leaves")

f(L(0), L(2))
#> [1] "Both leaves"
f(L(0), x)
#> [1] "Left leaf"
f(x, L(42))
#> [1] "Right leaf"
f(x, x)
#> [1] "None are leaves"
```

This will not work with functions created with `case_func`. Handling an
arbitrary number of patterns requires checks are runtime, and
`case_func` creates a function with specialised pattern checking at
creation time.

You can easily define data types to get similar behaviour, though:

``` r
tuples := ..(first, second) | ...(first, second, third)
f <- case_func(..(.,.) -> 2, ...(.,.,.) -> 3)
f(..(1, 2))
#> [1] 2
f(...(1, 2, 3))
#> [1] 3
```

### Function transformations

Since this package provides its functionality through a DSL, you *can*
get into trouble when other parts of R tries to make sense of it.

Consider this function:

``` r
is_leaf <- function(tree) {
    cases(tree,
          L(x) -> TRUE,
          otherwise -> FALSE)
}
is_leaf(L(1))
#> [1] TRUE
is_leaf(T(L(1),L(2)))
#> [1] FALSE
```

It works as intended, but if you try to byte-compile it, you get an
error:

``` r
compiler::cmpfun(is_leaf)
#> Error: bad assignment: 'TRUE <- L(x)'
```

This is because the byte-compiler tries to make meaning out of the `->
TRUE` and `-> FALSE` assignments (it sees them as `TRUE <-` and `FALSE
<-` because that is what the parser actually returns).

There are two ways around this problem; one is easy and just requires a
different syntax in the call to `cases`, the other is *almost* as easy,
but requires that we transform the function.

First, we can get around the issue using a formula syntax instead of an
assignment syntax. Instead of the `is_leaf` above, we can define

``` r
other_is_leaf <- function(tree) {
    cases(tree,
          L(x) ~ TRUE,
          otherwise ~ FALSE)
}
```

This function, you can byte compile

``` r
compiler::cmpfun(other_is_leaf)
#> function(tree) {
#>     cases(tree,
#>           L(x) ~ TRUE,
#>           otherwise ~ FALSE)
#> }
#> <bytecode: 0x7f85dd540840>
```

The `pmatch` package makes no distinction betwen the `~` or the `->`
syntax; you can use them interchangeable.

Alternatively, we can transform the body of functions that calls `cases`
to replace the DSL with `if`-statements.

``` r
is_leaf_tr <- transform_cases_function(is_leaf)
is_leaf_tr
#> function (tree) 
#> {
#>     if (!rlang::is_null(..match_env <- pmatch::test_pattern(tree, 
#>         L(x)))) 
#>         with(..match_env, TRUE)
#>     else if (!rlang::is_null(..match_env <- pmatch::test_pattern(tree, 
#>         otherwise))) 
#>         with(..match_env, FALSE)
#> }
```

After the transformation, the byte-compiler is happy.

``` r
is_leaf_tr_bc <- compiler::cmpfun(is_leaf_tr)
is_leaf_tr_bc(L(1))
#> [1] TRUE
is_leaf_tr_bc(T(L(1),L(2)))
#> [1] FALSE
```

Transformation has the added benefit of slightly speeding up the
function. And, of course, being able to byte-compile can add a bit more
to the performance.

``` r
microbenchmark::microbenchmark(
    is_leaf(L(1)), is_leaf_tr(L(1)), is_leaf_tr_bc(L(1))
)
#> Unit: microseconds
#>                 expr     min       lq     mean   median      uq      max
#>        is_leaf(L(1)) 264.228 275.4965 314.7998 290.6590 305.721 2134.119
#>     is_leaf_tr(L(1)) 204.092 214.2180 289.6069 224.9885 234.918 3765.645
#>  is_leaf_tr_bc(L(1)) 205.923 212.4610 229.7888 221.8180 237.637  370.995
#>  neval
#>    100
#>    100
#>    100
```

For more examples, see below.

## Installation

You can install the stable version of pmatch from CRAN using

``` r
install.packages("pmatch")
```

You can install the development version pmatch from github with:

``` r
# install.packages("devtools")
devtools::install_github("mailund/pmatch")
```

## Examples

To show how the `pmatch` package can be used, I will use three data
structures that I have implemented without `pmatch` in my book on
[*Functional Data Structures in R*](http://amzn.to/2Eb4RKK): linked
lists, plain search trees, and red-black search trees.

To run the examples below, you will need to use the `magrittr` package
for the pipe operator, `%>%`.

``` r
library(magrittr)
```

### Linked lists

The `list` type in R is allocated to have a certain size when it is
created, and changing the size of `list` objects involve creating a new
object and moving all the elements from the old object to the new. This
is a linear time operation, so growing lists usually lead to quadratic
running times. With linked lists, on the other hand, you can prepend
elements in constant time–at the cost of linear time random access.

You can implement a linked list using `list` objects. You simply
construct a list that contains two elements, the head of the linked
lists–traditionally called `car`–and the tail of the list–another linked
list, traditionally named `cdr`. You need a special representation for
empty lists, and a natural choice is `NULL`. With `pmatch` we will use a
constant instead, though, so we can pattern match on empty lists.

We can define a linked list using the `pmatch` syntax like this:

``` r
linked_list := NIL | CONS(car, cdr : linked_list)
lst <- CONS(1, CONS(2, CONS(3, NIL)))
```

Although R doesn’t implement tail recursion optimization, habit forces
me to write tail recursive functions. For list functions, this usually
means providing an accumulator parameter. Other than that, recursive
functions operating on linked lists should simply match on `NIL` and
`CONS` patterns. Two examples could be computing the length of a list
and reversing a list:

``` r
list_length <- case_func(acc = 0,
        NIL -> acc,
        CONS(car, cdr) -> list_length(cdr, acc + 1)
)

list_length(lst)
#> [1] 3

reverse_list <- case_func(acc = NIL,
        NIL -> acc,
        CONS(car, cdr) -> reverse_list(cdr, CONS(car, acc))
)

reverse_list(lst)
#> CONS(car = 3, cdr = CONS(car = 2, cdr = CONS(car = 1, cdr = NIL)))
```

Translating to and from vectors/`list` objects is relatively simple. To
go from a vector to a linked list, we use `NIL` and `CONS`, and to go
the other direction we use pattern
matching:

``` r
vector_to_list <- function(vec) purrr::reduce_right(vec, ~ CONS(.y, .x), .init = NIL)

list_to_vector <- function(lst) {
  n <- list_length(lst)
  v <- vector("list", length = n)
  f <- case_func(i,
          NIL -> NULL,
          CONS(car, cdr) -> {
            v[[i]] <<- car
            f(cdr, i + 1)
            }
  )
  f(lst, 1)
  v %>% unlist
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

Search trees are binary trees that holds values in all inner nodes and
satisfy the invariant that all values in a left subtree are smaller than
the value in an inner node, and all values in the right subtree are
larger.

We can define a search tree like this:

``` r
search_tree := E | T(left : search_tree, value, right : search_tree)
```

Here, we use an empty tree, `E`, for leaves. We only store values in
inner nodes, created with the constructor `T`.

``` r
tree <- T(T(E,1,E), 3, T(E,4,E))
tree
#> T(left = T(left = E, value = 1, right = E), value = 3, right = T(left = E, value = 4, right = E))
```

Because of the invariant, we know where values should be found if they
are in a tree. We can look at the value in the root of a subtree. If it
is larger than the value we are searching for, we need to search to the
left. If it is smaller, we need to search to the right. Otherwise, it
must be equal to the value. If we reach an empty tree in this search,
then we know the value is no the tree.

``` r
member <- case_func(x,
        E -> FALSE,
        T(left, val, right) -> {
          if (x < val) member(left, x)
          else if (x > val) member(right, x)
          else TRUE
        }
)
member(tree, 0)
#> [1] FALSE
member(tree, 1)
#> [1] TRUE
member(tree, 2)
#> [1] FALSE
member(tree, 3)
#> [1] TRUE
member(tree, 4)
#> [1] TRUE
```

Since data in R, in general, are immutable, we cannot update search
trees. We can, however, create copies with updated structure, and
because R implements “copy-on-write”, this is an efficient way of
updating the structure of data we work on. If we insert elements into a
search tree, what we will really be doing is to create a new tree that
holds all the values the old tree held plus the new values. If the value
is already in the old tree we do not add it again, but we will be
returning a new tree. We create the new tree in a recursion. Whenever we
call recursively, we create a new inner node that will contain one
subtree that is an exact copy of one of the subtrees from the old
tree–shared with the old tree so no actual copying takes place–and one
subtree that is created in the recursive insertion. The recursion goes
left or right using the same logic as in the `member` function. If we
find that the element is already in the tree, we terminate the recursion
with the tree that contains the value. If we reach an empty tree, the
element was not in the old tree, but we have found the place where it
should be in the new tree, so we create an inner tree with two empty
subtrees and the value.

``` r
insert <- case_func(x,
        E -> T(E, x, E),
        T(left, val, right) ->
          if (x < val)
            T(insert(left, x), val, right)
          else if (x > val)
            T(left, val, insert(right, x))
          else
            T(left, x, right)
)

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

The worst-case time usage for both of these functions is proportional to
the depth of the tree, and that can be linear in the number of elements
stored in the tree. If we keep the tree balanced, though, the time is
reduced to logarithmic in the size of the tree. A classical data
structure for keeping search trees balanced is so-called *red-black*
search trees. Implementing these using pointer or reference manipulation
in languages such as C/C++ or Java can be quite challenging, but in a
functional language, balancing such trees is a simple matter of
transforming trees based on local structure.

Red-black search trees are binary search trees where each tree has a
colour associated, either red or black. We can define colours using
constant constructors and define a red-black search tree by extending
the plain search tree:

``` r
colour := R | B
rb_tree := E | T(col : colour, left : rb_tree, value, right : rb_tree)
```

Except for including the colour in the pattern matching, the `member`
function for this data structure is the same as for the plain search
tree.

``` r
member <- case_func(x,
        E -> FALSE,
        T(col, left, val, right) -> {
          if (x < val) member(left, x)
          else if (x > val) member(right, x)
          else TRUE
        }
)

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
```

Red-black search trees are kept balanced because we enforce these two
invariants:

1.  No red node has a red parent.
2.  Every path from the root to a leaf has the same number of black
    nodes.

If every path from root to a leaf has the same number of black nodes,
then the tree is perfectly balanced if we ignored the red nodes. Since
no red node has a red parent, the longest path, when red nodes are
considered, can be no longer than twice the length of the shortest path.

These invariants can be guaranteed by always inserting new values in red
leaves, potentially invalidating the first invariant, and then
rebalancing all sub-trees that invalidate this invariant, and at the end
setting the root to be black. The rebalancing is done when returning
from the recursive insertion calls that otherwise work as insertion in
the plain search tree.

``` r
insert_rec <- case_func(x,
        E -> T(R, E, x, E),
        T(col, left, val, right) -> {
          if (x < val)
            balance(T(col, insert_rec(left, x), val, right))
          else if (x > val)
            balance(T(col, left, val, insert_rec(right, x)))
          else
            T(col, left, x, right) # already here
        }
)
insert <- function(tree, x) {
  tree <- insert_rec(tree, x)
  tree$col <- B
  tree
}
```

The transformation rules for the `balance` function are shown in the
figure below:

![](http://users-cs.au.dk/mailund/RBT-transformations.png)

Every time we see one of the trees around the edges, we must transform
it into the tree in the middle. We can implement these transformations
as simple as this:

``` r
balance <- function(tree) {
  cases(tree,
        T(B,T(R,a,x,T(R,b,y,c)),z,d) -> T(R,T(B,a,x,b),y,T(B,c,z,d)),
        T(B,T(R,T(R,a,x,b),y,c),z,d) -> T(R,T(B,a,x,b),y,T(B,c,z,d)),
        T(B,a,x,T(R,b,y,T(R,c,z,d))) -> T(R,T(B,a,x,b),y,T(B,c,z,d)),
        T(B,a,x,T(R,T(R,b,y,c),z,d)) -> T(R,T(B,a,x,b),y,T(B,c,z,d)),
        otherwise -> tree)
}
```
