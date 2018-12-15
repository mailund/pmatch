## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(pmatch)

## ------------------------------------------------------------------------
classical_list <- list(1, 2, 3)

## ------------------------------------------------------------------------
another_classical_list <- c(0, classical_list)

## ------------------------------------------------------------------------
cons <- function(head, tail) {
    list(head = head, tail = tail)
}

a_list <- cons(1, cons(2, cons(3, NULL)))
a_list

## ------------------------------------------------------------------------
another_list <- cons(0, a_list)

## ------------------------------------------------------------------------
list_length <- function(the_list) {
    n <- 0
    while (!is.null(the_list)) {
        n <- n + 1
        the_list <- the_list$tail
    }
    n
}

list_length(a_list)
list_length(another_list)

## ------------------------------------------------------------------------
rec_list_length <- function(the_list) {
    if (is.null(the_list)) 0
    else 1 + list_length(the_list$tail)
}
rec_list_length(a_list)
rec_list_length(another_list)

## ------------------------------------------------------------------------
linked_list := NIL | CONS(car, cdr : linked_list)

## ------------------------------------------------------------------------
a_list <- CONS(1, CONS(2, CONS(3, NIL)))
a_list

## ------------------------------------------------------------------------
list_length <- function(the_list)
    cases(the_list,
          NIL -> 0,
          CONS(car, cdr) -> 1 + list_length(cdr))

list_length(a_list)

## ------------------------------------------------------------------------
list_sum <- function(the_list)
    cases(the_list,
          NIL -> 0,
          CONS(car, cdr) -> car + list_sum(cdr))

list_sum(a_list)

## ------------------------------------------------------------------------
colours := RED | GREEN | BLUE
counts := ZERO | ONE | MANY

## ------------------------------------------------------------------------
linked_list := NIL | CONS(car, cdr : linked_list)
tree := Leaf | Tree(left : tree, val, right : tree)

## ------------------------------------------------------------------------
f <- function(tree) {
    cases(tree,
          Leaf -> "leaf",
          Tree(left, 0, right) -> "zero",
          Tree(left, v, right) -> v)
}
f(Leaf)
f(Tree(Leaf,0,Leaf))
f(Tree(Tree(Leaf,0,Leaf), 42, Tree(Leaf, 1, Leaf)))
f(Tree(Tree(Leaf,0,Leaf), 1984, Tree(Leaf, 1, Leaf)))

## ------------------------------------------------------------------------
important <- 42
f <- function(tree) {
    cases(tree,
          Leaf -> "leaf",
          Tree(left, 0, right) -> "zero",
          Tree(left, !!important, right) -> "The meaning of life!",
          Tree(left, v, right) -> v)
}
f(Leaf)
f(Tree(Leaf,0,Leaf))
f(Tree(Tree(Leaf,0,Leaf), 42, Tree(Leaf, 1, Leaf)))
f(Tree(Tree(Leaf,0,Leaf), 1984, Tree(Leaf, 1, Leaf)))

