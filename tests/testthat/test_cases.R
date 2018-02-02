context("cases")

test_that("We can match on constant constructors", {
    type := ONE | TWO | THREE
    f <- function(x) cases(x, ONE -> 1, TWO -> 2, THREE -> 3)
    expect_equal(f(ONE), 1)
    expect_equal(f(TWO), 2)
    expect_equal(f(THREE), 3)
    expect_error(f("foo"))
})

test_that("We can create match on function constructors", {

    linked_list := NIL | CONS(car, cdr : linked_list)

    f <- function(lst) {
        cases(lst,
              NIL                         -> 0,
              CONS(x,NIL)                 -> 1,
              CONS(x,CONS(y,NIL))         -> 2,
              CONS(x,CONS(y,CONS(z,NIL))) -> 3,
              otherwise -> 42)
    }

    expect_equal(f(NIL), 0)
    expect_equal(f(CONS(1,NIL)), 1)
    expect_equal(f(CONS(1,CONS(2,NIL))), 2)
    expect_equal(f(CONS(1,CONS(2,CONS(3, NIL)))), 3)
    expect_equal(f(CONS(1,CONS(2,CONS(3, CONS(4, NIL))))), 42)
})

test_that("We can create match constants on function constructors", {

    linked_list := NIL | CONS(car, cdr : linked_list)

    f <- function(lst) {
        cases(lst,
              NIL         -> 0,
              CONS(1,NIL) -> 11,
              CONS(2,NIL) -> 22,
              CONS(x,NIL) -> x,
              otherwise   -> 42)
    }

    expect_equal(f(NIL), 0)
    expect_equal(f(CONS(1,NIL)), 11)
    expect_equal(f(CONS(2,NIL)), 22)
    expect_equal(f(CONS(3,NIL)), 3)
    expect_equal(f(CONS(1,CONS(2,NIL))), 42)
    expect_equal(f(CONS(1,CONS(2,CONS(3, NIL)))), 42)
    expect_equal(f(CONS(1,CONS(2,CONS(3, CONS(4, NIL))))), 42)
})

test_that("We can create match variables in function constructors", {

    linked_list := NIL | CONS(car, cdr : linked_list)

    f <- function(lst) {
        cases(lst,
              NIL                         -> 0,
              CONS(x,NIL)                 -> x,
              CONS(x,CONS(y,NIL))         -> x + y,
              CONS(x,CONS(y,CONS(z,NIL))) -> x + y + z,
              otherwise -> 42)
    }

    expect_equal(f(CONS(1,NIL)), 1)
    expect_equal(f(CONS(11,NIL)), 11)
    expect_equal(f(CONS(1,CONS(2,NIL))), 1 + 2)
    expect_equal(f(CONS(1,CONS(2,CONS(3,NIL)))), 1 + 2 + 3)
})
