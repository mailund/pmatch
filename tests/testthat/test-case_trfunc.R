context("Tail recursive case functions")

test_that("We can match on constant constructors", {
    type := ONE | TWO | THREE
    f <- case_trfunc(ONE -> 1, TWO -> 2, THREE -> 3)
    expect_equal(f(ONE), 1)
    expect_equal(f(TWO), 2)
    expect_equal(f(THREE), 3)
    expect_error(f("foo"))

    one <- ONE
    two <- TWO

    expect_null(test_pattern(one, TWO))
    expect_null(test_pattern_(one, quote(TWO)))
    expect_true(!rlang::is_null(test_pattern(two, TWO)))
    expect_true(!rlang::is_null(test_pattern_(two, quote(TWO))))
})

test_that("We can create match on function constructors", {
    linked_list := NIL | CONS(car, cdr:linked_list)

    f <- case_trfunc(
        NIL -> 0,
        CONS(x, NIL) -> 1,
        CONS(x, CONS(y, NIL)) -> 2,
        CONS(x, CONS(y, CONS(z, NIL))) -> 3,
        otherwise -> 42
    )

    expect_equal(f(NIL), 0)
    expect_equal(f(CONS(1, NIL)), 1)
    expect_equal(f(CONS(1, CONS(2, NIL))), 2)
    expect_equal(f(CONS(1, CONS(2, CONS(3, NIL)))), 3)
    expect_equal(f(CONS(1, CONS(2, CONS(3, CONS(4, NIL))))), 42)
})

test_that("We can formula as well as assignment syntax", {
    type := ONE | TWO | THREE
    f <- case_trfunc(ONE ~ 1, TWO ~ 2, THREE ~ 3)
    expect_equal(f(ONE), 1)
    expect_equal(f(TWO), 2)
    expect_equal(f(THREE), 3)
    expect_error(f("foo"))

    one <- ONE
    two <- TWO

    expect_null(test_pattern(one, TWO))
    expect_null(test_pattern_(one, quote(TWO)))
    expect_true(!rlang::is_null(test_pattern(two, TWO)))
    expect_true(!rlang::is_null(test_pattern_(two, quote(TWO))))

    linked_list := NIL | CONS(car, cdr:linked_list)

    f <- case_trfunc(
        NIL ~ 0,
        CONS(x, NIL) ~ 1,
        CONS(x, CONS(y, NIL)) ~ 2,
        CONS(x, CONS(y, CONS(z, NIL))) ~ 3,
        otherwise ~ 42
    )

    expect_equal(f(NIL), 0)
    expect_equal(f(CONS(1, NIL)), 1)
    expect_equal(f(CONS(1, CONS(2, NIL))), 2)
    expect_equal(f(CONS(1, CONS(2, CONS(3, NIL)))), 3)
    expect_equal(f(CONS(1, CONS(2, CONS(3, CONS(4, NIL))))), 42)

    tree := T(left:tree, right:tree) | L(value)
    f <- case_trfunc(
        L(v) ~ v,
        T(L(v), L(w)) ~ v + w,
        otherwise ~ 5
    )
    res <- f(T(L(4), L(5)))
    expect_equal(res, 9)
})


test_that("We can create match constants on function constructors", {
    linked_list := NIL | CONS(car, cdr:linked_list)

    f <- case_trfunc(
        NIL -> 0,
        CONS(1, NIL) -> 11,
        CONS(2, NIL) -> 22,
        CONS(x, NIL) -> x,
        otherwise -> 42
    )

    expect_equal(f(NIL), 0)
    expect_equal(f(CONS(1, NIL)), 11)
    expect_equal(f(CONS(2, NIL)), 22)
    expect_equal(f(CONS(3, NIL)), 3)
    expect_equal(f(CONS(1, CONS(2, NIL))), 42)
    expect_equal(f(CONS(1, CONS(2, CONS(3, NIL)))), 42)
    expect_equal(f(CONS(1, CONS(2, CONS(3, CONS(4, NIL))))), 42)
})


test_that("We can create match variables in function constructors", {
    linked_list := NIL | CONS(car, cdr:linked_list)

    f <- case_trfunc(
        NIL -> 0,
        CONS(x, NIL) -> x,
        CONS(x, CONS(y, NIL)) -> x + y,
        CONS(x, CONS(y, CONS(z, NIL))) -> x + y + z,
        otherwise -> 42
    )

    expect_equal(f(CONS(1, NIL)), 1)
    expect_equal(f(CONS(11, NIL)), 11)
    expect_equal(f(CONS(1, CONS(2, NIL))), 1 + 2)
    expect_equal(f(CONS(1, CONS(2, CONS(3, NIL)))), 1 + 2 + 3)
})


test_that("We can distinguish between constructors", {
    type := ONE(val) | TWO(val)

    f <- case_trfunc(ONE(v) -> 1, TWO(v) -> 2)

    expect_equal(f(ONE(1)), 1)
    expect_equal(f(TWO(1)), 2)
})


test_that("We do not confuse variables for constructors", {
    type := ONE | TWO(x)

    one <- ONE
    two <- TWO(2)

    f <- case_trfunc(ONE -> 1, TWO(two) -> two)
    expect_equal(f(ONE), 1)

    f <- case_trfunc(ONE -> 1, TWO(two) -> two)
    expect_equal(f(TWO(3)), 3)

    f <- case_trfunc(ONE -> 1, x -> x)
    expect_equal(f(TWO(3)), TWO(3))

    f <- case_trfunc(ONE -> 1, two -> two)
    expect_equal(f(TWO(3)), TWO(3))

    f <- case_trfunc(ONE -> 1, two -> two)
    expect_equal(f(one), 1)

    f <- case_trfunc(ONE -> 1, two -> two)
    expect_equal(f(two), two)

    f <- case_trfunc(one -> one)
    expect_equal(f(one), one)

    f <- case_trfunc(two -> two)
    expect_equal(f(one), one)
})


test_that("We can do quasi-quoting", {
    type := ONE | TWO(x)
    two_q <- rlang::expr(TWO(2))

    f <- case_trfunc(ONE -> 1, !!two_q -> TWO(2))
    expect_equal(f(TWO(2)), TWO(2))
})


test_that("We can add arguments to a case_func", {
    f <- case_trfunc(default, . -> default)
    expect_equal(f("foo", "bar"), "bar")

    f <- case_trfunc(default = "qux", . -> default)
    expect_equal(f("foo", "bar"), "bar")
    expect_equal(f("foo"), "qux")

    x <- 42
    f <- case_trfunc(x, . -> x)
    expect_equal(f("foo", "bar"), "bar")

    f <- case_trfunc(y = x, . -> y)
    expect_equal(f("foo", "bar"), "bar")
})

test_that("We get the additional arguments in the right order in case_func", {
    f <- case_trfunc(
        a, b, c,
        . -> 42
    )
    expect_equal(
        c(".match_expr", "a", "b", "c"),
        names(formals(f)),
    )
})





test_that("We handle recursion", {
    llist := NIL | CONS(car, cdr:llist)

    lst <- CONS(1, CONS(2, CONS(3, NIL)))

    # tail recursive
    llength <- case_trfunc(
        acc = 0,
        NIL -> acc,
        CONS(., cdr) -> Recall(cdr, acc + 1)
    )

    lsum <- case_trfunc(
        acc = 0,
        NIL -> acc,
        CONS(car, cdr) -> Recall(cdr, acc + car)
    )
    expect_equal(llength(lst), 3)
    expect_equal(lsum(lst), 6)

    # not tail recursive -- these still work, they just do not have
    # the optimisation
    llength <- case_trfunc(
        NIL -> 0,
        CONS(., cdr) -> 1 + Recall(cdr)
    )

    lsum <- case_trfunc(
        NIL -> 0,
        CONS(car, cdr) -> car + Recall(cdr)
    )
    expect_equal(llength(lst), 3)
    expect_equal(lsum(lst), 6)
})
