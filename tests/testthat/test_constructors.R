context("constructors")

test_that("We can create constant constructors", {
    type := A | B

    expect_true(exists("A", environment()))
    expect_true(inherits(A, "type"))
    expect_true(exists("B", environment()))
    expect_true(inherits(B, "type"))

    expect_false(rlang::is_null(attr(A, "constructor_constant")))
    expect_equal(attr(A, "constructor_constant"), "A")
})

test_that("We can create function constructors", {
    linked_list := NIL | CONS(car, cdr : linked_list)

    expect_true(exists("NIL", environment()))
    expect_true(inherits(NIL, "linked_list"))

    expect_true(exists("CONS", environment()))
    expect_true(inherits(CONS, "constructor"))

    expect_s3_class(NIL, "linked_list")
    expect_s3_class(CONS(1, NIL), "linked_list")
    expect_s3_class(CONS(1, CONS(2, NIL)), "linked_list")
    expect_error(CONS(1,2))
})

test_that("We can create print constructed values", {
    linked_list := NIL | CONS(car, cdr : linked_list)

    expect_equal(toString(NIL), "NIL")
    expect_equal(toString(CONS(1,NIL)), "CONS(car = 1, cdr = NIL)")

    expect_output(print(NIL), "NIL")
    expect_output(print(CONS(1,NIL)), "CONS\\(car = 1, cdr = NIL\\)")
})
