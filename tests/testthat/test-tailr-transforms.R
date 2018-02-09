context("Transformations for the tailr package")

test_that("we can transform a function that contains a call to cases", {
    if (!requireNamespace("tailr", quietly = TRUE))
        skip("tailr not installed")

    llist := NIL | CONS(car, cdr : llist)
    llength <- function(lst, acc = 0)
        cases(lst,
              NIL -> acc,
              CONS(car, cdr) -> llength(cdr, acc + 1))

    expect_true(tailr::can_loop_transform(llength))

    skip("We cannot yet handle `with` expressions in `tailr`, which is needed here.")

})


