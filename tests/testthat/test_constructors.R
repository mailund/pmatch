context("constructors")



test_that("We can create constant constructors", {
  type := A | B
  expect_true(exists("A", environment()))
  expect_true(inherits(A, "type"))
})
