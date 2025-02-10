# this test file verifies that are_all_close meets the 
# hw2 testing requirements for tolerance-based comparisons.
# (hw2 requires three tests.

library(testthat)

# test that it returns TRUE when vectors are nearly identical
test_that("are_all_close returns TRUE when vectors are nearly identical", {
  v <- c(1, 2, 3)
  w <- c(1 + 1e-7, 2 - 1e-7, 3)
  expect_true(are_all_close(v, w))
})

# test that it returns FALSE when relative error exceeds `rel_tol`
test_that("are_all_close returns FALSE when relative error exceeds tolerance", {
  v <- c(1e-8, 2e-8, 3e-8)
  w <- c(1.1e-8, 2.1e-8, 3.1e-8)  # difference is 1e-9, which is < abs_tol (1e-6)
  expect_false(are_all_close(v, w))
})

# test that it returns FALSE when absolute error exceeds `abs_tol`
test_that("are_all_close returns FALSE when absolute error exceeds tolerance", {
  v <- c(1, 2, 3)
  w <- c(1 + 1e-5, 2, 3)  # 1e-5 > abs_tol (1e-6)
  expect_false(are_all_close(v, w))
})