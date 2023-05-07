test_that("function does not take values above 6", {
  expect_error(pokegen(10))
})

test_that("function does not take strings", {
  expect_error(pokegen("hello"))
})

test_that("function does not take logicals", {
  expect_error(pokegen(TRUE))
})

test_that("function outputs the correct number of pokemon names", {
  expect_vector(pokegen(5))
})
