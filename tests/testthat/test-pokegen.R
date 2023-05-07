test_that("function does not take values above 6", {
  expect_error(pokegen(10))
})

test_that("function does not take strings", {
  expect_error(pokegen("hello"))
})

test_that("function does not take logicals", {
  expect_error(pokegen(TRUE))
})

test_that("function does not take decimal numbers or negative numbers", {
  expect_error(pokegen(2.5))
  expect_error(pokegen(-3))
})

test_that("function outputs a vector", {
  expect_vector(pokegen(5))
})

