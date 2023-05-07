test_that("function produces graph", {
  expect_visible(pokestats(1))
})

test_that("function does not take decimals", {
  expect_error(pokestats(3.5))
})

test_that("title is a string", {
  expect_type(pokestats(title = "asd"), "list")
})
