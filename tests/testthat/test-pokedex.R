test_that("function takes valid pokemon names", {
  expect_error(pokedex("pikachuu"))
})

test_that("function outputs a data frame", {
  expect_s3_class(pokedex(c(1, 2, 3)), "data.frame")
})
