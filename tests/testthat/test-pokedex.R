test_that("correct pokemon name", {
  expect_error(pokedex("pikachuu"))
})

test_that("check class", {
  expect_s3_class(pokedex(c(1, 2, 3)), "data.frame")
})
