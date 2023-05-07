test_that("function outputs a data frame", {
  expect_s3_class(pokedex(c(1, 2, 3)), "data.frame")
  expect_s3_class(pokedex(c(1,2,3)),"data.frame")
  expect_s3_class(pokedex(1), "data.frame")
  expect_s3_class(pokedex("pikachu"), "data.frame")
  expect_s3_class(pokedex(c("pikachu","charmander")), "data.frame")
  expect_s3_class(pokedex(c("pikachu",5)), "data.frame")
})


test_that("function does not take incorrect inputs", {
  expect_error(pokedex(2.5))
  expect_error(pokedex(390))
  expect_error(pokedex(-2))
  expect_error(pokedex("pikachuu"))
  expect_error(pokedex(1,"pikachu", "this color does not exist"))
})

