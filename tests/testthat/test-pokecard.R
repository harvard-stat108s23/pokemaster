test_that("function produces sprites", {
  expect_visible(pokecard())
  expect_visible(pokecard(c(1,2,3)))
  expect_visible(pokecard(1))
  expect_visible(pokecard("pikachu"))
  expect_visible(pokecard(c("pikachu","charmander")))
  expect_visible(pokecard(c("pikachu",5)))
  expect_visible(pokecard(1,"pikachu", "red"))
})


test_that("function does not take incorrect inputs", {
  expect_error(pokecard(2.5))
  expect_error(pokecard(390))
  expect_error(pokecard(-2))
  expect_error(pokecard("pikachuu"))
  expect_error(pokecard(c(1,2,3,4,5,6,7,8)))
  expect_error(pokecard(1,"pikachu", "this color does not exist"))
})
