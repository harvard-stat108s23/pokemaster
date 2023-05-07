test_that("function produces a graph", {
  expect_visible(pokestats())
  expect_visible(pokestats(c(1,2,3)))
  expect_visible(pokestats(1))
  expect_visible(pokestats("pikachu"))
  expect_visible(pokestats(c("pikachu","charmander")))
  expect_visible(pokestats(c("pikachu",5)))
})

test_that("function does not take incorrect inputs", {
  expect_error(pokestats(2.5))
  expect_error(pokestats(390))
  expect_error(pokestats(-2))
  expect_error(pokestats("pikachuu"))
  expect_error(pokestats(c(1,2,3,4,5,6,7,8)))
  expect_error(pokestats(1,"pikachu", "this color does not exist"))
})
