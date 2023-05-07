test_that("function produces sprites", {
  expect_visible(pokecard(c(1,2,3)))
})

test_that("function does not take incorrect pokemon names", {
  expect_error(pokecard("pikachuu"))
})
