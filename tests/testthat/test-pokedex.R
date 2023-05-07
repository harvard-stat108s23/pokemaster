test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("check class", {
  expect_s3_class(pokedex(pokemaster), "is.character")
})

if (!(is.numeric(ids)|is.character(ids))) {
  stop('`ids` must be a vector of integers or valid pokemon names')
}

  # Check that, if the input is a numeric vector, all values must be integers
  if(is.numeric(ids)){
    if(!all(ids %% 1 == 0)){
      stop('If using numeric values, all of them must be integers')
    }}

  # Check that, if the input is a numeric vector, the values must be between 1 and 386
  if(is.numeric(ids)){
    if(!all(ids >= 1 & ids <= 386)){
      stop('If using numeric values, they must be between 1 and 386')
    }}

  # Check that, if the input is a string vector, the values are valid pokemon names
  if(is.character(ids)){
    if((!all(suppressWarnings(ids[is.na(as.numeric(ids))]) %in% pokedex_df$name))){
      stop('If using pokemons names, all values must be valid pokemon names from the first 3 generations')
    }}
