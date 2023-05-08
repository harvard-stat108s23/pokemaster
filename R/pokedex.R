#' Obtain the information and stats of the pokemons in a data frame
#'
#' `pokedex` creates a dataframe with information and stats of the corresponding pokemons
#' @param ids Vector of integers (from 1 to 386) or valid pokemon names (lowercase). By default, it extracts the information and stats of the pokemons of the first 3 generations, which corresponds to the numeric ids 1 to 386
#'
#' @return Data frame with the information and stats of the pokemons

#' @examples
#' pokedex(c("bulbasaur", "charmander", "squirtle"))
#' pokedex(c(1,4,7))
#' @export


pokedex <- function(ids = seq(1,386)){

# ---------- Check inputs

  # Check that the input is a vector of integers or strings
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


# ---------- Function

  # Instantiate NULL variables
  stat <- name <- base_stat <- type <- slot <- NULL
  pokedex_df <- data.frame(NULL)

  # Loop through all the ids, extract the content using the API, and parse the data to build the data frame
  for (id in ids) {
    response <- httr::GET(paste0("https://pokeapi.co/api/v2/pokemon/",id,"/"))
    content <- httr::content(response, as = "text")
    pokemon_info <- jsonlite::fromJSON(content)[c("id", "name", "weight", "height", "base_experience", "stats", "types")]

    # Information for one pokemon: wrangles the relevant information and builds a dataframe
    new_pokemon <- data.frame(id = pokemon_info$id,
                              name = pokemon_info$name,
                              weight = pokemon_info$weight,
                              height = pokemon_info$height,
                              base_experience = pokemon_info$base_experience) |>
      cbind(
        pokemon_info$stats|>
          tidyr::unnest(cols=c(stat)) |>
          dplyr::select(name, base_stat) |>
          tidyr::pivot_wider(names_from =name, values_from = base_stat),

        pokemon_info$types |>
          tidyr::unnest(cols=c(type)) |>
          dplyr::select(slot, name)  |>
          tidyr::pivot_wider(names_from =slot, values_from = name)
      )

    # Append the information
    pokedex_df <- dplyr::bind_rows(pokedex_df, new_pokemon)
  }

  # To avoid an error, in case the pokemon only has one 'type'
  if (!('2' %in% names(pokedex_df))) {
    pokedex_df$'2' <- NA
  }

  # Rename variables
  pokedex_df <- pokedex_df |>
    dplyr::rename("special_attack" = 'special-attack',
                  "special_defense" = "special-defense",
                  "type_1" =  "1" ,"type_2" = "2")

  return(pokedex_df)
}

