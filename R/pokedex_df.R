#' Pokedex: Information of the pokemons of the 3 first generations.
#'
#' @format A tibble with 386 rows and 13 variables:
#' \describe{
#'   \item{id}{The identifier for of the pokemon. Only pokemons from the first 3 generations are considered.}
#'   \item{name}{The name of the pokemon.}
#'   \item{weight}{	The weight of this Pokémon in hectograms.}
#'   \item{height}{	The height of this Pokémon in decimetres.}
#'   \item{base_experience}{The base experience gained for defeating this Pokémon.}
#'   \item{hp}{Determines how much damage a Pokémon can receive before fainting.}
#'   \item{attack}{Determines how much damage a Pokémon deals when using a physical move.}
#'   \item{defense}{Determines how much damage a Pokémon receives when it is hit with a physical move.}
#'   \item{special_attack}{Determines how much damage a Pokémon deals when using a special move.}
#'   \item{special_defense}{Determines how much damage a Pokémon receives when it is hit with a special move.}
#'   \item{speed}{Determines the order of Pokémon that can act in battle.}
#'   \item{type_1}{The first type the referenced Pokémon has.}
#'   \item{type_2}{	The second type the referenced Pokémon has.}
#' }
#' @source {https://pokeapi.co/docs/v2#pokemon}

"pokedex_df"
