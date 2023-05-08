#'  Barplot of the main stats of the pokemons
#'
#' `pokestats` creates a barplot of the main stats of the pokemons (HP, Attack, Defense, Sp. Attack, Sp. Defense and Speed)
#'
#' @param myteam A vector with a length of up 6 to pokemons. It can be valid numeric ids or valid pokemon names (lowercase). By default, it creates a barplot of a team of 6 random pokemons.
#' @param title  A string. It determines the title at the top of the plot By default, the title is 'My Pokemon Team'
#'
#' @return ggplot2 object, which contains the barplot of the main stats for each pokemon
#' @examples
#' pokestats(myteam = c("pikachu","bulbasaur", "squirtle", "charmander", "pidgey", "psyduck"))
#' pokestats(myteam = c("charmander","charmeleon", "charizard"))
#' @export



pokestats <- function(myteam= pokegen(), title="My Pokemon Team") {

# ---------- Check inputs

  # Check that the input is a vector of integers or strings
  if (!(is.numeric(myteam)|is.character(myteam))) {
    stop('`myteam` must be a vector of integers or valid pokemon names')
  }

  # Check that, if the input is a numeric vector, all values must be integers
  if(is.numeric(myteam)){
    if(!all(myteam %% 1 == 0)){
      stop('If using numeric values, all of them must be integers')
    }}

  # Check that, if the input is a numeric vector, the values must be between 1 and 386
  if(is.numeric(myteam)){
    if(!all(myteam >= 1 & myteam <= 386)){
      stop('If using numeric values, they must be between 1 and 386')
    }}

  # Check that, if the input is a string vector, the values are valid pokemon names
  if(is.character(myteam)){
    if((!all(suppressWarnings(myteam[is.na(as.numeric(myteam))]) %in% pokemaster::pokedex_df$name))){
      stop('If using pokemons names, all values must be valid pokemon names from the first 3 generations')
    }}

  # Check that the pokemon team has between 1 and 6 pokemons
  if(!(length(myteam) >= 1 & length(myteam) <= 6)){
    stop('The team must have between 1 and 6 pokemons')
  }

  # Check that the title is a string
  if(!(is.character(title))){
    stop('`Title` must be a string')
  }


# ---------- Function

  # Instantiate NULL and empty elements
  name <- value <- variable <- palette <- NULL

  # Create the barplot using ggplot2, previously wrangling the data
  colors <- c("#FF0000", "#F08030", "#F8D030", "#6890F0", "#78C850", "#F85888")
  lab = c("HP", "Attack", "Defense", "Special Attack", "Special Defense", "Speed")
  barplot <- pokemaster::pokedex(myteam) |>
    dplyr::select(c("name", "hp", "attack", "defense", "special_attack", "special_defense", "speed")) |>
    reshape2::melt(id.vars = "name") |>
    ggplot2::ggplot(ggplot2::aes(x = tools::toTitleCase(name), y = value, fill = variable)) +
    ggplot2::geom_bar(stat="identity", position = "dodge") +
    ggplot2::scale_fill_manual(values=colors, labels=lab) +
    ggplot2::labs(fill = "Stat") +
    ggplot2::ggtitle(title) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x=ggplot2::element_blank(),
          axis.title.y=ggplot2::element_blank())


  return(barplot)
}



