#' Title TO EDIT
#'
#' @param myteam
#' @param title  TO EDIT
#'
#' @return
#' @export
#'
#' @examples


pokestats <- function(myteam= pokegen(), title="My Pokemon Team") {
  name <- value <- variable <- NULL


  #TEST
  #myteam must be a vector of numbers between 1 and 386 or a vector of valid pokemon names or a combination of both
  #the length of the vector must be an integer between 1 and 6
  #title must be a string

  #Delina test function
   if(!(is.numeric(myteam) | (is.character(myteam)))){
     stop('Input must be a vector of positive integers, strings, or a combination of both.')
   }
  # #you said numbers but you mean integers right
  # if(is.numeric(myteam) & (!(myteam %% 1 == 0))){
  #   stop('Input must be an integer.')
  # }
  # if(is.character(myteam) & (!(myteam %in% pokemon_info$name))){
  #   stop('Input must be a valid pokemon name.')
  # }
  # if(is.numeric(myteam) & (myteam < 1 | myteam > 386)){
  #   stop('Input must be between 1 and 386.')
  # }
   x = length(myteam)
   if((x < 1) | (x > 6)){
     stop('Length of vector must be between 1 and 6.')
   }
   if(!(is.character(title))){
     stop('Title must be a string.')
   }
  #IMPROVE GRAPH
  #Pls improve title, axis, colors, legend name
  #Rename legend "variable" to "stat"
  #Capitalize name of the pokemons
  #replace the names of the stats:
  #hp -> HP
  #attack -> Attack
  #defense -> Defense
  #special_attack -> Special Attack
  #special_defense -> Special Defense
  #speed -> Speed

  colors <- palette(RColorBrewer::brewer.pal(6, "Set1"))
  barplot <- pokemaster::pokedex(myteam) |>
    dplyr::select(c("name", "hp", "attack", "defense", "special_attack", "special_defense", "speed")) |>
    reshape2::melt(id.vars = "name") |>
    ggplot2::ggplot(ggplot2::aes(x = name, y = value, fill = variable)) +
    ggplot2::geom_bar(stat="identity", position = "dodge") +
    ggplot2::scale_fill_manual(values=colors) +
    ggplot2::ggtitle(title) #+
  #theme_classic()

  return(barplot)
}

#pokestats(myteam = c("pikachu","staryu", "exeggutor", "wynaut", "wailmer", "xatu"), "Best Team")   SHOULD NOT BE HERE



