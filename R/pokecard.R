#' Create a flashcard of the pokemon team
#'
#' `pokecard` create an image of the pokemon team, appending their sprites
#'
#' @param myteam A vector with a length of up 6 to pokemons. It can be valid numeric ids or valid pokemon names (lowercase). By default, it creates a flashcard of a team of 6 random pokemons.
#' @param title A string. It determines the title at the top of the flashcard. By default, the title is 'My Pokemon Team'
#' @param color A string, which should be a valid color. It determines the color of the border of the flashcard.
#'
#' @return An external pointer of class 'magick-image'. It shows the flashcard along with a tibble describing the features of the image.
#' @examples
#' pokecard(c("pikachu","bulbasaur", "squirtle", "charmander", "pidgey", "psyduck"))
#' pokecard(c("charmander","charmeleon", "charizard"),  "Charmander's evolutions", "Orange")
#' @export


pokecard <- function(myteam = pokegen(), title="My Pokemon Team", color="#c60031") {

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

  # Check that the color is valid
  if(!(tryCatch(is.matrix(grDevices::col2rgb(color)), error = function(e) FALSE))){
    stop('`Color` must be a valid color')
  }


# ---------- Function

  # Instantiate NULL and empty elements
  ids <- pokemaster::pokedex(myteam)$id
  sprites_links <- images <- c()
  joined_image <- NULL

  # Get the sprites
  i=1
  for (id in ids) {
    sprites_links <- c(sprites_links, paste0("https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/",id,".png"))
    images <- c(images, paste0("image",i))
    i=i+1
  }

  # Process the sprites and join them
  for (i in 1:length(images)) {
    assign(images[i], magick::image_trim(magick::image_scale(magick::image_read(sprites_links[i]), "300x300")))
    joined_image <- magick::image_join(joined_image, get(images[i]))
  }

  # Add the border of the flashcard
  flashcard <- magick::image_border(
    magick::image_montage(joined_image, tile = 3),
    geometry = "25x25",
    color=color
  )

  # Add the title of the flashcard
  flashcard <- magick::image_annotate(flashcard, text = title, size = 15, gravity = "north", location="-0+3")
  return(flashcard)
}

