#' Title
#'
#' @param myteam
#' @param color
#' @param title  TO EDIT
#'
#' @return
#' @export
#'
#' @examples

pokecard <- function(myteam = pokegen(), color="#c60031", title="My Pokemon Team") {


  #TEST
  #myteam must be a vector of numbers between 1 and 386 or a vector of valid pokemon names or a combination of both
  #the length of the vector must be an integer between 1 and 6
  #title must be a string
  #color must be a valid color or valid HEX code

  #DELINA TEST
  # #does this handle a combination of both or only if its a vector of numbers or names?
  # if(!(is.numeric(myteam) | (is.character(myteam)))){
  #   stop('Input must be a vector of positive integers, strings, or a combination of both.')
  # }
  # #not sure that this works
  # x = length(myteam)
  # if((x < 1) | (x > 6)){
  #   stop('Length of vector must be between 1 and 6.')
  # }
  # #regular expression for hex
  # #regex = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$"
  # #if(color ))){
  # #stop('Color must be a valid color name or HEX code.')
  # #}
  # if(!(is.character(title))){
  #   stop('Title must be a string.')
  # }
  # #should this say is.numeric first??
  # if(is.numeric(myteam) & (!(myteam %% 1 == 0))){
  #   stop('Input must be an integer.')
  # }
  # if(is.character(myteam) & (!(myteam %in% pokemon_info$name))){
  #   stop('Input must be a valid pokemon name.')
  # }
  # if(is.numeric(myteam) & (myteam < 1 | myteam > 386)){
  #   stop('Input must be between 1 and 386.')
  # }

  ids <- pokemaster::pokedex(myteam)$id
  sprites_links <- images <- c()
  joined_image <- NULL

  i=1
  for (id in ids) {
    sprites_links <- c(sprites_links, paste0("https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/",id,".png"))
    images <- c(images, paste0("image",i))
    i=i+1
  }

  for (i in 1:length(images)) {
    assign(images[i], magick::image_trim(magick::image_scale(magick::image_read(sprites_links[i]), "300x300")))
    joined_image <- magick::image_join(joined_image, get(images[i]))
  }

  flashcard <- magick::image_border(
    magick::image_montage(joined_image, tile = 3),
    geometry = "25x25",
    color=color
  )

  flashcard <- magick::image_annotate(flashcard, text = title, size = 15, gravity = "north", location="-0+3")
  return(flashcard)
}


#pokecard(myteam = c("pikachu","staryu", "exeggutor", "wynaut", "wailmer", "xatu"), color="LightBlue", title="Best Team")    SHOULD NOT BE HERE

