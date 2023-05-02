#' Title
#'
#' @param ids
#'
#' @return
#' @export
#'
#' @examples

library(httr)
library(jsonlite)
library(magick)
library(pryr)
library(hexSticker)
library(ggplot2)
library(RColorBrewer)

pokedex <- function(ids = seq(1,386)){

  #TEST
  #ids must be a VECTOR of NATURAL (INTEGER POSITIVE) numbers from 1 to 386 OR   - Example: pokedex(c(1,4,3))
  #it can also be a vector of strings of the name of the pokemons (MUST BE A VALID NAME) - Example: pokedex(c("pikachu","charmander"))
  #it can also be a mixture of both  Example: pokedex(c(1,"pikachu"))

  #Delina test function
   if(!(is.numeric(ids) | (is.character(ids)))){
     stop('Input must be a vector of positive integers, strings, or a combination of both.')
   }
   if(!(ids %% 1 == 0)){
     stop('Input must be an integer.')
   }
   if(is.numeric(ids) & (ids < 1 | ids > 386)){
     stop('Input must be between 1 and 386.')
   }
   if(!(ids %in% pokemon_info$name)){
     stop('Input must be a valid pokemon name.')
   }

  #change this name
  pokedex <- data.frame(NULL)

  for (id in ids) {
    response <- GET(paste0("https://pokeapi.co/api/v2/pokemon/",id,"/"))
    content <- content(response, as = "text")
    pokemon_info <- fromJSON(content)[c("id", "name", "weight", "height", "base_experience", "stats", "types")]

    new_pokemon <- data.frame(id = pokemon_info$id,
                              name = pokemon_info$name,
                              weight = pokemon_info$weight,
                              height = pokemon_info$height,
                              base_experience = pokemon_info$base_experience) %>%
      cbind(
        pokemon_info$stats%>%
          tidyr::unnest(cols=c(stat)) %>%
          dplyr::select(name, base_stat) %>%
          tidyr::pivot_wider(., names_from =name, values_from = base_stat),

        pokemon_info$types %>%
          tidyr::unnest(cols=c(type)) %>%
          dplyr::select(slot, name)  %>%
          tidyr::pivot_wider(., names_from =slot, values_from = name)
      )

    pokedex <- dplyr::bind_rows(pokedex, new_pokemon)
  }

  if (!('2' %in% names(pokedex))) {
    pokedex$'2' <- NA
  }

  pokedex <- pokedex %>%
    dplyr::rename("special_attack" = 'special-attack',
                  "special_defense" = "special-defense",
                  "type_1" =  "1" ,"type_2" = "2")

  return(pokedex)
  #assign("pokedex", pokedex, envir = .GlobalEnv)   ##this creates a df named pokemodex (should it be done this way?) Not for now
}

pokedex()
