#' Title
#'
#' @param n
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


pokegen <- function(n=6) {


  #TEST
  #n must be a VECTOR of NATURAL (INTEGER POSITIVE) from 1 to 6. Example:  pokegen(3)
  #Delina test function
   if(!(is.numeric(n))){
     stop('Input must be a vector of positive integers.')
   }
   if(!(n %% 1 == 0)){
     stop('Input must be an integer.')
   }
   if(is.numeric(n) & (n < 1 | n > 6)){
     stop('Input must be between 1 and 6.')
   }
   poketeam <- pokedex(sample(seq(1,386),n))$name
   return(poketeam)
 }

myteam <- pokegen()
