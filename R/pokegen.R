#' Generate a team of pokemons
#'
#' `pokegen` creates a vector of `n` random pokemons
#' @param n Integer. Number of pokemons to generate. Can be from 1 pokemon to 6 pokemons
#'
#' @return Vector of names of the `n` pokemons generated
#' @examples
#' pokegen(3)
#' @export



pokegen <- function(n=6) {


  #TEST
  #n must be a VECTOR of NATURAL (INTEGER POSITIVE) from 1 to 6. Example:  pokegen(3)
  #Delina test function
  # if(!(is.numeric(n))){
  #   stop('Input must be a vector of positive integers.')
  # }
  # if(!(n %% 1 == 0)){
  #   stop('Input must be an integer.')
  # }
  # if(is.numeric(n) & (n < 1 | n > 6)){
  #   stop('Input must be between 1 and 6.')
  # }
   poketeam <- pokemaster::pokedex(sample(seq(1,386),n))$name
   return(poketeam)
 }


