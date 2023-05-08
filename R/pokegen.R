#' Generate a random pokemon team
#'
#' `pokegen` creates a vector of `n` random pokemons
#' @param n Integer. `n` is the number of pokemons to generate. A pokemon team can range from 1 pokemon to 6 pokemons
#'
#' @return Vector of the names of the `n` pokemons randomly generated
#' @examples
#' pokegen(n=3)
#' pokegen(n=6)
#' @export


pokegen <- function(n=6) {
# ---------- Check inputs

  if(is.character(n)){
    stop('`n` must be an integer')
  }

  #Check that 'n' is a single integer
  if(!(length(n) == 1 & is.numeric(n) & all(n%%1 == 0))){
    stop('`n` must be a single integer')
  }

  # Check that 'n' is between 1 and 6
  if(!(n >= 1 & n <= 6)){
    stop('`n` must be between 1 and 6.')
  }

# ---------- Function

  #Random sample `n` pokemon names from the full pokedex
   poketeam <- pokemaster::pokedex(sample(seq(1,386),n))$name
   return(poketeam)
 }


