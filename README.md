
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Pokemaster <img src="/man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
<!-- badges: end -->

The goal of pokemaster is to explore the pokemon API. The users can
first create a dataframe of the interested pokemons (our default is the
386 pokemons, the first 3 generations). It then helps you generate a
pokemon team. You can also see the relevant stats of your team and
create a graph of the pokemon team including the selected pokemons.

## Installation

You can install pokemaster pacage from [GitHub](https://github.com/)
with:

``` r
install.packages("pokemaster")
```

You can install the development version of pokemaster from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("harvard-stat108s23/project2-group3")
```

## About the Data

The [pokemon API](https://pokeapi.co/) contains information on Pokémon,
their moves, abilities, types, egg groups and much, much more.

An API (Application Programming Interface) is a contract that allow
developers to interact with an application through a set of interfaces.
In this case, the application is a database of thousands of
Pokémon-related objects, and the interfaces are URL links.

## Example 1 – Information Retrieval

This is a basic example which shows you how to retrieve the relevant
information we have about a specific Pokemon.

``` r
library(pokemaster)
```

``` r
pokedex(c("charizard"))
#>   id      name weight height base_experience hp attack defense special_attack
#> 1  6 charizard    905     17             267 78     84      78            109
#>   special_defense speed type_1 type_2
#> 1              85   100   fire flying
```

``` r
pokedex(c(6))
#>   id      name weight height base_experience hp attack defense special_attack
#> 1  6 charizard    905     17             267 78     84      78            109
#>   special_defense speed type_1 type_2
#> 1              85   100   fire flying
```

In the above example, we illustrated how someone could retrieve the
relevant information of the Pokemon “charizard” by its name or by its
id. We can show its weight, height, base_experience, hp, attack,
defense, special attact, special_defense, speed, and type 1 and type 2
information.

## Example 2 – Pokecard Generation

In example 2, we illustrate how you can easily create a team of Pokemons
of your choice, and create a graphic out of it.

``` r
pokecard(myteam = c("pikachu","staryu", "exeggutor", "wynaut", "wailmer", "xatu"), color="LightBlue", title="Best Team")
```

<img src="man/figures/README-pokecard-1.png" width="100%" />

In the above example, we used the pokecard function to pick a team of
Pokemonds. We picked 6 Pokemons – “pikachu”,“staryu”, “exeggutor”,
“wynaut”, “wailmer”, “xatu”, customized the card color to “LightBlue”,
and chose a name for the card “Best Team”. You can see the pokecard is
generated accordingly.

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.
