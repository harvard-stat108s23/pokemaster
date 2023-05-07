
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pokemaster <img src="/man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
<!-- badges: end -->

The goal of `pokemaster` is to explore the Pokémon API. This package
exploits the information of the Pokémon of the first 3 generations
(which corresponds to the ids 1 to 386). The users can first create a
data frame of the Pokémons of interest. It then helps you generate a
Pokémon team. You can also see the relevant stats of your team and
create a graph of the team including the selected Pokémons. Our intended
audience are Pokémon enthusiasts as well as gamers in general. For more
general information about Pokémon, you can visit
[Bulbapedia](https://bulbapedia.bulbagarden.net/wiki/List_of_Pok%C3%A9mon_by_National_Pok%C3%A9dex_number).

## Installation

You can install `pokemaster` package from [GitHub](https://github.com/)
with:

``` r
install.packages("pokemaster")
```

You can install the development version of `pokemaster` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("harvard-stat108s23/pokemaster")
```

## About the Data

The [pokemon API](https://pokeapi.co/) contains information on Pokémons,
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
relevant information of the Pokémon “charizard” by its name or by its
id. We can show its weight, height, base_experience, HP, attack,
defense, special attack, special_defense, speed, type 1 and type 2.

## Example 2 – Pokecard Generation

In example 2, we illustrate how you can easily create a team of Pokémons
of your choice, and create a graph out of it.

``` r
pokecard(myteam = c("pikachu","staryu", "exeggutor", "wynaut", "wailmer", "xatu"), color="LightBlue", title="Best Team")
```

<img src="man/figures/README-pokecard-1.png" width="100%" />

In the above example, we used the `pokecard` function to pick a team of
Pokemons. We picked 6 Pokémons - “pikachu”,“staryu”, “exeggutor”,
“wynaut”, “wailmer”, “xatu” -, customized the card color to “LightBlue”,
and chose a name for the card “Best Team”. You can see the `pokecard` is
generated accordingly.

## Example 3 – Stats analysis

In example 3, we illustrate how you can easily visualize the 6 main
stats (HP, Attack, Defense, Special Attack, Special Defense and Speed)
of your Pokémon team.

``` r
pokestats(myteam = c("pikachu","staryu", "exeggutor", "wynaut", "wailmer", "xatu"), title="Best Team")
```

<img src="man/figures/README-pokestats-1.png" width="100%" />

In the above example, we used the `pokestats` function to plot the stats
of the same Pokémon team and chose the title name “Best Team”. You can
see the `pokestats` barplot is generated accordingly.
