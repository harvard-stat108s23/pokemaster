pokedex_df <- NULL
for (id in seq(1,386)) {
  response <- httr::GET(paste0("https://pokeapi.co/api/v2/pokemon/",id,"/"))
  content <- httr::content(response, as = "text")
  pokemon_info <- jsonlite::fromJSON(content)[c("id", "name", "weight", "height", "base_experience", "stats", "types")]

  new_pokemon <- data.frame(id = pokemon_info$id,
                            name = pokemon_info$name,
                            weight = pokemon_info$weight,
                            height = pokemon_info$height,
                            base_experience = pokemon_info$base_experience) |>
    cbind(
      pokemon_info$stats|>
        tidyr::unnest(cols=c(stat)) |>
        dplyr::select(name, base_stat) |>
        tidyr::pivot_wider(names_from =name, values_from = base_stat),

      pokemon_info$types |>
        tidyr::unnest(cols=c(type)) |>
        dplyr::select(slot, name)  |>
        tidyr::pivot_wider(names_from =slot, values_from = name)
    )

  pokedex_df <- dplyr::bind_rows(pokedex_df, new_pokemon)
}

pokedex_df <- pokedex_df |>
  dplyr::rename("special_attack" = 'special-attack',
                "special_defense" = "special-defense",
                "type_1" =  "1" ,"type_2" = "2")

usethis::use_data(pokedex_df, overwrite = TRUE)
