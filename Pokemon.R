library(httr)
library(jsonlite)

response <- GET("https://pokeapi.co/api/v2/pokemon/pikachu/")
pokemon_data <- content(response, as = "text")
pokemon_df <- fromJSON(pokemon_data)

cat(paste("Name:", pokemon_df$name))
cat(paste("Weight:", pokemon_df$weight))


new_df <- data.frame(id = pokemon_df$id,
                     name = pokemon_df$name,
                     weight = pokemon_df$weight,
                     height = pokemon_df$height,
                     base_experience = pokemon_df$base_experience)

pokemon_df$stats



pokemon_df$abilities
pokemon_df$form
#pokemon_df$game_indices
pokemon_df$held_items
pokemon_df$is_default
pokemon_df$location_area_encounters
pokemon_df$moves
pokemon_df$order
pokemon_df$past_types
pokemon_df$species
pokemon_df$sprites$front_default
pokemon_df$types





response3 <- GET(pokemon_df$sprites$front_default)
response3
library(magick)
url <- "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/25.png"
img1 <- image_read(url)
url2 <- "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/32.png"
img2 <- image_read(url2)

print(img1)
print(img2)
image1 <- image_scale(img1, "100x100")
image2 <- image_scale(img2, "100x100")


title <- "My Image Title"
result <- image_append(c(image1, image2), stack = FALSE)
result <- image_annotate(result, text = title, size = 20, gravity = "north")
print(result)



image_name<- readJPEG(getURLContent(x)) # for jpg
image_name<- readPNG(getURLContent(x)) # for png



library(png)
img <- readPNG(paste0("https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/", pokemon_df$id, ".png"))
plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
rasterImage(img,0,0,1,1)


response2 <- GET(pokemon_df$species$url)
response2 <- content(response2, as = "text")
response2 <- fromJSON(response2)
response2




image1 <- image_read("https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/25.png")
image2 <- image_read("https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/26.png")
image3 <- image_read("https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/27.png")
image4 <- image_read("https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/28.png")
image5 <- image_read("https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/29.png")
image6 <- image_read("https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/30.png")

# Resize the images to the same dimensions
width <- 500
height <- 500
image1 <- image_trim(image_scale(image1, paste0(width, "x", height)))
image2 <- image_trim(image_scale(image2, paste0(width, "x", height)))
image3 <- image_trim(image_scale(image3, paste0(width, "x", height)))
image4 <- image_trim(image_scale(image4, paste0(width, "x", height)))
image5 <- image_trim(image_scale(image5, paste0(width, "x", height)))
image6 <- image_trim(image_scale(image6, paste0(width, "x", height)))

# Create a 3x2 grid of the images
grid <- image_montage(c(image1, image2, image3, image4, image5), tile = 3)
title <- "My PokeTeam"

border_width <- 20
image_bordered <- image_border(
  grid,
  geometry = "20x20",
)

grid <- image_annotate(image_bordered, text = title, size = 15, gravity = "north", location="-0-10")


# Display the result
print(image_bordered)
print(image_trim(image1))



# functions:
#
# (by name, random, numberofpokemons/conditions (?) legendary, starter, type, with/without reposition)
# 
# 
# 1. POKEDEX: function that creates dataframe with feautres like id, name, stats, type). 
# input: nothing
# output: dataframe of all pokemons
# 
# 
# 2. POKETEAM: flashcard of the team
# input: vector of up to 6 pokemons 
# output: image
# 
# 3. POKESTATS: function that plots barplot of the basic stats for each pokemon_data
# input: vector of up to 6 pokemons  
# output: ggbarplot
# hitpoints, attack, defense, sp attack, special defense, speed

# 4. POKEFILTER: function that ranks and filters ALL pokemon based on different criteria (stats, type, etc)
# input: filter arguments
# output: filtered dataframe

# 5. team_generator (number of pokemons (6))
# output: vector of n random pokemons (#pikachu, "sd", "as", "asds")