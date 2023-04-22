library(httr)
library(jsonlite)
library(magick)
library(pryr)
library(hexSticker)
library(ggplot2)
library(RColorBrewer)


#FUNCTION 1#################################################################
pokedex <- function(ids = seq(1,386)){
  #TEST
  #ids must be a VECTOR of NATURAL (INTEGER POSITIVE) numbers from 1 to 386 OR   - Example: pokedex(c(1,4,3))
  #it can also be a vector of strings of the name of the pokemons (MUST BE A VALID NAME) - Example: pokedex(c("pikachu","charmander"))
  #it can also be a mixture of both  Example: pokedex(c(1,"pikachu"))
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


#FUNCTION 2#######################################
pokegen <- function(n=6) {
  #TEST
  #n must be a VECTOR of NATURAL (INTEGER POSITIVE) from 1 to 6. Example:  pokegen(3)
  poketeam <- pokedex(sample(seq(1,386),n))$name
  return(poketeam)
}

myteam <- pokegen()


#FUNCTION 3#########################################
pokecard <- function(myteam, color="#c60031", title="My Pokemon Team") {
  #TEST
  #myteam must be a vector of numbers between 1 and 386 or a vector of valid pokemon names or a combination of both
  #the length of the vector must be an integer between 1 and 6
  #title must be a string
  #color must be a valid color or valid HEX code
  
  ids <- pokedex(myteam)$id
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
    joined_image <- image_join(joined_image, get(images[i]))
  }
  
  flashcard <- magick::image_border(
    magick::image_montage(joined_image, tile = 3),
    geometry = "25x25",
    color=color
  )
  
  flashcard <- magick::image_annotate(flashcard, text = title, size = 15, gravity = "north", location="-0+3")
  return(flashcard)
}

pokecard(myteam, color="LightBlue", title="Best Team")


#FUNCTION 4###################################
pokestats <- function(myteam, title="My Pokemon Team") {
  #TEST
  #myteam must be a vector of numbers between 1 and 386 or a vector of valid pokemon names or a combination of both
  #the length of the vector must be an integer between 1 and 6
  #title must be a string
  
  #IMPROVE GRAPH
  #Pls improve title, axis, colors, legend name
  #Rename legend "variable" to "stat"
  #Capitalize name of the pokemons
  #replace the names of the stats:
  #hp -> HP
  #attack -> Attack
  #defense -> Defense
  #special_attack -> Special Attack
  #special_defense -> Special Defense
  #speed -> Speed
  
  colors <- palette(RColorBrewer::brewer.pal(6, "Set1"))
  barplot <- pokedex(myteam) %>%
    dplyr::select(c("name", "hp", "attack", "defense", "special_attack", "special_defense", "speed")) %>%
    reshape2::melt(., id.vars = "name") %>%
    ggplot2::ggplot(., aes(x = name, y = value, fill = variable)) + 
    ggplot2::geom_bar(stat="identity", position = "dodge") + 
    scale_fill_manual(values=colors) +
    ggtitle(title) #+
    #theme_classic()
  
  return(barplot)
}

pokestats(myteam, "Best Team")



#HEX STICKER######################################################
img <- image_read('C:\\Users\\samch\\OneDrive\\Desktop\\STAT108\\HW\\Project 2\\Imagen1.png')
hexSticker::sticker(
  package = "pokemaster",
  subplot = img,
  s_width=1,
  s_height=1,
  s_x=1,
  s_y=0.75,
  p_size=16,
  h_fill = '#5c3784',
  h_color = '#ac2556',
  h_size=5,
  l_y=1,
  l_x=1,
  l_height=3,
  l_alpha=0.3,
  filename = "C:\\Users\\samch\\OneDrive\\Desktop\\STAT108\\HW\\Project 2\\pokemaster.png"
)  %>% print()










######################DRAFT CODE###### DO NOT TAKE INTO ACCOUNT ANYTHING AFTER THIS ###################

#GLOBAL##########################################
#pokemon <- c("bulbasaur","ivysaur","venusaur","charmander","charmeleon","charizard","squirtle","wartortle","blastoise","caterpie","metapod","butterfree","weedle","kakuna","beedrill","pidgey","pidgeotto","pidgeot","rattata","raticate","spearow","fearow","ekans","arbok","pikachu","raichu","sandshrew","sandslash","nidoran-female","nidorina","nidoqueen","nidoran-male","nidorino","nidoking","clefairy","clefable","vulpix","ninetales","jigglypuff","wigglytuff","zubat","golbat","oddish","gloom","vileplume","paras","parasect","venonat","venomoth","diglett","dugtrio","meowth","persian","psyduck","golduck","mankey","primeape","growlithe","arcanine","poliwag","poliwhirl","poliwrath","abra","kadabra","alakazam","machop","machoke","machamp","bellsprout","weepinbell","victreebel","tentacool","tentacruel","geodude","graveler","golem","ponyta","rapidash","slowpoke","slowbro","magnemite","magneton","farfetch'd","doduo","dodrio","seel","dewgong","grimer","muk","shellder","cloyster","gastly","haunter","gengar","onix","drowzee","hypno","krabby","kingler","voltorb","electrode","exeggcute","exeggutor","cubone","marowak","hitmonlee","hitmonchan","lickitung","koffing","weezing","rhyhorn","rhydon","chansey","tangela","kangaskhan","horsea","seadra","goldeen","seaking","staryu","starmie","mr. mime","scyther","jynx","electabuzz","magmar","pinsir","tauros","magikarp","gyarados","lapras","ditto","eevee","vaporeon","jolteon","flareon","porygon","omanyte","omastar","kabuto","kabutops","aerodactyl","snorlax","articuno","zapdos","moltres","dratini","dragonair","dragonite","mewtwo","mew","chikorita","bayleef","meganium","cyndaquil","quilava","typhlosion","totodile","croconaw","feraligatr","sentret","furret","hoothoot","noctowl","ledyba","ledian","spinarak","ariados","crobat","chinchou","lanturn","pichu","cleffa","igglybuff","togepi","togetic","natu","xatu","mareep","flaaffy","ampharos","bellossom","marill","azumarill","sudowoodo","politoed","hoppip","skiploom","jumpluff","aipom","sunkern","sunflora","yanma","wooper","quagsire","espeon","umbreon","murkrow","slowking","misdreavus","unown","wobbuffet","girafarig","pineco","forretress","dunsparce","gligar","steelix","snubbull","granbull","qwilfish","scizor","shuckle","heracross","sneasel","teddiursa","ursaring","slugma","magcargo","swinub","piloswine","corsola","remoraid","octillery","delibird","mantine","skarmory","houndour","houndoom","kingdra","phanpy","donphan","porygon2","stantler","smeargle","tyrogue","hitmontop","smoochum","elekid","magby","miltank","blissey","raikou","entei","suicune","larvitar","pupitar","tyranitar","lugia","ho-oh","celebi","treecko","grovyle","sceptile","torchic","combusken","blaziken","mudkip","marshtomp","swampert","poochyena","mightyena","zigzagoon","linoone","wurmple","silcoon","beautifly","cascoon","dustox","lotad","lombre","ludicolo","seedot","nuzleaf","shiftry","taillow","swellow","wingull","pelipper","ralts","kirlia","gardevoir","surskit","masquerain","shroomish","breloom","slakoth","vigoroth","slaking","nincada","ninjask","shedinja","whismur","loudred","exploud","makuhita","hariyama","azurill","nosepass","skitty","delcatty","sableye","mawile","aron","lairon","aggron","meditite","medicham","electrike","manectric","plusle","minun","volbeat","illumise","roselia","gulpin","swalot","carvanha","sharpedo","wailmer","wailord","numel","camerupt","torkoal","spoink","grumpig","spinda","trapinch","vibrava","flygon","cacnea","cacturne","swablu","altaria","zangoose","seviper","lunatone","solrock","barboach","whiscash","corphish","crawdaunt","baltoy","claydol","lileep","cradily","anorith","armaldo","feebas","milotic","castform","kecleon","shuppet","banette","duskull","dusclops","tropius","chimecho","absol","wynaut","snorunt","glalie","spheal","sealeo","walrein","clamperl","huntail","gorebyss","relicanth","luvdisc","bagon","shelgon","salamence","beldum","metang","metagross","regirock","regice","registeel","latias","latios","kyogre","groudon","rayquaza","jirachi","deoxys")
#id <- seq(1,length(pokemon))
#pokemons_df <- data.frame(id, pokemon)


#cat(paste("Name:", pokemon_df$name))
#cat(paste("Weight:", pokemon_df$weight))
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


pokemon_df$types %>%
  tidyr::unnest(cols=c(type)) %>%
  dplyr::select(slot, name)  %>%
  tidyr::pivot_wider(., names_from =slot, values_from = name)


response3 <- GET(pokemon_df$sprites$front_default)
response3

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
