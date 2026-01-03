# This is a comment
# Analyse the top selling games on Steam
# Look at age ratings by revenue category
# See what the categories of the games are (action, strategy, etc)
# Check how the review ratings are in comparison to the game's revenue category

# Factors to think about:
# Game ID
# Game Title
# Game cover
# Country of Origin
# Original Price in USD
# Primary descriptive categories
# User recommendation rating from all languages
# PEGI Age Rating
# Year of Release
# Revenue Category (Platinum, Gold, Silver, Bronze)

# Links:
# https://store.steampowered.com/charts/bestofyear/bestof2024?tab=2
# https://store.steampowered.com/app/2358720/Black_Myth_Wukong/
# https://steamdb.info/app/2358720/charts/
# https://pegi.info/search-pegi?q=Black+Myth%3A+Wukong&op=Search&age%5B%5D=&descriptor%5B%5D=&publisher=&platform%5B%5D=&release_year%5B%5D=&page=1&form_build_id=form-rZtf5mHuUlfQWZSercj8Oam8D3sFalPy3kCT4rk7FYM&form_id=pegi_search_form

library(tidyverse)
library(rvest)

test_url <- "https://store.steampowered.com/charts/bestofyear/bestof2024?tab=2"
steam_link <- read_html_live(test_url)

# HTML sections to check on:
# div class contains "SaleSectionCtn items expanded Panel Focusable"
# The above div represents the top new release category.
# There should be at most 4 of the above divs since we have Platinum, Gold, Silver, and Bronze
# div class contains "SaleSectionHeader" -> This is where the category name is
# In div class "ImpressionTrackedElement" under Panel Focusable, you get the URL of the game
# Below the div is another div with data-key="hover div"

categories <- steam_link %>% 
  html_element(xpath = "/html/body/div[1]/div[7]/div[7]/div[3]/div[2]/div/div/div/div/div/div[2]/div/div/div[2]/div[2]/div[4]/div/div[2]/div") %>%
  html_children()
categories




# Collecting The Top Selling Games ----------------------------------------


# To access an individual game:
# div class = "Panel Focusable" -> All games in a metal category (platinum, gold, silver)
# div class = "center_row_trgt ItemCount_3" -> Three games per row in the category
# div class = "" -> div of game
# div class = "... Panel Focusable"
# div class = "ImpressionTrackedElement"
# div class = "..." data-key = "hover div"
# anchor to game -> link to the actual game


# Grab all IDs of the games in the platinum category

plat_game_urls <- top_games_2024_url %>%
  html_elements(xpath ="/html/body/div[1]/div[7]/div[7]/div[3]/div[2]/div/div/div/div/div/div[2]/div/div/div[2]/div[2]/div[4]/div/div[3]") %>%
  html_children() %>%
  html_elements("a") %>%
  html_attr("href") %>%
  unique()
print(plat_game_urls)

plat_game_spliced <- strsplit(plat_game_urls, "/")
plat_game_id_list <- c()
for (game in plat_game_spliced)
{
  plat_game_id_list <- c(plat_game_id_list, game[5])
}
plat_game_id_list <- type.convert(plat_game_id_list, as.is = TRUE)
print(plat_game_id_list)


# Grab all IDs of the games in the gold category

gold_game_urls <- top_games_2024_url %>%
  html_elements(xpath ="/html/body/div[1]/div[7]/div[7]/div[3]/div[2]/div/div/div/div/div/div[2]/div/div/div[2]/div[2]/div[5]/div/div[3]") %>%
  html_children() %>%
  html_elements("a") %>%
  html_attr("href") %>%
  unique()
print(gold_game_urls)

gold_game_spliced <- strsplit(gold_game_urls, "/")
gold_game_id_list <- c()
for (game in gold_game_spliced)
{
  gold_game_id_list <- c(gold_game_id_list, game[5])
}
gold_game_id_list <- type.convert(gold_game_id_list, as.is = TRUE)
print(gold_game_id_list)


# Grab all IDs of the games in the silver category

silver_game_urls <- top_games_2024_url %>%
  html_elements(xpath ="/html/body/div[1]/div[7]/div[7]/div[3]/div[2]/div/div/div/div/div/div[2]/div/div/div[2]/div[2]/div[6]/div/div[3]") %>%
  html_children() %>%
  html_elements("a") %>%
  html_attr("href") %>%
  unique()
print(silver_game_urls)

silver_game_spliced <- strsplit(silver_game_urls, "/")
silver_game_id_list <- c()
for (game in silver_game_spliced)
{
  silver_game_id_list <- c(silver_game_id_list, game[5])
}
silver_game_id_list <- type.convert(silver_game_id_list, as.is = TRUE)
print(silver_game_id_list)
