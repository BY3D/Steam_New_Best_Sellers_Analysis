# This R script collects the data of the best-selling new games on Steam.
# The years 2022 to 2024 are collected. 2025 will be collected when available
# The collected data is then assembled into one tibble (data frame/table)
# The tibble is exported as a CSV file

library(tidyverse)
library(polite)
library(rvest)
library(httr2)
library(rjson)



# Part 1.1 - Collecting the Top Selling Games -----------------------------


# Grab all IDs of the games

# Constants

TOP_GAMES_2024_URL      <-  "https://store.steampowered.com/charts/bestofyear/bestof2024?tab=2"
PLAT_GAMES_2024_XPATH   <-  "/html/body/div[1]/div[7]/div[7]/div[3]/div[2]/div/div/div/div/div/div[2]/div/div/div[2]/div[2]/div[4]/div/div[3]"
GOLD_GAMES_2024_XPATH   <-  "/html/body/div[1]/div[7]/div[7]/div[3]/div[2]/div/div/div/div/div/div[2]/div/div/div[2]/div[2]/div[5]/div/div[3]"
SILVER_GAMES_2024_XPATH <-  "/html/body/div[1]/div[7]/div[7]/div[3]/div[2]/div/div/div/div/div/div[2]/div/div/div[2]/div[2]/div[6]/div/div[3]"

TOP_GAMES_2023_URL      <-  "https://store.steampowered.com/charts/bestofyear/BestOf2023?tab=2"
PLAT_GAMES_2023_XPATH   <-  "/html/body/div[1]/div[7]/div[7]/div[3]/div[2]/div/div/div/div/div/div[2]/div/div/div/div[2]/div[4]/div/div[3]"
GOLD_GAMES_2023_XPATH   <-  "/html/body/div[1]/div[7]/div[7]/div[3]/div[2]/div/div/div/div/div/div[2]/div/div/div/div[2]/div[5]/div/div[3]"
SILVER_GAMES_2023_XPATH <-  "/html/body/div[1]/div[7]/div[7]/div[3]/div[2]/div/div/div/div/div/div[2]/div/div/div/div[2]/div[6]/div/div[3]"

TOP_GAMES_2022_URL      <-  "https://store.steampowered.com/charts/bestofyear/BestOf2022?tab=2"
PLAT_GAMES_2022_XPATH   <-  "/html/body/div[1]/div[7]/div[7]/div[3]/div[2]/div/div/div/div/div/div[2]/div/div/div/div[2]/div[4]/div/div[3]"
GOLD_GAMES_2022_XPATH   <-  "/html/body/div[1]/div[7]/div[7]/div[3]/div[2]/div/div/div/div/div/div[2]/div/div/div/div[2]/div[5]/div/div[3]"
SILVER_GAMES_2022_XPATH <-  "/html/body/div[1]/div[7]/div[7]/div[3]/div[2]/div/div/div/div/div/div[2]/div/div/div/div[2]/div[6]/div/div[3]"


# Functions

# To access an individual game on the Steam webpage:
# div class = "Panel Focusable" -> All games in a metal category (platinum, gold, silver)
# div class = "center_row_trgt ItemCount_3" -> Three games per row in the category
# div class = "" -> div of game
# div class = "... Panel Focusable"
# div class = "ImpressionTrackedElement"
# div class = "..." data-key = "hover div"
# anchor to game -> url to the actual game
# Note: Duplicates of the same links will be captured, hence the unique() later on

# This function returns the Steam IDs of the games in a top selling category 
# Input: URL to best selling games, XPath to best selling category
# Output: Integer vector
get_game_ids <- function(top_games_xml, category_xpath)
{
  game_urls <- top_games_xml %>%
    html_elements(xpath = category_xpath) %>%
    html_children() %>%
    html_elements("a") %>%
    html_attr("href") %>%
    unique()
  game_urls_spliced <- strsplit(game_urls, "/")
  game_ids <- c()
  for (game_url in game_urls_spliced)
  {
    game_ids <- c(game_ids, game_url[5])
  }
  game_ids <- type.convert(game_ids, as.is = TRUE)
  return(game_ids)
}

# This function returns a vector of the Steam Top Selling Games URLs
# Input: Strings
# Output: Character vector
assemble_steam_url_vector <- function()
{
  return(c(TOP_GAMES_2022_URL, TOP_GAMES_2023_URL, TOP_GAMES_2024_URL))
}

# This function returns a vector of the Steam Top Selling Games XPaths
# Input: Strings
# Output: Character vector
assemble_steam_category_xpath_vector <- function()
{
  return(c(PLAT_GAMES_2022_XPATH, GOLD_GAMES_2022_XPATH, SILVER_GAMES_2022_XPATH,
           PLAT_GAMES_2023_XPATH, GOLD_GAMES_2023_XPATH, SILVER_GAMES_2023_XPATH,
           PLAT_GAMES_2024_XPATH, GOLD_GAMES_2024_XPATH, SILVER_GAMES_2024_XPATH)
         )
}

# Returns the top selling category of a game
# Input: Integer
# Output: string
get_category <- function(number)
{
  if ((number %% 3) == 1) return("Platinum")
  else if ((number %% 3) == 2) return("Gold")
  else if ((number %% 3) == 0) return("Silver")
}


# Collect game IDs of 2024

#plat_game_ids_2024 <- get_game_ids(TOP_GAMES_2024_URL, PLAT_GAMES_2024_XPATH)
#gold_game_ids_2024 <- get_game_ids(TOP_GAMES_2024_URL, GOLD_GAMES_2024_XPATH)
#silver_game_ids_2024 <- get_game_ids(TOP_GAMES_2024_URL, SILVER_GAMES_2024_XPATH)

# Collect game IDs of 2023

#plat_game_ids_2023 <- get_game_ids(TOP_GAMES_2023_URL, PLAT_GAMES_2023_XPATH)
#gold_game_ids_2023 <- get_game_ids(TOP_GAMES_2023_URL, GOLD_GAMES_2023_XPATH)
#silver_game_ids_2023 <- get_game_ids(TOP_GAMES_2023_URL, SILVER_GAMES_2023_XPATH)

# Collect game IDs of 2022

#plat_game_ids_2022 <- get_game_ids(TOP_GAMES_2022_URL, PLAT_GAMES_2022_XPATH)
#gold_game_ids_2022 <- get_game_ids(TOP_GAMES_2022_URL, GOLD_GAMES_2022_XPATH)
#silver_game_ids_2022 <- get_game_ids(TOP_GAMES_2022_URL, SILVER_GAMES_2022_XPATH)



# Part 1.2 - Collecting the Data of Each Game -----------------------------


# So far we have the Steam IDs of each game, we need to collect the following:
# Game Title
# Game Release Date
# Game Cover (maybe)
# Game Genre
# Price (in USD ideally)
# Country of Origin (may not be possible)
# User rating percentage 
# Absolute number of reviews
# PEGI Age Rating
# Number of Languages Supported
# Recommended Graphics Card

# To collect user ratings of the game's release year: https://store.steampowered.com/appreviewhistogram/(appid)


# Constants

# URLs
STEAM_GAME_PAGE <- "https://store.steampowered.com/app/"
STEAM_USER_RATINGS_URL <- "https://store.steampowered.com/appreviewhistogram/"

# Sample Games
BLACK_MYTH <- 2358720
LEGO_STAR_WARS <- 920210
ENSHROUDED <- 1203620

# HTML Attributes, Classes, and IDs
GAME_TITLE_HTML_ID <- "#appHubAppName"                                  # div
RELEASE_DATE_HTML_CLASS <- ".release_date"                              # div
GAME_GENRE_HTML_ATTR <- "data-panel=\'{\"flow-children\":\"row\"}\'"    # span
GAME_PRICE_HTML_CLASS <- ".game_purchase_action_bg"                     # div
AGE_RATING_HTML_CLASS <- ".game_rating_icon"                            # div
LANGUAGES_HTML_ID <- "#languageTable"                                   # div
GRAPHICS_HTML_CLASS <- ".game_area_sys_req_rightCol"                    # div
PUBLISHER_HTML_CLASS <- ".dev_row"                                      # div

# HTML XPaths and CSS Selectors
GAME_TITLE_XPATH <- "/html/body/div[1]/div[7]/div[7]/div[3]/div[2]/div[1]/div[2]/div[2]/div[2]/div"                                     # <div class="apphub_HeaderStandardTop">
RELEASE_DATE_XPATH <- "/html/body/div[1]/div[7]/div[7]/div[3]/div[2]/div[1]/div[2]/div[4]/div/div[3]/div[1]/div/div[2]/div[1]/div[2]"   # <div class="release_date">
GAME_GENRE_CSS_PATH <- "#genresAndManufacturer > span:nth-child(4)"                                                                     # <span data-panel="{"flow-children":"row"}">
GAME_PRICE_XPATH <- "/html/body/div[1]/div[7]/div[7]/div[3]/div[2]/div[1]/div[4]/div[2]/div[1]/div[1]/div/div[2]/div"                   # <div class="game_purchase_action_bg">
GAME_PRICE_DISCOUNT_XPATH <- "/html/body/div[1]/div[7]/div[7]/div[3]/div[2]/div[1]/div[4]/div[2]/div[2]/div[1]/div/div[2]/div"          # <div class="game_purchase_action_bg">
AGE_RATING_XPATH <- "/html/body/div[1]/div[7]/div[7]/div[3]/div[2]/div[1]/div[4]/div[1]/div[9]/div[2]/div[1]/div[1]"                    # Currently attempts to get ESRB rating
LANGUAGES_XPATH <- "/html/body/div[1]/div[7]/div[7]/div[3]/div[2]/div[1]/div[4]/div[1]/div[7]/div[3]/table"
GRAPHICS_XPATH <- "/html/body/div[1]/div[7]/div[7]/div[3]/div[2]/div[1]/div[4]/div[2]/div[10]/div[1]/div/div/div[2]/ul/ul/li[5]"
USER_RATING_XPATH <- "/html/body/div[1]/div[7]/div[7]/div[3]/div[2]/div[1]/div[6]/div/div/div[2]/div[3]/div[1]/span[2]"


# Functions

# This function returns the title of the game
# Input: HTML Webpage (XML Nodeset)
# Output: String
get_game_title <- function(game_xml)
{
  title <- game_xml %>%
    html_element(GAME_TITLE_HTML_ID) %>%
    html_text2()
  return(title)
}

# This function returns the publisher of the game
# Input: HTML Webpage (XML Nodeset)
# Output: String
get_game_publisher <- function(game_xml)
{
  divs <- game_xml %>% html_elements(PUBLISHER_HTML_CLASS)
  publisher <- ""
  for (div in divs)
  {
    txt <- div %>% html_text2()
    if (str_detect(txt, "Publisher"))
    {
      publisher <- div %>% 
        html_element("a") %>% 
        html_text2()
      break
    }
  }
  return(publisher)
}

# This function returns the genre tags of the game
# Input: HTML Webpage (XML Nodeset)
# Output: Character vector
get_game_genres <- function(game_xml)
{
  genre <- game_xml %>% 
    html_element(css = GAME_GENRE_CSS_PATH) %>%
    html_elements("a") %>%
    html_text2() %>%
    unlist()
  return(genre)
}

# This function returns the release date of the game
# Input: HTML Webpage (XML Nodeset)
# Output: Date scalar
get_game_date <- function(game_xml)
{
  date_xml <- game_xml %>%
    html_elements(RELEASE_DATE_HTML_CLASS) %>%
    html_children()
  release_date <- date_xml[2] %>%
    html_text2() %>%
    as.Date(format = "%d %b, %Y")
  return(release_date)
}

# This function returns the price of a game
# Input: HTML Webpage (XML Nodeset)
# Output: Integer scalar
get_game_price <- function(game_xml)
{
  if (!is.na(game_xml %>% html_element("#freeGameBtn"))) return(0)
  price_html <- game_xml %>%
    html_elements(css = GAME_PRICE_HTML_CLASS)
  for (node in price_html)
  {
    elem <- node %>% html_element("div")
    if (is.na(elem %>% html_attr("data-price-final"))) next
    else 
      {
        price_html <- elem
        break
      }
  }
  if (length(price_html) == 0) return(NA)
  game_price <- price_html %>% 
    html_attr("data-price-final") %>%
    as.double() / 100
  # If the game is currently discounted, obtain its original price
  # Use this formula: discounted price / (1 - discount percent)
  discount <- price_html %>% html_attr("data-discount")
  if (!is.na(discount)[1])
  {
    discount <- (100 - as.double(discount)) / 100
    game_price <- game_price / discount
  }
  return(ceiling(game_price))
}

# Returns the age rating of the game
# Input: HTML webpage (XML Nodeset)
# Output: Integer scalar
get_age_rating <- function(game_xml)
{
  # This function assumes PEGI age ratings are being sought after
  age <- game_xml %>% 
    html_element(AGE_RATING_HTML_CLASS) %>%
    html_element("img") %>%
    html_attr("alt")
  if (!is.na(age))
  {
    age <- age %>% str_sub(1, str_length(age) - 1)
    age <- as.integer(age)
  }
  return(age)
}

# This function returns languages with interface or spoken audio support
# Input: HTML webpage (XML Nodeset), boolean for either interface or spoken audio
# Output: Character vector
get_languages <- function(game_xml, interface = TRUE)
{
  languages <- c()
  column <- ifelse((interface), 2, 3) # column 2 is interface, column 3 is audio support
  lang_table <- game_xml %>%
    html_element(LANGUAGES_HTML_ID) %>%
    html_table(trim = TRUE)
  for (i in 1:nrow(lang_table))
  {
    if (is.na(lang_table[i, column])) next
    if (lang_table[i, column] != "")
    {
      languages <- c(languages, lang_table[i, 1])
    }
  }
  languages <- unlist(languages)
  if (is.null(languages) || length(languages) == 0) return(NA)
  else return(languages)
}

# This function returns the recommended GPUs of a game
# Input: Character vector
# Output: Character vector
get_recom_graphics <- function(game_xml)
{
  requirements <- game_xml %>%
    html_element(GRAPHICS_HTML_CLASS) %>%
    html_elements("ul")
  recommend_req <- requirements[2]
  req_list <- recommend_req %>% html_elements("li")
  for (li_element in req_list)
  {
    if (str_detect(li_element %>% html_text2(), "Graphics:"))
    {
      gpus_raw <- li_element
      break
    }
  }
  if (!exists("gpus_raw")) return(NA)
  gpus_raw <- gpus_raw %>% 
    html_text2() %>%
    str_split(regex(" |/")) %>%
    unlist()
  gpus <- combine_gpu_name(gpus_raw)
  if (length(gpus) < 1) return(NA)
  return(gpus)
}

# This function returns a vector of GPU names
# Input: Character vector
# Output: Character vector
combine_gpu_name <- function(string_vector)
{
  # GPU naming convention: Company Brand Line-up Model-number Label
  # Examples: NVIDIA GeForce RTX 2070 Super, AMD Radeon VII, Intel Arc A750 
  graphic_cards <- c()
  i <- 1
  len <- length(string_vector)
  while (i < len)
  {
    if (str_detect(string_vector[i], regex("nvidia|geforce", ignore_case = TRUE)))
    {
      end <- ifelse((i + 3) < len, (i + 3), len)
      name <- string_vector[i:end] # Copy NVIDIA GPU. Longest possible name is NVIDIA GeForce GTX ####
      j <- str_which(name, "[:digit:]") # End the string by the model number
      j <- j[1]
      j <- ifelse(str_detect(string_vector[i + j], regex("ti|super|gt", ignore_case = TRUE)),
                  j + 1, j) # Account for Ti/Super labels
      j <- j[1]
      if (is.na(j)) j <- str_which(name, "[:digit:]")
      j <- j[1]
      name <- paste(string_vector[i:(i + j - 1)], collapse = " ")
      name <- clean_graphics_card_name(name)
      graphic_cards <- c(graphic_cards, name)
      i <- i + j
    }
    else if (str_detect(string_vector[i], regex("amd|ati|radeon", ignore_case = TRUE)))
    {
      end <- ifelse((i + 4) < len, (i + 4), len)
      name <- string_vector[i:end] # Copy AMD GPU. Longest possible name is AMD Radeon RX VEGA ####
      j <- str_which(name, "[:digit:]") # End the string by the model number
      j <- j[1]
      j <- ifelse(str_detect(string_vector[i + j], regex("xt", ignore_case = TRUE)), 
                  j + 1, j) # Account for XT label
      j <- j[1]
      j <- ifelse(is.na(j), str_which(name, "VII"), j) # Without this, the AMD Radeon VII is left out
      j <- j[1]
      if (is.na(j)) j <- str_which(name, "[:digit:]")
      if (is.na(j[1])) return(NA)
      name <- paste(string_vector[i:(i + j - 1)], collapse = " ") # j - 1 as paste will go beyond what's expected otherwise
      name <- clean_graphics_card_name(name)
      graphic_cards <- c(graphic_cards, name)
      i <- i + j
    }
    else if (str_detect(string_vector[i], regex("intel|arc", ignore_case = TRUE)))
    {
      end <- ifelse((i + 3) < len, (i + 3), len)
      name <- string_vector[i:end] # Copy Intel GPU. Longest possible name is Intel Arc ####
      j <- str_which(name, regex("[:digit:]")) # End the string by the model number
      j <- j[1]
      name <- paste(string_vector[i:(i + j - 1)], collapse = " ")
      name <- clean_graphics_card_name(name)
      graphic_cards <- c(graphic_cards, name)
      i <- i + j
    }
    else
    {
      i <- i + 1
    }
  }
  return(graphic_cards)
}

# This function returns a cleaned-up graphics card string
# Input: String
# Output: String
clean_graphics_card_name <- function(gpu_string)
{
  gpu_string <- gpu_string %>% str_replace_all("-", " ")
  gpu_string <- gpu_string %>% str_remove_all(":")
  gpu_string <- gpu_string %>% str_remove_all(",")
  gpu_string <- gpu_string %>% str_remove_all(" or")
  gpu_string <- gpu_string %>% str_remove_all("®")
  gpu_string <- gpu_string %>% str_remove_all("™")
  gpu_string <- gpu_string %>% str_trim()
  return(gpu_string)
}

# This function returns the total positive and negative user ratings of a game
# Over the game's first month of release
# Input: Steam ID of game, release date of game
# Output: Integer vector
get_user_ratings <- function(game_id, release_date)
{
  user_ratings <- c(0, 0) # Column 1 is positive ratings, column 2 is negative ratings
  json_url <- paste0(STEAM_USER_RATINGS_URL, game_id)
  raw_ratings <- fromJSON(file = json_url)
  num_of_periods <- length(raw_ratings$results$rollups)
  for (i in 1:num_of_periods)
  {
    period_date <- as.Date.POSIXct(raw_ratings$results$rollups[[i]]$date)
    date_diff <- abs(as.integer(period_date - release_date))
    if (date_diff > 31) next
    user_ratings[1] <- user_ratings[1] + raw_ratings$results$rollups[[i]]$recommendations_up
    user_ratings[2] <- user_ratings[2] + raw_ratings$results$rollups[[i]]$recommendations_down
  }
  return(user_ratings)
}


# Part 1.3 - Test the Functions -------------------------------------------

test_all_get_functions <- function()
{
  temp_xml <- read_html_live("https://store.steampowered.com/charts/bestofyear/bestof2024?tab=2")
  temp_xpath <- "/html/body/div[1]/div[7]/div[7]/div[3]/div[2]/div/div/div/div/div/div[2]/div/div/div[2]/div[2]/div[4]/div/div[3]"
  temp_xpath2 <- "/html/body/div[1]/div[7]/div[7]/div[3]/div[2]/div/div/div/div/div/div[2]/div/div/div[2]/div[2]/div[5]/div/div[3]"
  # URL to use for agecheck: https://store.steampowered.com/agecheck/app/(appid)
  game_ids_to_test <- get_game_ids(temp_xml, temp_xpath2)
  for (game in game_ids_to_test)
  {
    game_url <- paste0("https://store.steampowered.com/app/", game)
    #steam_session <- bow(game_url, delay = 6)
    #game_xml <- scrape(steam_session)
    game_xml <- read_html_live(game_url)
    Sys.sleep(3)
    #game_xml$view()
    if (!is.na(game_xml %>% html_element(".agegate_btn_ctn")))
    {
      # Press the "View Page" anchor element, then nod to the game page
      #age_session <- session(game_url) %>% session_follow_link(css = "#view_product_page_btn")
      #game_xml$click(css = "#view_product_page_btn")
      #steam_session <- steam_session %>%
      #nod(game_url)
      game_xml$click(css = "#ageYear")
      for (i in 1:25)
      {
        game_xml$press(css = "#ageYear", "ArrowUp")
      }
      game_xml$click(css = "#ageYear")
      game_xml$click(css = "#view_product_page_btn")
    }
    print(paste0("ID: ", game))
    title <- game_xml %>%
      html_element(GAME_TITLE_HTML_ID) %>%
      html_text2()
    print(paste0("Title: ", title))
    print("Game Publisher ")
    print(get_game_publisher(game_xml))
    publish_date <- get_game_date(game_xml)
    print(paste0("Date Published: ", publish_date))
    print(paste0("Price ", get_game_price(game_xml)))
    genre <- game_xml %>% 
      html_element(css = GAME_GENRE_CSS_PATH) %>%
      html_elements("a") %>%
      html_text2()
    print("Genre ")
    print(genre)
    print("Age Rating")
    print(get_age_rating(game_xml))
    print("Languages ")
    print(get_languages(game_xml))
    print("Recommended Graphics ")
    print(get_recom_graphics(game_xml))
    print("Total User Ratings")
    print(get_user_ratings(game, publish_date))
    print("")
  }
}



# Part 2 - Assemble the Tibbles -------------------------------------------


# This function returns a tibble containing Game IDs, Year Published, and Top Selling Category
# Input: Vector of URLs (strings), vector of XPaths (strings)
# Output: n x 3 Tibble
create_top_game_base_tibble <- function(top_game_urls, top_seller_xpaths)
{
  id                <- c()  # Game ID
  year_published    <- c()  # Year Launched
  top_sell_category <- c()  # Top-Seller Category
  MAX_I <- length(top_game_urls) # number of years to loop through
  multiplier <- 1
  for (i in 1:MAX_I)
  {
    top_game_xml <- read_html_live(top_game_urls[i])
    for (j in multiplier:(multiplier + 2))
    {
      game_ids <- get_game_ids(top_game_xml, top_seller_xpaths[j])
      id <- c(id, game_ids)
      year <- switch(i, 2022, 2023, 2024)
      category <- get_category(j)
      for (k in 1:length(game_ids))
      {
        year_published <- c(year_published, year)
        top_sell_category <- c(top_sell_category, category)
      }
    }
    mutliplier <- multiplier + 3
    Sys.sleep(3)
  }
  base_tibble <- tibble(id, year_published, top_sell_category)
  return(base_tibble)
}

# This function returns a tibble containing records of the top-selling games
# Input: Tibble of ID, Year Published, and Top Seller Category
# Output: n x 11 Tibble with Title, Month Published, Year Published,
#         Top Selling Category, Genre, Interface Languages, Spoken Languages,
#         Recommended Graphics, Positive User Ratings, Negative User Ratings
create_partial_top_game_tibble <- function(base_tibble)
{
  id                    <- c()      # Game ID
  title                 <- c()      # Game Title
  month_published       <- c()      # Month Launched
  genre                 <- c()      # Genres
  languages_interface   <- c()      # Supported Languages
  languages_spoken      <- c()      # Audio Supported Languages
  recommended_graphics  <- c()      # Recommended Graphics Cards
  #genre                 <- list()   # Genres
  #languages_interface   <- list()   # Supported Languages
  #languages_spoken      <- list()   # Audio Supported Languages
  #recommended_graphics  <- list()   # Recommended Graphics Cards
  user_ratings_positive <- c()      # Total Positive User Ratings
  user_ratings_negative <- c()      # Total Negative User Ratings
  for (game_id in base_tibble$id)
  {
    game_xml <- read_html_live(paste0(STEAM_GAME_PAGE, game_id))
    # If the age check exists, then set an age to move on to the store page
    if (!is.na(game_xml %>% html_element(".agegate_btn_ctn")))
    {
      game_xml$click(css = "#ageYear")
      for (i in 1:25)
      {
        game_xml$press(css = "#ageYear", "ArrowUp")
      }
      game_xml$click(css = "#ageYear")
      game_xml$click(css = "#view_product_page_btn")
    }
    # Spacing for improved readability
    id <- c(id, game_id)
    
    title <- c(title, get_game_title(game_xml))
    
    publish_date <- get_game_date(game_xml)
    
    month_published <- c(month_published, format(publish_date, "%m"))
    
    #genre[[length(genre) + 1]] <- get_game_genres(game_xml)
    genre <- c(genre, paste(unlist(get_game_genres(game_xml)), collapse = "|"))
    
    #languages_interface[[length(languages_interface) + 1]] <- get_languages(game_xml)
    languages_interface <- c(languages_interface, paste(unlist(get_languages(game_xml)), collapse = "|"))
    
    #languages_spoken[[length(languages_spoken) + 1]] <- get_languages(game_xml, FALSE)
    languages_spoken <- c(languages_spoken, paste(unlist(get_languages(game_xml, FALSE)), collapse = "|"))
    
    #recommended_graphics[[length(recommended_graphics) + 1]] <- get_recom_graphics(game_xml)
    recommended_graphics <- c(recommended_graphics, paste(unlist(get_recom_graphics(game_xml)), collapse = "|"))
    
    user_ratings <- get_user_ratings(game_id, publish_date)
    
    user_ratings_positive <- c(user_ratings_positive, user_ratings[1])
    
    user_ratings_negative <- c(user_ratings_negative, user_ratings[2])
    
    Sys.sleep(1)
  }
  
  # Create the current primary tibble
  partial_tibble <- tibble(
    id, 
    title, 
    month_published, 
    genre, 
    languages_interface, 
    languages_spoken, 
    recommended_graphics, 
    user_ratings_positive, 
    user_ratings_negative
    )
  write_csv(partial_tibble, "partial_tibble.csv")
  return(partial_tibble)
}

# This function writes a formatted partial tibble to a CSV file
# Input: Tibble, string
# Output: CSV file
format_tibble_to_CSV <- function(partial_tib, file_name)
{
  tib <- partial_tib %>% mutate("genre" = paste(unlist(partial_tib$genre), collapse = "|"))
  tib <- tib %>% mutate("language_interface" = paste(unlist(partial_tib$language_interface), collapse = "|"))
  tib <- tib %>% mutate("language_spoken" = paste(unlist(partial_tib$language_spoken), collapse = "|"))
  tib <- tib %>% mutate("recommended_graphics" = paste(unlist(partial_tib$recommended_graphics), collapse = "|"))
  write_csv(tib, file_name)
}

# This function returns the base tibble
# Input: Nothing
# Output: n x 3 Tibble
create_base_tibble_dataset <- function()
{
  game_urls <- assemble_steam_url_vector()
  game_xpaths <- assemble_steam_category_xpath_vector()
  return(create_top_game_base_tibble(game_urls, game_xpaths))
}

# This function returns the price tibble
# Input: Tibble
# Output: CSV file and Tibble
create_price_tibble <- function(base_tibble)
{
  id        <- c()  # Game ID
  price_USD <- c()  # Game Price in USD
  for (game_id in base_tibble$id)
  {
    game_xml <- read_html_live(paste0(STEAM_GAME_PAGE, game_id))
    if (!is.na(game_xml %>% html_element(".agegate_btn_ctn")))
    {
      # If the age check exists, then set an age to move on to the store page
      game_xml$click(css = "#ageYear")
      for (i in 1:25)
      {
        game_xml$press(css = "#ageYear", "ArrowUp")
      }
      game_xml$click(css = "#ageYear")
      game_xml$click(css = "#view_product_page_btn")
    }
    id <- c(id, game_id)
    price_USD <- c(price_USD, get_game_price(game_xml)[1])
    Sys.sleep(3)
  }
  price_tibble <- tibble(id, price_USD)
  #primary_tibble <- left_join(base_tibble, price_tibble, by = "id")
  write_csv(price_tibble, "price_tibble.csv")
  return(price_tibble)
}

# This function returns the age rating tibble
# Input: Tibble
# Output: CSV file
create_age_rating_tibble <- function(base_tibble)
{
  id                <- c()  # Game ID
  age_rating_PEGI   <- c()  # PEGI Age Rating
  for (game_id in base_tibble$id)
  {
    game_xml <- read_html_live(paste0(STEAM_GAME_PAGE, game_id))
    if (!is.na(game_xml %>% html_element(".agegate_btn_ctn")))
    {
      # If the age check exists, then set an age to move on to the store page
      game_xml$click(css = "#ageYear")
      for (i in 1:25)
      {
        game_xml$press(css = "#ageYear", "ArrowUp")
      }
      game_xml$click(css = "#ageYear")
      game_xml$click(css = "#view_product_page_btn")
    }
    id <- c(id, game_id)
    age_rating_PEGI <- c(age_rating_PEGI, get_age_rating(game_xml))
    Sys.sleep(3)
  }
  age_tibble <- tibble(id, age_rating_PEGI)
  write_csv(age_tibble, "age_tibble.csv")
  return(age_tibble)
}

# This function returns the publisher tibble
# Input: Tibble
# Output: CSV file
create_publisher_tibble <- function(base_tibble)
{
  id        <- c()  # Game ID
  publisher <- c()  # Publisher of Game
  for (game_id in base_tibble$id)
  {
    game_xml <- read_html_live(paste0(STEAM_GAME_PAGE, game_id))
    if (!is.na(game_xml %>% html_element(".agegate_btn_ctn")))
    {
      # If the age check exists, then set an age to move on to the store page
      game_xml$click(css = "#ageYear")
      for (i in 1:25)
      {
        game_xml$press(css = "#ageYear", "ArrowUp")
      }
      game_xml$click(css = "#ageYear")
      game_xml$click(css = "#view_product_page_btn")
    }
    id <- c(id, game_id)
    publisher <- c(publisher, get_game_publisher(game_xml))
    Sys.sleep(1)
  }
  pub_tibble <- tibble(id, publisher)
  write_csv(pub_tibble, "pub_tibble.csv")
  return(pub_tibble)
}

# This function produces the top-selling new games on Steam
# Input: Primary Tibble of Data, Tibble of Prices, and Tibble of Age Ratings
# Output: CSV file
create_complete_dataset <- function()
{
  # Join the tibbles
  partial_tib <- read_csv("Data/partial_tibble.csv")
  base_tib <- read_csv("Data/base_tibble.csv")
  age_tib <- read_csv("Data/age_tibble.csv")
  price_tib <- read_csv("Data/price_tibble.csv")
  publish_tib <- read_csv("Data/pub_tibble.csv")
  bp_tib <- inner_join(base_tib, partial_tib, by = join_by("id"))
  bpa_tib <- inner_join(bp_tib, age_tib, by = join_by("id"))
  bpas_tib <- inner_join(bpa_tib, price_tib, by = join_by("id"))
  bpasp_tib <- inner_join(bpas_tib, publish_tib, by = join_by("id"))
  # Format the final tibble
  bpasp_tib <- distinct(bpasp_tib)
  bpasp_tib <- relocate(bpasp_tib, "month_published", .before = "year_published")
  bpasp_tib <- relocate(bpasp_tib, "title", .after = "id")
  bpasp_tib <- relocate(bpasp_tib, "age_rating_PEGI", .after = "genre")
  bpasp_tib <- relocate(bpasp_tib, "price_USD", .after = "top_sell_category")
  bpasp_tib <- relocate(bpasp_tib, "publisher", .after = "title")
  write_csv(bpasp_tib, "Data/Steam_Top_Selling_New_Games.csv")
  return(bpasp_tib)
}

# This function returns a tibble of a CSV file dataset
# Input: File name and path (string)
# Output: Tibble
import_dataset <- function(tibble_file)
{
  data <- read_csv(tibble_file)
  return(data)
}
