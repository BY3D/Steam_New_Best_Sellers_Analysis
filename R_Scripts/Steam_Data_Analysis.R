# This R script analyses the data from the Steam New Best Selling Games dataset

library(tidyverse)

Steam_Data <- read_csv("Data/Steam_Top_Selling_New_Games.csv")

# Questions to Answer
# Average price of game per category? For instance, are Platinum games more expensive than gold games?
# Average price of game per year? Are the game prices by category changing?
# Most popular genres?
# Most common age ratings? Age ratings among the categories? For example, are 12-16+ games the most popular in gold games?
# Most common languages from the games? How is it among the top seller categories?
# Most recommended graphics cards? Do the recommendations change for each category and year?
# Most liked game among users? Do the ratings change depending on category?
# Most popular release month? Least popular release month?
# Most frequent publisher? Who is publishing the best selling games?
# Any causations to look out for?


# Calculate User Rating, add it as a dedicated column to the dataset
Steam_Data <- Steam_Data %>% mutate(
  user_ratings_total = user_ratings_positive + user_ratings_negative
  )
Steam_Data <- Steam_Data %>% mutate(
  user_ratings_pct = round(
    (user_ratings_positive / (user_ratings_positive + user_ratings_negative)), 
    digits = 2
    )
  )

write_csv(Steam_Data, "Data/Steam_Best_Selling_Games.csv")


# The Free to Play Games
ftp_games <- Steam_Data %>% filter(price_USD == 0)

# The De-listed Games
na_games <- Steam_Data %>% filter(is.na(price_USD))

# The Games by Year
games_2022 <- Steam_Data %>% filter(year_published == 2022)
games_2023 <- Steam_Data %>% filter(year_published == 2023)
games_2024 <- Steam_Data %>% filter(year_published == 2024)

# The Games by Top Seller Category
games_plat <- Steam_Data %>% filter(str_detect(top_sell_category, "Platinum"))
games_gold <- Steam_Data %>% filter(str_detect(top_sell_category, "Gold"))
games_silver <- Steam_Data %>% filter(str_detect(top_sell_category, "Silver"))



# Price Info and Plots ----------------------------------------------------


# Approximate exchange rates
# 1 USD : 1.4 CAD
# 1 USD : 0.85 EUR
# 1 USD : 0.75 GBP
# 1 USD : 1.55 AUD
# 1 USD : 155 JPY
# 1 USD : 7.1 CNY

# This function returns the mean, median, min, and max prices of the data
# Input: Tibble and either integer or string
# Output: Summary table. Access elements with list notation
# Note: [[1]] is min, [[3]] is median, [[4]] is mean, and [[6]] is max
get_price_info <- function(steam_data, year = NULL, category = NULL)
{
  if (!is.null(year))
  {
    game_prices <- steam_data %>% filter(year_published == year)
  }
  else if (!is.null(category))
  {
    game_prices <- steam_data %>% filter(str_detect(top_sell_category, category))
  }
  game_prices <- game_prices %>% filter(price_USD != 0)
  return(summary(game_prices$price_USD))
}

price_info_2022 <- get_price_info(Steam_Data, year = 2022)
price_info_2023 <- get_price_info(Steam_Data, year = 2023)
price_info_2024 <- get_price_info(Steam_Data, year = 2024)


price_info_plat <- get_price_info(Steam_Data, category = "Platinum")
price_info_gold <- get_price_info(Steam_Data, category = "Gold")
price_info_silver <- get_price_info(Steam_Data, category = "Silver")

# Making the Plots

# By Year

price_info_plot_year <- ggplot(data = filter(Steam_Data, price_USD != 0),
                               aes(x = price_USD,
                                   fill = fct_relevel(factor(top_sell_category), 
                                                      "Platinum"))) +
  geom_histogram(binwidth = 10,
                 colour = rgb(0, 0, 0, 1)) +
  scale_fill_brewer() +
  scale_x_continuous(labels = scales::label_currency()) +
  facet_wrap(~year_published) +
  theme_bw() + 
  labs(title = "Game Price Distribution by Year",
       x = "USD Price",
       y = "Number of Games",
       fill = "Top Selling Categories")

# Experimental Plots

price_info_plot_year_2 <- ggplot(data = filter(Steam_Data, price_USD != 0),
       aes(x = as.factor(year_published),
           y = price_USD)) + 
  geom_violin(scale = "area") +
  labs(title = "Game Price Distribution by Year",
       x = "Year",
       y = "USD") +
  scale_y_continuous(labels = scales::label_currency())

# By Top Selling Category
price_info_plot_cat <- ggplot(data = filter(Steam_Data, price_USD != 0),
       aes(x = as.factor(top_sell_category),
           y = price_USD)) + 
  geom_boxplot(width = 0.3) +
  labs(title = "Game Price Distribution by Top Selling Category",
       x = "Category",
       y = "$ USD") +
  theme_bw()



# Genre Info and Plots ----------------------------------------------------


# Find the most popular genres from all the games

# This function returns the sorted frequency of genres
# Input: Tibble
# Output: Table
get_popular_genres <- function(steam_data)
{
  all_genre_tags <- c()
  for (raw_genre_tags in steam_data$genre)
  {
    tags <- str_split_1(raw_genre_tags, coll("|"))
    all_genre_tags <- append(all_genre_tags, tags) 
  }
  return(sort(table(all_genre_tags), decreasing = FALSE))
}

most_pop_genres <- get_popular_genres(Steam_Data)

pop_genres_2022 <- get_popular_genres(games_2022)
pop_genres_2023 <- get_popular_genres(games_2023)
pop_genres_2024 <- get_popular_genres(games_2024)

pop_genres_plat <- get_popular_genres(games_plat)
pop_genres_gold <- get_popular_genres(games_gold)
pop_genres_silver <- get_popular_genres(games_silver)

# Making the Plots

# This function returns a bar chart of the most popular genre tags
# Input: Table
# Output: Plot
make_pop_genre_plot <- function(steam_data, plot_subtitle)
{
  plot <- ggplot(data = data.frame(steam_data),
                 aes(y = all_genre_tags,
                     x = Freq)) +
    geom_col(width = 0.7,
             position = "dodge2",
             fill = rgb(0.15, 0.26, 0.65),
             show.legend = FALSE) +
    theme_bw() +
    labs(title = "Most Popular Genre Tags",
         subtitle = plot_subtitle,
         y = "Genre",
         x = "Number of Games in the Genre")
  return(plot)
}

# Charts by Year
pop_genres_2022_plot <- make_pop_genre_plot(pop_genres_2022, "For 2022")
pop_genres_2023_plot <- make_pop_genre_plot(pop_genres_2023, "For 2023")
pop_genres_2024_plot <- make_pop_genre_plot(pop_genres_2024, "For 2024")

most_pop_genre_plot <- ggplot(data = data.frame(most_pop_genres),
                              aes(y = all_genre_tags,
                                  x = Freq)) +
  geom_col(width = 0.7,
           position = "dodge2",
           #colour = rgb(0, 0, 0),
           fill = rgb(0.15, 0.26, 0.65),
           show.legend = FALSE) +
  theme_bw() +
  labs(title = "Most Popular Genre Tags",
       y = "Genre",
       x = "Number of Games in the Genre")

# Pie charts are a bit complex and don't show the data well. A bar chart is sufficient
most_pop_genre_plot_pie <- ggplot(data = data.frame(most_pop_genres),
                                  aes(x = "",
                                      y = Freq,
                                      fill = all_genre_tags)) +
  geom_col(width = 1, 
           colour = "black",
           show.legend = TRUE) +
  coord_radial(theta = "y", 
               expand = FALSE) +
  labs(title = "Most Popular Genre Tags",
       x = "",
       y = "")



# Age Rating Info and Plots -----------------------------------------------


# This function returns the sorted frequency of age ratings
# Input: Tibble
# Output: Table
get_age_rating_freq <- function(steam_data)
{
  return(sort(table(steam_data$age_rating_PEGI), decreasing = TRUE))
}

pegi_ratings_2022 <- get_age_rating_freq(games_2022)
pegi_ratings_2023 <- get_age_rating_freq(games_2023)
pegi_ratings_2024 <- get_age_rating_freq(games_2024)

pegi_ratings_plat <- get_age_rating_freq(games_plat)
pegi_ratings_gold <- get_age_rating_freq(games_gold)
pegi_ratings_silver <- get_age_rating_freq(games_silver)

# Making the Plots

age_ratings_plot <- ggplot(data = filter(Steam_Data, !is.na(age_rating_PEGI)),
                           aes(x = factor(age_rating_PEGI),
                               fill = fct_relevel(factor(top_sell_category), 
                                                  "Platinum"))) +
  geom_bar(colour = rgb(0, 0, 0)) +
  scale_fill_brewer() +
  facet_wrap(~year_published) +
  theme_bw() + 
  labs(title = "Game Age Rating Distribution",
       subtitle = "By Year and Revenue Category",
       x = "PEGI Age Rating",
       y = "Number of Games",
       fill = "Top Selling Categories")

# Experimental Plots
age_ratings_2023_plot <- ggplot(data = data.frame(pegi_ratings_2023), 
       aes(y = factor(1),
           #x = Var1,
           fill = factor(Var1))) +
  geom_bar() + 
  coord_radial(expand = FALSE) +
  labs(title = "2023 Games",
       x = "Age Rating",
       y = "Number of Games")



# Supported Languages Info and Plots --------------------------------------


# This function returns the frequency of the interface languages or spoken ones
# Input: Tibble and string
# Output: Table
get_lang_freq <- function(steam_data, aspect)
{
  all_langs <- c()
  if (str_detect(aspect, "languages_interface")) selection <- steam_data$languages_interface
  else if (str_detect(aspect, "languages_spoken")) selection <- steam_data$languages_spoken
  for (raw_langs in selection)
  {
    if (is.na(raw_langs)) next
    langs <- str_split_1(raw_langs, coll("|"))
    all_langs <- append(all_langs, langs) 
  }
  return(sort(table(all_langs), decreasing = FALSE))
}

top_interface_languages <- get_lang_freq(Steam_Data, "languages_interface")
top_spoken_languages <- get_lang_freq(Steam_Data, "languages_spoken")

top_inter_langs_2022 <- get_lang_freq(games_2022, "languages_interface")
top_inter_langs_2023 <- get_lang_freq(games_2023, "languages_interface")
top_inter_langs_2024 <- get_lang_freq(games_2024, "languages_interface")

top_spoke_langs_2022 <- get_lang_freq(games_2022, "languages_spoken")
top_spoke_langs_2023 <- get_lang_freq(games_2023, "languages_spoken")
top_spoke_langs_2024 <- get_lang_freq(games_2024, "languages_spoken")

top_inter_langs_plat <- get_lang_freq(games_plat, "languages_interface")
top_inter_langs_gold <- get_lang_freq(games_gold, "languages_interface")
top_inter_langs_silver <- get_lang_freq(games_silver, "languages_interface")

top_spoke_langs_plat <- get_lang_freq(games_plat, "languages_spoken")
top_spoke_langs_gold <- get_lang_freq(games_gold, "languages_spoken")
top_spoke_langs_silver <- get_lang_freq(games_silver, "languages_spoken")

# Making the Plots

make_lang_plot <- function(steam_data, plot_title, plot_subtitle)
{
  game_count <- as.numeric(count(Steam_Data))
  language_plot <- ggplot(data = data.frame(steam_data),
                          aes(y = all_langs,
                              x = Freq,
                              fill = all_langs)) +
    geom_col(width = 0.7,
             position = "dodge2",
             fill = rgb(0.15, 0.26, 0.65),
             show.legend = FALSE) +
    geom_text(aes(label = paste0(round(Freq/150 * 100), "%"), hjust = -0.3)) +
    theme_minimal() +
    labs(title = plot_title,
         subtitle = plot_subtitle,
         y = "Languages",
         x = "Number of Games Supporting the Language")
  return(language_plot)
}

# Spoken Languages Plot by Top Seller Category

spoken_langs_plat_plot <- make_lang_plot(top_spoke_langs_plat, 
                                         "Games with Full Audio Support",
                                         "For Platinum Games")

spoken_langs_gold_plot <- make_lang_plot(top_spoke_langs_gold, 
                                         "Games with Full Audio Support",
                                         "For Gold Games")
spoken_langs_silver_plot <- make_lang_plot(top_spoke_langs_silver, 
                                         "Games with Full Audio Support",
                                         "For Silver Games")

# Spoken Languages Plot by Year

spoken_langs_2022_plot <- make_lang_plot(top_spoke_langs_2022, 
                                         "Games with Full Audio Support",
                                         "For 2022")

spoken_langs_2023_plot <- make_lang_plot(top_spoke_langs_2023, 
                                         "Games with Full Audio Support",
                                         "For 2023")
spoken_langs_2024_plot <- make_lang_plot(top_spoke_langs_2024, 
                                           "Games with Full Audio Support",
                                           "For 2024")

# Note that a pie chart doesn't make sense here. Values overlap so the pie chart can't show that
top_lang_inter_plot <- ggplot(data = data.frame(top_interface_languages),
                              aes(y = all_langs,
                                  x = Freq,
                                  fill = all_langs)) +
  geom_col(width = 0.7,
           position = "dodge2",
           colour = rgb(0, 0, 0),
           fill = rgb(0.15, 0.26, 0.65),
           show.legend = FALSE) +
  #coord_radial(theta = "y", expand = FALSE) +
  #scale_fill_viridis_d() +
  #scale_fill_hue() +
  geom_text(aes(label = paste0(Freq), hjust = -0.3)) +
  theme_minimal() +
  labs(title = "Most Popular Interface Languages",
       y = "Languages",
       x = "Number of Games supporting the Language")

plot <- ggplot(data = data.frame(top_interface_languages),
               aes(x = all_langs, y = Freq)) +
  geom_segment(aes(x = all_langs, xend = all_langs, y = 0, yend = Freq)) +
  scale_colour_viridis_d() +
  geom_point(size=2, color="black") +
  coord_flip() +
  theme_minimal()



# Publisher Info and Plots ------------------------------------------------


# This function returns the publishers with the most best-selling games
# Input: Tibble and integer
# Output: Tibble
get_top_publishers <- function(steam_data, max_publishers)
{
  publishers <- count(steam_data, publisher) %>%
    filter(n > 1) %>%
    arrange(n) %>%
    slice_tail(n = max_publishers)
  return(arrange(publishers, n))
}

top_publishers <- count(Steam_Data, publisher) %>%
  filter(n > 1) %>%
  arrange(n) %>%
  slice_tail(n = 8)

top_publishers <- get_top_publishers(Steam_Data, 20)

top_publish_2022 <- get_top_publishers(games_2022, 10)
top_publish_2023 <- get_top_publishers(games_2023, 10)
top_publish_2024 <- get_top_publishers(games_2024, 10)

top_publish_plat <- get_top_publishers(games_plat, 10)
top_publish_gold <- get_top_publishers(games_gold, 10)
top_publish_silver <- get_top_publishers(games_silver, 10)

# Making the Plots

top_publish_plot <- ggplot(data = top_publishers,
                              aes(x = reorder(publisher, n),
                                  y = n,
                                  fill = publisher)) +
  geom_col(width = 0.7,
           position = "dodge2",
           colour = rgb(0, 0, 0),
           fill = rgb(0.15, 0.26, 0.65),
           show.legend = FALSE) +
  geom_text(aes(label = paste0(n), hjust = -0.5)) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Most Popular Publishers",
       x = "Publisher",
       y = "Number of Games Listed")

#tmp_data <- Steam_Data %>% filter(count(publisher) > 3)

top_publishers_plot <- ggplot(data = Steam_Data %>% filter(top_sell_category == "Gold"),
                              aes(y = publisher,
                                  fill = fct_relevel(factor(top_sell_category), 
                                                     "Platinum"))
                              ) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Most Popular Publishers",
       x = "Count",
       y = "Publisher")


# Graphics Cards Info and Plots -------------------------------------------


# This function returns the recommended graphics cards of the best-selling games
# Input: Tibble
# Output: Table
get_popular_gpus <- function(steam_data)
{
  all_gpus <- c()
  for (raw_gpus in steam_data$recommended_graphics)
  {
    if (is.na(raw_gpus)) next
    gpus <- str_split_1(str_to_upper(raw_gpus), coll("|"))
    all_gpus <- append(all_gpus, gpus) 
  }
  return(as_tibble(table(all_gpus)))
}

most_pop_gpus <- get_popular_gpus(Steam_Data)

pop_gpus_2022 <- get_popular_gpus(games_2022)
pop_gpus_2023 <- get_popular_gpus(games_2023)
pop_gpus_2024 <- get_popular_gpus(games_2024)

pop_gpus_plat <- get_popular_gpus(games_plat)
pop_gpus_gold <- get_popular_gpus(games_gold)
pop_gpus_silver <- get_popular_gpus(games_silver)

most_pop_nvidia <- most_pop_gpus %>% 
  filter(str_detect(all_gpus, "NVIDIA")) %>%
  filter(n > 3) %>%
  mutate(all_gpus = str_sub(all_gpus, start = 16L)) %>%
  rename(gpu_name = all_gpus)

# Making the Plots

library(wordcloud2)

most_pop_gpu_plot <- most_pop_nvidia %>% ggplot(aes(x = fct_reorder(gpu_names, desc(n)),
                                                    y = n)) +
  geom_col(fill = rgb(0.46, 0.72, 0))

most_pop_gpu_wc_plot <- wordcloud2(data = most_pop_nvidia,
                                color = rgb(0.46, 0.72, 0),
                                #rotateRatio = 0,
                                minRotation = -pi/2,
                                maxRotation = -pi/2)



# User Rating Info and Plots ----------------------------------------------


user_rating_info <- summary(Steam_Data$user_ratings_pct)

user_rating_2022 <- summary(games_2022$user_ratings_pct)
user_rating_2023 <- summary(games_2023$user_ratings_pct)
user_rating_2024 <- summary(games_2024$user_ratings_pct)

user_rating_plat <- summary(games_plat$user_ratings_pct)
user_rating_gold <- summary(games_gold$user_ratings_pct)
user_rating_silver <- summary(games_silver$user_ratings_pct)

# Making the Plots

user_rating_plot <- ggplot(data = Steam_Data,
                           aes(x = as.character(year_published),
                               y = user_ratings_pct,
                               fill = fct_relevel(factor(top_sell_category), 
                                                  "Platinum"))) +
  geom_boxplot(width = 0.8) +
  annotate("text", x = 2.1, y = 0.13, label = "NBA 2K24") +
  annotate("text", x = 3.1, y = 0.38, label = "MSFS 2024") +
  scale_fill_brewer() +
  scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1),
                     guide = guide_axis(minor.ticks = TRUE),
                     labels = scales::label_percent()) +
  theme_classic() +
  labs(title = "User Ratings",
       subtitle = "By Year and Top Selling Category",
       x = "Year",
       y = "Recommendation",
       fill = "Top Selling Category")



# Release Month Info and Plots --------------------------------------------


month_publish_freq <- Steam_Data %>% count(month_published)

pop_month_2022 <- games_2022  %>% count(month_published)
pop_month_2023 <- games_2023  %>% count(month_published)
pop_month_2024 <- games_2024  %>% count(month_published)

pop_month_plat <- games_plat  %>% count(month_published)
pop_month_gold <- games_gold  %>% count(month_published)
pop_month_silver <- games_silver  %>% count(month_published)

# Making the Plots

#tmp$date_published <- with(Steam_Data, year(Steam_Data$year_published), month(Steam_Data$month_published))

tmp <- count(Steam_Data, Steam_Data$month_published)

tmp_plot <- ggplot(data = tmp,
                   aes(x = `Steam_Data$month_published`,
                       y = n)) +
  geom_point()

month_publish_plot <- ggplot(data = Steam_Data, 
                             aes(x = month_published)) +
  geom_bar(fill = rgb(0.15, 0.26, 0.65)) +
  #facet_wrap(~year_published) +
  #scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10),
                     #guide = guide_axis(minor.ticks = TRUE)) +
  theme_classic() +
  labs(title = "Number of Games Published by Month",
       x = "Month",
       y = "Count",
       fill = "Top Selling Category")

