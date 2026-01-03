# An Analysis of the Best Selling New Games on Steam

## Basic Information

This repository contains the dataset and R files of the article "What Makes New Games Stand Out on PC?"
Link to article: https://by-bibliotheque.shinyapps.io/Best_Selling_Steam_Games_Analysis/

To activate dark mode on the webpage, click the tiny toggle icon at the top right corner of the page.

The 150 game dataset is in the Data folder as "Steam_Top_Selling_New_Games.csv"

## The Web Scraping Script

The web-scraping script is in the R_Scripts folder as "Steam_Top_Sellers.R"

The script requires Google Chrome to work because it uses the rvest package from Tidyverse.
The script was made in November 2025.
Since then, Steam has changed the HTML structure of its best games webpages and their URLS.
Due to possible limitations of rvest, the R script opens a new Chrome tab to collect the data of a game.
Thus, collecting all the data will involve a lot of RAM usage.
If the script is executed, all the Chrome tabs can be closed once the script is done.

## Future Plans For This Analysis Project

1. Update the article to include Steam's 2025 data (when they finalise it).
2. Potentially include the newly added bronze category to the dataset.
3. Modify the web-scraping script to be more memory efficient. I will either try Selenium in R or use Python tools.
4. Make the dark mode toggle in the top right corner of the webpage more visible in the light theme. 
