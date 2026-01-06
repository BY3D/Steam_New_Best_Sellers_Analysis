# An Analysis of the Best Selling New Games on Steam

## Basic Information

This repository contains the dataset and R files of the article "What Makes New Games Stand Out on PC?"
The article was originally published on 31/12/2025

Link to article: https://by-bibliotheque.shinyapps.io/Best_Selling_Steam_Games_Analysis/

To activate dark mode on the webpage, click the tiny toggle icon at the top right corner of the page.

The 150 game dataset is in the Data folder as "Steam_Best_Selling_Games.csv"

The article itself is in the "Analysing_Best_Selling_Games.qmd" file.

## The Web Scraping Script

The web-scraping script is in the R_Scripts folder as "Steam_Top_Sellers.R"

The script requires Google Chrome to work because it uses the rvest package from Tidyverse.
The script was made in November 2025.
Since then, Steam has changed the HTML structure of its best games webpages and their URLS.
Thus, the XPaths and CSS Paths in the R script may need to be adjusted.
Due to possible limitations of rvest, the R script opens a new Chrome tab for every game to collect its data.
Thus, collecting all the data will involve a lot of RAM usage from open Chrome tabs.
After the script is executed, all the Chrome tabs can be safely closed.

## Necessary Software and Packages to Run the R Scripts
R - version 4.5.0

RStudio

Shiny

Tidyverse

rvest

httr2

rjson

## Future Plans For This Analysis Project

1. Create a self-contained offline version of the article with ShinyLive
2. Host the article on a more permanent server, such as on Github or elsewhere
3. Update the article to include Steam's 2025 data (when Valve finalises it).
4. Potentially include the newly added bronze category to the dataset.
5. Modify the web-scraping script to be more memory efficient. I will either try Selenium in R or use Python tools.
6. Make the dark mode toggle in the top right corner of the webpage more visible in the light theme. 
