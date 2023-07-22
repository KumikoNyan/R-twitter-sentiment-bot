# R-twitter-sentiment-bot

This is a simple Twitter sentiment analysis bot. It collects, cleans hashtags, and shows (3) data presentations.

## Features
- Extracts Tweets
- Frequency Graphs
- Histograms
- Barplots
- Text Mining

## Requirements
- Latest version of [R](https://cran.r-project.org/bin/windows/base/)
  - Add this to your ```PATH```
- R packages
  - ```httpuv```
  - ```lubridate```
  - ```tm```
  - ```rtweet```
  - ```plyr```
  - ```stringr```
  - ```ggplot2```
  - ```ggeasy```
  - ```dplyr```
- Optional: [R Studio](https://www.rstudio.com/products/rstudio/download/)

## Setup
1. Download all the required packages.
2. Setup your Twitter bot [here](https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html).
3. Input your ```API key``` and ```Access token``` in their respective variables.
4. Run the code.

## Notes
Apparently, Twitter's API limits the number of extracted tweets (15,000 tweets) if your developer account is not granted elevated access. In order to solve this, pay their pricing on elevated access. 

by @KumikoNyan
