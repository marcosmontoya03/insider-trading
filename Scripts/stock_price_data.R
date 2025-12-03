########## GATHERING STOCK DATA FROM MASSIVE API ###################
library(httr2)
library(jsonlite)
library(dplyr)
library(tidyverse)


### test ###

stocks <- read.csv("/Users/marco/Documents/GitHub/insider-trading/data/dec_1_insider_trading.csv", 
                   row.names = 1) %>% 
  select(ticker, date, time)




#' Function to append the scraped data together
#'  
append_scraped_data <- function(){
  
  df_base = "data/dec_1_insider_trading.csv"
  # Code to get the number of scraped files in the data folder
  num_files <- 
    
    for(i in 2:num_files){
      r_bind
      
    }
}