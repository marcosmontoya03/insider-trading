########## GATHERING STOCK DATA FROM MASSIVE API ###################
library(httr2)
library(jsonlite)
library(dplyr)
library(tidyverse)
library(lubridate)


### test ###

stocks <- read.csv("/Users/marco/Documents/GitHub/insider-trading/data/dec_1_insider_trading.csv", 
                   row.names = 1) %>%
  group_by(ticker, time) %>% 
  summarise(total = n())


test <- dec_3 %>% 
  mutate(time = parse_time(time)) %>% 
  filter(time > hm("09:30"), time < hm("16:30")) %>% 
  group_by(ticker, date) %>% 
  mutate(total_events_in_day = n()) %>% 
  ungroup() %>% 
  group_by(ticker, datetime) %>% 
  mutate(tot_event_same_time = n())

  


# GETTING MINUT BY MINUTE DATA 

clean_insider_data <- function(df){
  
  threholds <- readline(prompt = " what threshold do you want? edplain what theshold is, and why it matters ")
  
  
  df <- df %>%
    mutate(time = parse_time(time)) %>% 
    filter(time > hm("09:30"), time < hm("16:30")) %>% 
    group_by(ticker, date) %>% 
    mutate(total_events_in_day = n()) %>% 
    ungroup() %>% 
    group_by(ticker, datetime) %>% 
    mutate(tot_event_same_time = n())
    
  # if(total in a day is same as total in datetime){
  #   dont do anytign 
  # }
  # 
  # else {
  #   
  #   if (they are close to each toher )
  #     
  #     user gets to pick if they wnat to collapse (based on threshold)
  # }
  
}






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