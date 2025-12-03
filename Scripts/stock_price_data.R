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
  
  text <- "Some companies have multiple insider trading reports, and you’ll need to
  decide how to handle them. Sometimes these reports are released at the exact
  same time and have a single, combined effect on the stock price. Other times,
  several reports come out close together—sometimes just 1–2 minutes apart—making
  it difficult to separate the impact of each one. We need you to choose the
  time-gap (in minutes) that will serve as the threshold for treating reports as
  part of the same event."
  
  # Wrap text at 80 characters per line and print
  cat(paste(strwrap(text, width = 80), collapse = "\n"))
  
  threshold <- as.numeric(readline(prompt = "What is your desired threshold?"))
  
  
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
  
  return(df)
}


clean_insider_data(dec_3)




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