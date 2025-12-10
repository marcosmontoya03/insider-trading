########## INSIDER TRADING DATA SCRAPPING FOR DEC 1 - DEC 5 ###################

library(tidyverse)
library(dplyr)
library(rvest)
library(janitor)
library(stringr)
library(lubridate)
library(readr)

############################## SCRAPING FUNCTION ##############################

#' Insider Trading Data Scraper Function 
#' 
#' @description This function accesses FinViz's insider trading online data and 
#' creates a data base of all buy/sale reports published on the day of usage 
#' 
#' @return data frame with insider trading information (ticker, type of transaction,
#' cost of share, total shares bought, total value of purchase, date, and time)

get_insider_trading <- function(){
  
  # Store DF here 
  results_list <- list() 
  
  counter <- 1
  
  # Scrape for both buy and sell reports 
  for(tc_value in 1:2){
    
    link <- paste0("https://finviz.com/insidertrading.ashx?tc=", tc_value)
    
    page <- read_html(link)
    
    df <- page %>% 
      html_nodes(".text-right:nth-child(8) , .text-right:nth-child(7) , 
                 #insider-table .text-center+ .text-right , .text-left:nth-child(1) , 
                 #insider-table .text-center , .tabular-nums .tab-link , td:nth-child(1) .tab-link") %>% 
      html_text() %>% 
      as.data.frame()
    
    # reformats df 
    df <- df[-seq(13, nrow(df), by = 7), ] %>% 
      as.data.frame() %>% 
      rename(x = ".")
    
    df <- as.data.frame(matrix(df[,1], byrow=TRUE, ncol = 6))
    colnames(df) <- df[1, ]
    df <- df[-1, ] 

    # cleaning step for later use 
    df <- df %>% 
      clean_names() %>% 
      rename(total_value = value,
             datetime = sec_form_4,
             cost_share = cost) %>% 
      mutate(date_part = word(datetime, 1, 2),
             date_full = paste(date_part, "2025"),
             date = as.Date(date_full, format = "%b %d %Y"),
             time = parse_time(word(datetime, 3, -1))) %>% 
      select(-date_part, -date_full) %>% 
      filter(date == Sys.Date(),
             time >= hm("09:30"), time <= hm("16:30"))
    
    results_list[[counter]] <- df
    
    counter <- counter + 1
    
    } 
  
  # bind both buy and sell clean reports 
  final_df <- bind_rows(results_list)
  
  return(final_df)
  
}

raw <- get_insider_trading()

################# Scraping at 4:30 pm each day ################################
  
# DECEMBER 1 
# dec_1 <- get_insider_trading()
# write.csv(dec_1, "dec_1_insider_trading.csv")

# DECEMBER 2
# dec_2 <- get_insider_trading()
# write.csv(dec_2, "dec_2_insider_trading.csv")
  
# DECEMBER 3
# dec_3 <- get_insider_trading()
# write.csv(dec_3, "dec_3_insider_trading.csv", row.names = F)

# DECEMBER 4
# dec_4 <- get_insider_trading()
# write.csv(dec_4, "dec_4_insider_trading.csv", row.names = F)

# DECEMBER 5
# dec_5 <- get_insider_trading()
# write.csv(dec_5, "dec_5_insider_trading.csv", row.names = F)


############################## BINDING DATA ##############################


all_raw_data <- list.files(path = "/Users/marco/Documents/GitHub/insider-trading/data/insider_data",
                           pattern = ".csv", full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows()

# 
# write.csv(all_raw_data, "insider_trading_dec_1-5.csv", row.names = F)

raw <- read.csv("data/insider_data/insider_trading_dec_1-5.csv")
