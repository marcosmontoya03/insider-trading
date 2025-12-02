########## GATHERING STOCK DATA FROM MASSIVE API ###################
library(httr2)
library(jsonlite)
library(dplyr)
library(tidyverse)


### test ###

stocks <- read.csv("/Users/marco/Documents/GitHub/insider-trading/data/dec_1_insider_trading.csv", 
                   row.names = 1) %>% 
  select(ticker, date, time)

