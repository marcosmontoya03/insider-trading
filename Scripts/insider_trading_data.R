library(tidyverse)
library(dplyr)
library(rvest)
library(janitor)
library(stringr)


get_insider_trading <- function(type){
  
  tc_value <- if(type == "buy") {
    "1"
  } else if(type == "sale") {
    "2" 
  } else {
    stop("Invalid type. Type buy or sale")
  }

  link <- paste0("https://finviz.com/insidertrading.ashx?tc=", tc_value)

page <- read_html(link)

df <- page %>% 
  html_nodes(".text-right:nth-child(8) , .text-right:nth-child(7) , #insider-table .text-center+ .text-right , .text-left:nth-child(1) , #insider-table .text-center , .tabular-nums .tab-link , td:nth-child(1) .tab-link") %>% 
  html_text() %>% 
  as.data.frame()

df <- df[-seq(13, nrow(df), by = 7), ] %>% 
  as.data.frame() %>% 
  rename(x = ".")

df <- as.data.frame(matrix(df[,1], byrow=TRUE, ncol = 6))

colnames(df) <- df[1, ]

df <- df[-1, ] 

df <- df %>% 
  clean_names() %>% 
  rename(total_value = value,
         datetime = sec_form_4,
         cost_share = cost) %>% 
  mutate(date_part = word(datetime, 1, 2),
         date_full = paste(date_part, "2025"),
         date = as.Date(date_full, format = "%b %d %Y"),
         time = word(datetime, 3, -1)) %>% 
  select(-date_part, -date_full, -datetime)
  

return(df)

}


# DECEMBER 1 

dec_1_sales <- get_insider_trading("sale")

dec_1_buys <- get_insider_trading("buy")

dec_1_buys <- dec_1_buys %>% 
  filter(date == "2025-12-01")

dec_1_trading <- rbind(dec_1_sales, dec_1_buys)


write.csv(dec_1_trading, "dec_1_insider_trading.csv")


# DECEMBER 1 

dec_2_sales <- get_insider_trading("sale")

dec2_buys <- get_insider_trading("buy")

dec_2_buys <- dec_1_buys %>% 
  filter(date == "2025-12-01")

dec_2_trading <- rbind(dec_1_sales, dec_1_buys)


write.csv(dec_1_trading, "dec_2_insider_trading.csv")

