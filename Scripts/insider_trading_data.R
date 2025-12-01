library(tidyverse)
library(dplyr)
library(rvest)
library(janitor)
library(stringr)


get_insider_data <- function(){

link <- "https://finviz.com/insidertrading.ashx?tc=7"

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
         datetime = sec_form_4) %>% 
  mutate( mutate(
    date_part = word(datetime, 1, 2),  # "Dec 01"
    time_part = word(datetime, 3, -1), # "02:00 PM"
    
    date_full = paste(date_part, "2025"),
    
    # convert to Date
    date_full = as.Date(date_full, format = "%b %d %Y"))
  

return(df)

}

dec_1 <- get_insider_data()

