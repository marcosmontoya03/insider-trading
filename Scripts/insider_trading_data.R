library(tidyverse)
library(dplyr)
library(rvest)


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
  rename(Num_Shares = "#Shares")

return(df)

}

dec_1 <- get_insider_data()

