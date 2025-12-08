####### File to manage user interaction with the insider trading data and selecting
# the functions and parameters to run
library(dplyr)
library(tidyverse)
library(lubridate)

#source("insider_trading_data.R")
source("stock_price_data.R")

testing_data <- read.csv("data/insider_data/test_data.csv")

#' Function to ask user to pick the event they want vs whole data to do a DiD of
#'  
#'
user_interaction <- function(){
  #Code to open something in the console
  "Do you want to look at a specific ticker?"
  
  specific_event <- #user input
    
  if(specific_event == 1){
    
    selection <- event_selection(single = TRUE,
                                 ticker = )#user input)
  }
  else{
    #Code to prompt user
    "Do you want to look at a single day"
    
    single_day <- #user input
      
    if(single_day == 1){
      while(TRUE){
      #Code to prompt user
      "How many days do you want to select?"
      
      start_date <- #user input
      
      end_date <- #user input
      
      if(end_date > start_date)  {
        break
      } 
      else{
        print("End date must be after or equal to start date")
      }
      }  
      
      triple_did(date = )
    }
    triple_did(,selection)
  }
  
}

#' Function that will prompt the user to select the event and date and time
#' they want if there are multiple instance of an event
#' 
#' @return A vector where the first index is the ticker and the second index is
#' the date, and the third is the time, and the fourth is whether is was a buy,
#' sale, or a proposed sale
selection <- function(ticker){
  
  selection <- c(0,0,0,0)
  selection[1] <- user_ticker
  selection[2] <- user_date
  selection[3] <- user_time
  selection[4] <- user_direction
  
  return(selection)
}

#' Data Formating
#' 
#' Will make the data into minute by minute data, where each row contains the
#' target stock price, the industry price, and the sector price
#' 
#' @param 
data_fromatting <- function()




#' Triple DiD
#' 
#' A function to see if insider trading caused a change in the stock price,
#' adjusting for market and industry trends
#' 
#' @param df_clean Properly formated data from the data_formatting function
#' @param trade_event The time of the trading event
#' @param threshhold The time range you want to look for your pre and post period
#' @param trade_type If the transaction was a "Buy", "Sale", "Proposed Sale"
#' 
#' @return A summary table to the output
#' 
new_triple_did <- function(df, 
                           trade_event, 
                           threshold, 
                           trade_type,
                           target_ticker,
                           target_sector,
                           outcome_var = "avg_price_high_low"){
  
  #Remove data before and after threshold
  df <- df %>%
    mutate(date = ymd_hms(date)) %>% 
    filter(date > ymd_hms(trade_event) - minutes(threshold) &
             date < ymd_hms(trade_event) + minutes(threshold)
    )
  
  #Create treated and post indicators
  df <- df %>%
    mutate(post = ifelse(date >= ymd_hms(trade_event),1,0),
           treated = ifelse(ticker == target_ticker,1,0),
           subgroup = ifelse(ticker == target_sector,1,0)
    )
  
  #Picking the outcome of interest
  df <- df %>% 
    mutate(outcome = case_when(outcome_var == "avg_price_high_low" ~ ((high + low)/2),
                               outcome_var == "avg_price_open_close" ~((open + close)/2),
                               outcome_var == "volume" ~ (volume),
                               outcome_var == "value" ~ (volume * ((open + close)/2)))
    ) %>% 
    select(ticker, date, outcome, post, treated, subgroup)
  
  #Running the regression
  reg <- lm(outcome ~ post + treated + 
              subgroup + post:treated + 
              post:subgroup + treated:subgroup +
              post:treated:subgroup, data = df)
  sum_reg <-  summary(reg)
  return(sum_reg)
}
#'New User Interaction DiD
#'
#'Updated code to walk the user through the DiD functions and making selections
#'on their outcome of choice, and which groups to compare to
new_user_interaction_did <- function(intra_day_list = NULL){
  df_full <- intra_day_list[[1]]
  user_stock <- intra_day_list[[2]]$ticker
  user_ETF <- 
  user_event <- 
  user_type <- intra_day_list[[2]]$type
  
   cat("Welcome to the Analysis portion of this code. You only need to make a few more selections. 
Please chose the type of output you want to measure, there are 4 options
   1. avg_price_high_low: this makes the outcome variable for each minute equal to average between the high price in each minute and the low price in each minute.
   2. avg_price_open_close: this makes the outcome varaible equal to the average between the price of the stock at the beginning of that minute compared to the end of that minute. 
   3. volume: this sets that outcome variable equal to the volume of stocks traded in that minute. 
   4. value: this sets the outcome variable equal to the volume multiplied by the avg_price_high_low")
  
   user_outcome <-  readline("Please select options 1 through 4")
   
   while(!(user_outcome %in% c("1","2","3","4"))){
     user_outcome <-  readline("Please select options 1 through 4")
   }
   
   user_outcome <- as.integer(user_outcome)
  
   cat("Great Choice.
   
Do you want a DiD analysis and graphs, or an analysis of the speed of market adjustment to insider trading?
       
Type 1 for DiD analyis, and 2 for Speed measures")
   
   user_analysis <-  readline("Please select 1 or 2")
   
   while(!(user_analysis %in% c("1","2"))){
     user_analysis <-  readline("Please select 1 or 2")
   }
   
   user_analysis <- as.integer(user_analysis)
   
   if(user_analysis == 1){
     
     #Creating the two datasets for the two seperate differnece in differences
     df_sector <- df_full %>% 
       filter(ticker == user_stock | 
                ticker == user_ETF)
     
     df_industry <- df_full %>% 
       filter(ticker == user_stock |
                ticker != user_ETF)
     
     user_thresh <-  readline("Type in your minutes thersh hold. If you enter a 
                  non integer, the default will be 5 minutes")
     attempted_thresh <- as.integer(user_thresh)
     
     if(is.na(attempted_thresh)){
       attempted_thresh <- 5L
     } 
     
     cat("*********************
Sector/ETF Analysis
*********************")
     did_data <- new_single_did(df_sector,
                    user_event,
                    attempted_thresh, 
                    user_type, 
                    user_stock, 
                    user_analysis)
     readline("Press enter to see the DiD Graph")
     graph_did(did_data)
     
     
     cat("*********************
Industry Analysis
*********************")
     did_data <- new_single_did(df_industry,
                    user_event,
                    attempted_thresh, 
                    user_type, 
                    user_stock, 
                    user_analysis)
     readline("Press enter to see the DiD Graph")
     graph_did(did_data)
     
   }
   else{
     #Implementing Speed or trying different thresholds
     #
   }
   
   
   
}

#' Graph DiD
#' 
#' Makes a nice graph for the insider trading even studied
graph_did <- function(df_did){
  
}

#' New Single DiD
#' 
#' A DiD analysis 
#' 
#' @return The dataframe used from the analysis
new_single_did <- function(df, 
                           trade_event, 
                           threshold, 
                           trade_type,
                           target_ticker,
                           outcome_var = "avg_price_high_low"){
  
  #Remove data before and after threshold
  df <- df %>%
    mutate(date = ymd_hms(date)) %>% 
    filter(date > ymd_hms(trade_event) - minutes(threshold) &
           date < ymd_hms(trade_event) + minutes(threshold)
           )

  #Create treated and post indicators
  df <- df %>%
    mutate(post = ifelse(date >= ymd_hms(trade_event),1,0),
           treated = ifelse(ticker == target_ticker,1,0)
           )
  
  #Picking the outcome of interest
  df <- df %>% 
    mutate(outcome = case_when(outcome_var == "avg_price_high_low" ~ ((high + low)/2),
                               outcome_var == "avg_price_open_close" ~((open + close)/2),
                               outcome_var == "volume" ~ (volume),
                               outcome_var == "value" ~ (volume * ((open + close)/2)))
          ) %>% 
    select(ticker, date, outcome, post, treated)
  
  #Running the regression
  reg <- lm(outcome ~ post + treated + post:treated, data = df)
  sum_reg <-  summary(reg)
  interpret(sum_reg, trade_type)
  return(df)
}
  
################ Running Code ##################
df_clean <-  testing_data
mod_etf <- testing_data %>% 
  mutate(ticker = "ETF",
         high = high + rnorm(n(),mean = 15, sd = 30),
        low = low + rnorm(n(),mean = 15, sd = 30),
        open = open + rnorm(n(),mean = 15, sd = 30),
        close = close + rnorm(n(),mean = 15, sd = 30),
        volume = volume + rnorm(n(),mean = 15, sd = 30))

mod_ind <- testing_data %>% 
  mutate(ticker = "IND",
         high = high + rnorm(n(),mean = 0, sd = 15),
         low = low + rnorm(n(),mean = 0, sd = 15),
         open = open + rnorm(n(),mean = 0, sd = 15),
         close = close + rnorm(n(),mean = 0, sd = 15),
         volume = volume + rnorm(n(),mean = 0, sd = 15)) 
df_clean_3 <- rbind(df_clean,mod_etf,mod_ind)

test <- new_single_did(df_clean, "2025-12-4 15:00:00",
                          5,
                          "Buy",
                          "AAPL",
                          "avg_price_high_low")
sum_test <- summary(test)
interpret(test,"Buy") 

test_3 <- new_triple_did(df_clean,
                         "2025-12-01 12:00:00",
                         120,
                         "Buy",
                         "ACT",
                         "IYF",
                         "avg_price_high_low")

test_3$coefficients

test_coeff

test_3



#'Interpret
#'
#'Will interpret the results of the DiD analyis
#'
#'@param reg_out The regression output
#'@param trade_type If the transaction was a "Buy", "Sale", "Proposed Sale"
interpret <- function(sum_reg, trade_type){
  
  direction <- ifelse(trade_type == "Buy","increase","decrease")
  coeff <-  sum_reg$coefficients[,1]
  pvalues <-  sum_reg$coefficients[,4]
  print(paste0("The DiD estimator is ", coeff[4], " with a p value of ", pvalues[4],".
  The trade type was a ",trade_type," so you would anticipate a ", direction,"."))
}



#' Will run 
loop_dif_thresholds

plot_one_did

loop_through_dif

#'Graphing DiD
#'

#'Time to Market Response
#'
#'Estimates how quickly the market responds to an insider trading report being
#'released
#'
#'@param trade_event The time of the trading event

#' Graping Market Responce
#' 


old_triple_did <- function(start_date, end_date = NULL, time, ticker = NULL, ){
  
  #If ticker is null, then we are looking at the whole dataset
  if(is.null(ticker)){
    
    #If there is an end_date, we want to subset our data
    if(!is.null(end_date)){
      df <- df %>% 
        filter(date >= start_date & date <= end_date)
      
      #DiD Package call on our dataset
      
    } else{
      
      #Did Package call on our dataset
    }
  }
  
  #If there is a ticker, we want to just look at one ticker at one time
  else{
    get_ticker_data(start_date = date, 
                    time = time, 
                    ticker = ticker,
                    threshold)
  }
  
  ticker = NULL,
  date = selection[2],
  time = selection[3],
  direction = selection[4]
  
  if(is.null(ticker)){
    
  } else{
    
  }
  
}
