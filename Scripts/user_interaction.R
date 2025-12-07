####### File to manage user interaction with the insider trading data and selecting
# the functions and parameters to run
library(dplyr)
library(tidyverse)
library(lubridate)

#source("insider_trading_data.R")
source("stock_price_data.R")

testing_data <- read.csv("data/insider_data/test_date.csv")

#' Function to ask user to pick the event they want vs whole data to do a DiD of
#'  
#'
user_interaction <- function(){
  #Code to open something in the console
  "Do you want to look at a specific ticker?"
  
  specific_event <- #user input
    
  if(specific_event == 1){
    
    selection <- event_selection(single = TRUE,
                                 ticker = #user input)
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
new_triple_did <- function(df_clean, trade_event, threshold, trade_type){
  reg <- lm()
}


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
  summary(reg)
  return(reg)
}
  
################ Running Code ##################
df_clean <-  testing_data

test <- new_single_did(df_clean, "2025-12-4 15:00:00",
                          5,
                          "Buy",
                          "AAPL",
                          "avg_price_high_low")

summary(test)

interpret(test) 



#'Interpret
#'
#'Will interpret the results of the DiD analyis
#'
#'@param reg_out The regression output
#'@param trade_type If the transaction was a "Buy", "Sale", "Proposed Sale"



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
