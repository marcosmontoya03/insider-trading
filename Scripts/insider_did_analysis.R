####### File to manage user interaction with the insider trading data and selecting
# the functions and parameters to run
library(dplyr)
library(tidyverse)
library(lubridate)

#source("insider_trading_data.R")
# source("stock_price_data.R")

############################### FUNCTIONS ###################################

#'Interpret
#'
#'@desription Will interpret the results of the DiD analysis and print out a message
#'
#'@param reg_out The regression output
#'@param trade_type If the transaction was a "Buy", "Sale", "Proposed Sale"
interpret <- function(sum_reg, trade_type, outcome_var){

  direction <- ifelse(trade_type == "Buy","increase","decrease")
  coeff <-  sum_reg$coefficients[,1]
  pvalues <-  sum_reg$coefficients[,4]
  
  print(paste0("Your outcome variable is: ", outcome_var))
  print(paste0("The DiD estimator is ", round(coeff[4],3), " with a p value of ", round(pvalues[4],3)))
  if(!(outcome_var %in% c("volume","value"))){
  print(paste0("The trade type was a ",trade_type," so you would anticipate a ", direction,"."))
  } else {
  print(paste0("You are looking at volume or value, so there is no directional interpretation of the results"))
  }
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
                           outcome_var = 1L){
  #Setting outcome_var equal to the coresponding string
  if(outcome_var == 1){
    outcome_var <- "avg_price_high_low"
  } else if (outcome_var == 2){
    outcome_var <- "avg_price_open_close"
  } else if (outcome_var == 3){
    outcome_var <- "volume"
  } else {
    outcome_var <- "value"
  }

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

  #Calling interpret function
  
  interpret(sum_reg, trade_type, outcome_var)

  #Returning the dataset used in the analyis
  return(df)
}


#' Graph DiD
#'
#' Makes a nice graph for the insider trading even studied
graph_did <- function(df_did, trade_event){

  trade_event <- as.POSIXct(trade_event, tz = "UTC")
  print(trade_event)
  
  df_did <- df_did %>% 
    arrange(ticker, date)
  
  g1 <- ggplot(data = df_did, aes(x = date, y = outcome, color = ticker, group = ticker)) +
    geom_line() +
    scale_x_datetime(date_labels = "%H:%M") +
    facet_wrap(facets = vars(ticker), scales = "free_y") +
    geom_vline(xintercept = trade_event,
               linetype = "longdash")
    

  return(g1)
}


#'New User Interaction DiD
#'
#'Updated code to walk the user through the DiD functions and making selections
#'on their outcome of choice and whether to display graphs
#'
#'@param intra_day_list The list returned from our intra_day_data function
#'
new_user_interaction_did <- function(intra_day_list = NULL){
  
  
  ######### Marcos Saved an RDS file to give me the correct list, so extract this
  ### and find how to format it
  
  ##### Then test the function to make sure everything is working 
  
  
  if(!is.null(intra_day_list)){
  df_full <- intra_day_list[[1]]
  user_stock <- intra_day_list[[2]]$ticker
  user_ETF <-
  user_event <-
  user_type <- intra_day_list[[2]]$type
  } else {
    df_full <- testing_data
    user_stock <- "ACT"
    user_ETF <- "IFY"
  }
  
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

   cat("Great Choice. 🥳

The Analysis will now begin 🛫")


     #Creating the two datasets for the two seperate differnece in differences
     df_sector <- df_full %>%
       filter(ticker == user_stock |
                ticker == user_ETF)

     df_industry <- df_full %>%
       filter(ticker == user_stock |
                ticker != user_ETF)

     #Getting the user threshold
     user_thresh <-  readline("😧 Oops, please type in your minutes thersh hold.
😠 If you enter a non integer, the default will be 5 minutes
🪰 There is a known bug if you pick a thershhold that is larger than the data, so don't do that, please")
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
                    user_outcome)
     graph_choice <- readline("📊 Press 1 to see the DiD Graph")

     if(graph_choice == "1"){
        g1 <- graph_did(did_data)
        print(g1)
     }

     cat("*********************
Industry Analysis
*********************")
     did_data <- new_single_did(df_industry,
                    user_event,
                    attempted_thresh,
                    user_type,
                    user_stock,
                    user_outcome)
     graph_choice <- readline("📊 Press 1 to see the DiD Graph")
     if(graph_choice == "1"){
       g1 <- graph_did(did_data)
       print(g1)
     }

     cat("🥳 Thank you for using our code, that is all for now.

Thank you for the amazing courses this semester and last semester 🥳")
   }






################ Running Code ##################
testing_data <- read.csv("data/insider_data/test_data.csv")

## Testing single DiD
df_sector <- testing_data %>%
  filter(ticker == "ACT" |
           ticker == "IYF")



df_industry <- testing_data %>%
  filter(ticker == "ACT" |
           ticker != "IYF")

#Testing new_single did
test_out_data <- new_single_did(df_sector,
               "2025-12-01 12:00:00",
               40,
               "Buy",
               "ACT",
               outcome_var = 1)

g1 <- graph_did(df_did = test_out_data,
                trade_event = "2025-12-01 12:00:00")
print(g1)

test_out_data <- new_single_did(df_industry,
                                "2025-12-01 12:00:00",
                                5,
                                "Buy",
                                "ACT",
                                outcome_var = 1)

# Testing graphing
g1 <- graph_did(df_did = test_out_data,
                trade_event = "2025-12-01 12:00:00")
print(g1)

# Testing new user interaction
