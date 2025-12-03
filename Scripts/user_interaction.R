####### File to manage user interaction with the insider trading data and selecting
# the functions and parameters to run

#source("insider_trading_data.R")
source("stock_price_data.R")





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


triple_did <- function(start_date, end_date = NULL, time, ticker = NULL, ){
  
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
