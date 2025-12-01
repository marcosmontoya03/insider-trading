######## USER INPUT ##########

one_stock <- function(ticker,
                      date,
                      pre_period, 
                      post_period,
                      sector){
  #Confirm that ticker in our data
  
  #If not, throw an error
  
  
  get_prices_data()
  
}

whole_data_results <- funciton(){
  
}

one_stock(ticker = "CNA", pretrend,  )

whole_data_

#' Check if in data
#' 
#' Tests if the inputed stock is in our scraped finviz data on a given day
#' Will throw an error if the stock is not in our data, telling the user to 
#' update the scrape, or try using one of our current day scraping functions.
#' 
#' If there are multiple instance of the same ticker in a day, it will prompt
#' 
#' 
#' @param ticker The ticker symbol of the target company
#' @param date The given date  
#' 
#' @return The chosen ticker, date, and time to use for the triple DiD
check_if_in_data <- function(ticker, date){
  
  
  #If there are multiple instance of a ticker on a date
  
  #Return the chosen instance of the ticker, date, and time in our data
}




#' Scrape Price Data
#' 
#' Returns a dataset from
#' 
#' @param ticker The ticker symbol of the stock present in our insider trading
#' data
#' 
scrape_price_data <- function(ticker,
                              pre_period,
                              post_period)
  
#' 
Scrapte_sector_data <- function(ticker,
                                pre_period,
                                post_period,
                                ETF = NULL)