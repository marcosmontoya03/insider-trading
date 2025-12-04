########## GATHERING STOCK DATA FROM MASSIVE API ###################
library(httr2)
library(jsonlite)
library(dplyr)
library(tidyverse)

############## CLEANING FUNCTION #############

clean_insider_data <- function(df){
  
  text <- "Some companies have multiple insider trading reports, and you’ll need to
  decide how to handle them. Sometimes these reports are released at the exact
  same time and have a single, combined effect on the stock price. Other times,
  several reports come out close together—sometimes just 1–2 minutes apart—making
  it difficult to separate the impact of each one. We need you to choose the
  time-gap (in minutes) that will serve as the threshold for treating reports as
  part of the same event."
  
  # Wrap text at 80 characters per line and print
  cat(paste(strwrap(text, width = 80), collapse = "\n"))
  
  threshold <- (as.numeric(readline(prompt = "Enter the time threshold in minutes: "))*60) 
  
  # Initial Cleaning
  df <- df %>%
    mutate(across(c(cost_share, number_shares, total_value), 
                  ~as.numeric(as.character(gsub(",", "", .))))) %>% 
    group_by(ticker, date) %>% 
    mutate(total_events_in_day = n(),
           n_transactions = n_distinct(transaction)) %>% 
    
    # Get rid of trades with different transactions for same company
    filter(n_transactions == 1) %>%
    select(-n_transactions) %>% 
    ungroup() %>% 
    group_by(ticker, datetime) %>% 
    mutate(tot_event_same_time = n()) %>% 
    ungroup()
  
  # Case 1: total_events_in_day == tot_event_same_time
  same <- df %>% 
    filter(total_events_in_day == tot_event_same_time) %>% 
    group_by(ticker, datetime) %>% 
    summarize(transaction = first(transaction),
              cost_share = sum(cost_share, na.rm = TRUE),
              number_shares = sum(number_shares, na.rm = TRUE),
              total_value = sum(total_value, na.rm = TRUE),
              date = first(date), 
              time = first(time)) %>% 
    select(ticker, transaction, cost_share, number_shares, total_value, 
           datetime, date, time) %>% 
    ungroup()
  
  # Case 2: Events close in time 
  close <- df %>%
    filter(total_events_in_day != tot_event_same_time) %>% 
    group_by(ticker, date) %>% 
    arrange(ticker, time) %>% 
    mutate(diff_prev = time - first(time),
           within_threshold = ifelse(diff_prev <= threshold, 1, 0)) %>%
    filter(within_threshold == 1) %>% 
    summarize(transaction = first(transaction),
              cost_share = sum(cost_share, na.rm = TRUE),
              number_shares = sum(number_shares, na.rm = TRUE),
              total_value = sum(total_value, na.rm = TRUE),
              date = first(date), 
              time = first(time),
              datetime = first(datetime)) %>% 
    select(ticker, transaction, cost_share, number_shares, total_value, 
           datetime, date, time) %>% 
    ungroup()
  
  # Bind all 
  df <- rbind(same, close) %>% 
    distinct(ticker, datetime, .keep_all = T)
  
  # Getting stock sector
  
  tickers <- df %>% 
    distinct(ticker) %>% 
    pull(ticker)
  
  results_list <- list()
  
  for (stock in tickers){
    
    link <-  paste0("https://stockanalysis.com/stocks/", stock, "/")
    
    page <- tryCatch(read_html(link), error = function(e) NULL)
    
    if (is.null(page)) {
      results_list[[stock]] <- data.frame(text = NA)
      next
    }
    
    extracted <- page %>% 
      html_nodes(".col-span-1:nth-child(2) .text-default") %>% 
      html_text()
    
    df_out <- data.frame(text = extracted, stringsAsFactors = FALSE)
    
    results_list[[stock]] <- df_out
  }
  
  stock_sector <- bind_rows(
    lapply(names(results_list), function(tick) {
      
      df <- results_list[[tick]]
      
      if (nrow(df) == 0) {
        df <- data.frame(text = NA, stringsAsFactors = FALSE)
      }
      
      df$ticker <- tick
      
      df
    })) %>% 
    rename(sector = text)
  
  # merging data 
  
  all_data <- left_join(df, stock_sector, by = "ticker")
  
  
  return(all_data)
}


df <- clean_insider_data(all_raw_data)

############## GETTING STOCK DATA FUNCTION #############

stock_data <- function(cleaned_df, api_key){
  
  df <- cleaned_df %>% 
    select(ticker, datetime) 
  
  
  
}

test <- stock_data(df)


########### test. ##########





