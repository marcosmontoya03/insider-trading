########## GATHERING STOCK DATA FROM MASSIVE API ###################
library(dplyr)
library(tidyverse)
library(rvest)
library(purrr)
library(furrr)
library(progress)

############## CLEANING FUNCTION #############

clean_insider_data <- function(df){
  
  text <- "Some companies have multiple insider trading reports, and you’ll need to
  decide how to handle them. Sometimes these reports are released at the exact
  same time and have a single, combined effect on the stock price. Other times,
  several reports come out close together—sometimes just 1–2 minutes apart—making
  it difficult to separate the impact of each one. We need you to choose the
  time-gap (in minutes) that will serve as the threshold for treating reports as
  part of the same event."
  
  # make text look better 
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
              cost_share = mean(cost_share, na.rm = TRUE),
              number_shares = sum(number_shares, na.rm = TRUE),
              total_value = sum(total_value, na.rm = TRUE),
              date = first(date), 
              time = first(time),
              datetime = first(datetime)) %>% 
    select(ticker, transaction, cost_share, number_shares, total_value, 
           datetime, date, time) %>% 
    ungroup()
  
  # Bind all 
  df_final <- bind_rows(same, close) %>% 
    distinct(ticker, datetime, .keep_all = T)
  
  return(df_final)
}


df <- clean_insider_data(all_raw_data)

############## GETTING STOCK SECTOR FUNCTION #############

get_stock_info <- function(df) {
  
  # extract unique tickers
  tickers <- df %>% 
    select(ticker) %>% 
    distinct() %>% 
    pull()
  
  # split tickers into batches of 100
  ticker_batches <- split(tickers, ceiling(seq_along(tickers)/100))
  
  results_list <- list()
  
  # inner scraping function (with possibly to avoid breaking function)
  scrape_stock <- possibly(function(stock) {
    link <- paste0("https://stockanalysis.com/stocks/", tolower(stock), "/")
    
    page <- read_html(link)
    
    info <- page %>% 
      html_nodes(".col-span-1:nth-child(1) .text-default , .col-span-1:nth-child(2) .text-default") %>% 
      html_text()
    
    if(length(info) < 2) info <- c(NA, NA)
    
    tibble(ticker = stock,
           sector = info[1],
           industry = info[2])
  }, 
  
  otherwise = tibble(ticker = NA, sector = NA, industry = NA))
  
  # loop over batches
  batch_counter <- 1
  
  for(batch in ticker_batches){
    
    message("Processing batch ", batch_counter, " of ", length(ticker_batches),
            " (", length(batch), " tickers)")
    
    batch_results <- map_dfr(batch, 
                             ~{scrape_stock(.x)})
    
    results_list <- append(results_list, list(batch_results))
    
    Sys.sleep(3)
    
    batch_counter <- batch_counter + 1
  }
  
  # bind all batches into a single df
  results_df <- bind_rows(results_list)
  
  # join all info 
  all_info <- inner_join(df, results_df, by = "ticker")
  
  return(all_info)
}

df <- get_stock_info(df)

############## SELECT YOUR FUN #############


select_your_analysis <- function(df){
  
  # transaction choice 
  transactions <- unique(df$transaction)
  
  quoted_transactions <- paste0("'", transactions, "'", collapse = ", ")
  
  text <- paste0("Given data constraints, you can only analyze one stock at a time. ",
    "To help you narrow down your choice, please select what type of insider ",
    "transaction (", quoted_transactions, ")  you are interested in.")
  
  cat(paste(strwrap(text, width = 80), collapse = "\n"))
  
  valid_transactions <- c("Buy", "Sale", "Proposed Sale")
  
  user_transaction <- readline(prompt = "Your Selection: ")
  
  while (!user_transaction %in% valid_transactions) {
    cat("Please choose one of: ", paste(valid_transactions, collapse = ", "), "\n")
    user_transaction <- readline(prompt = "Your Selection: ")
  }
  
  
  # metric choice 
  text <- "Now, do you want to select your stock based on the market price ('cost_share'),
  the number of shares traded ('number_shares') or the total value of the insider
  trading report ('total_value')?"
  
  cat(paste(strwrap(text, width = 80), collapse = "\n"))
  
  valid_metrics <- c("cost_share", "number_shares", "total_value")
  
  user_metric <- readline(prompt = "Your Selection: ")
  
  while (!user_metric %in% valid_metrics) {
    cat("Please choose one of: ", paste(valid_metrics, collapse = ", "), "\n")
    user_metric <- readline(prompt = "Your Selection: ")
  }

  
  
  # top or bottom choice
  text <- "To help you decide, we can provide the 6 stocks with the highest or
  lowest value based on your metric. Do you want to see the top values ('top')
  or the lowest values ('bottom')?"
  
  cat(paste(strwrap(text, width = 80), collapse = "\n"))
  
  user_head <- readline(prompt = "Your Selection: ")
  
  
  
  
  
  df <- df %>% 
    filter(transaction == user_transaction) %>% 
    select(ticker, transaction, all_of(user_metric), datetime,
           date, time) %>% 
    slice_head() # TBD but decieds top of bottom 
  
  return(df)
  
}

select_your_analysis <- function(df){
  
  
  
  steps 
  -what tyep of transaction (buy, sell, proposed)
  - by what metric do you want to select your stock (cost, total shares, total value)
  -once selected, we displya top stocks and ask you to pick the final stock 
  -for final stock, we will scrape indsutry and sector 
  -we will ask you to pick the ETFs 
  -great, now save your data set 
  
  -last functoin that marcost has to write: minnute by minute 
  -sign up for your token here 
  
  text <- "We are limited to analyzing insider trading on one stock at a time.
  As such, you will select which Sector and Industry you want to explore. After,
  you will selected one specific stock to conduct the final analysis."
  
  cat(paste(strwrap(text, width = 80), collapse = "\n"))
  
  
  readline(prompt = "Type Next: ")
  
  text <- "First, let's select a broad industry. Choose from the option below."
  
  industry <- df %>% 
    select(industry) %>% 
    distinct() %>% 
    pull()
  
  return(industry)

  }



############## GETTING ETFs #############

get_etfs <- function(df){
  
  
  industry <- df %>% 
    select(industry) %>% 
    distinct() %>% 
    pull()
  
  
  sector <- df %>% 
    select(sector) %>% 
    distinct() %>% 
    pull()
  
  
  
}




############## GETTING STOCK DATA FUNCTION #############

stock_data <- function(cleaned_df, api_key){
  
  df <- cleaned_df %>% 
    select(ticker, datetime) 
  
  
  
}

test <- stock_data(df)


########### test. ##########





