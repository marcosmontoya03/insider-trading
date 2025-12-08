########## GATHERING STOCK DATA FROM MASSIVE API ###################
library(dplyr)
library(tidyverse)
library(rvest)
library(purrr)
library(furrr)
library(progress)
library(riingo)
library(lubridate)

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
  
  # initial cleaning
  df <- df %>%
    mutate(across(c(cost_share, number_shares, total_value), 
                  ~as.numeric(as.character(gsub(",", "", .)))),
           datetime = as.POSIXct(datetime, format = "%b %d %I:%M %p", tz = "America/New_York")) %>% 
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
  
  # case 1: total_events_in_day == tot_event_same_time
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
  
  # case 2: events close in time 
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
  
  # bind all 
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
           industry = info[1],
           sector = info[2])
  }, 
  
  otherwise = tibble(ticker = NA, industry = NA, sector = NA))
  
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

# df <- get_stock_info(df)

############## SELECT YOUR ANALYSIS #############


select_your_analysis <- function(df, num_stocks = 5){
  
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
  
  # filter based on user input
  df_sorted <- df %>% 
    filter(transaction == user_transaction) %>% 
    arrange(desc(.data[[user_metric]])) 
  
  # getting sector and industry info for the top stocks 
  ordered_tickers <- df_sorted %>% 
    distinct(ticker) %>% 
    pull(ticker) 
  
  # while loop to only get top stocks (based on num_stocks, default = 5)
  n <- num_stocks
  step <- num_stocks
  valid_tickers <- character(0)
  
  while (length(valid_tickers) < num_stocks && n <= length(ordered_tickers)) {
    
    current_tickers <- ordered_tickers[1:n]
    
    df_top <- df_sorted %>% 
      filter(ticker %in% current_tickers)
    
    df_info <- get_stock_info(df_top)
    
    # collect valid tickers (non-NA sector)
    valid_tickers <- df_info %>% 
      filter(!is.na(sector)) %>% 
      distinct(ticker) %>% 
      pull(ticker)
    
    n <- n + step
  }
  
  # extract top stocks 
  final_tickers <- valid_tickers[1:num_stocks]
  
  final_tickers <- keep(final_tickers, is_supported_ticker)

  df_final <- df_info %>% 
    filter(ticker %in% final_tickers) %>% 
    mutate(date = as.Date(date))
  
  
  print(df_final %>% 
          select(ticker, all_of(user_metric), date, sector, industry))
  
  # final choice!!!!
  text <- paste0("Here are the ", num_stocks,  " stocks with the highest values for your metric, along 
  with their corresponding sector and industry. Which stock do you want to analyze?")
  
  cat(paste(strwrap(text, width = 80), collapse = "\n"))
  
  user_ticker <- readline(prompt = "Type the selected ticker: ")
  
  while (!user_ticker %in% final_tickers) {
    cat("Please choose one of: ", paste(final_tickers, collapse = ", "), "\n")
    user_ticker <- readline(prompt = "Type the selected ticker: ")
  }
 
  ticker_rows <- df_final %>% 
    filter(ticker == user_ticker)
  
  # if multiple rows exist for a ticker, let user choose by date
  if (nrow(ticker_rows) > 1) {
    cat("This ticker has multiple entries. Please choose a date from the following:\n")
    print(ticker_rows %>% 
            select(date) %>% 
            distinct())
    
    repeat {
      user_input <- readline(prompt = "Type the date you want to analyze (YYYY-MM-DD): ")
      user_date <- as.Date(user_input)
      
      # check if valid
      if (!is.na(user_date) && user_date %in% ticker_rows$date) {
        break
      } else {
        cat("Invalid date. Please choose one of: ", paste(ticker_rows$date, collapse = ", "), "\n")
      }
    }
    
    # final filter
    ticker_rows <- ticker_rows %>% 
      filter(date == user_date)
  }
  
  
  ############ ETF part #############
  df <- ticker_rows
  
  # overview of stock selected 
  text <- paste0("You selected ", df$ticker, ", which is in the ", df$sector, 
                 " sector and the ", df$industry, " industry."  )
  
  cat(strwrap(text, width = 80), collapse = "\n")
  
  readline(prompt = "Press enter")
  
  # getting user to search for ETFs 
  text <- "In this analysis, we are using exchage-traded-funds (ETFs) as our 
  control groups.ETFs are baskets of stocks that can be traded like any individual 
  stock. Some of them have themes that overlap with the sector and industry of your 
  stock. You will use the internet to find an ETF for the sector and industry of 
  your stock. For example, you can search up 'technology ETFs.' Use your best 
  judgement, and type the ticker of your selected ETFs below."
  
  cat(strwrap(text, width = 80), collapse = "\n")
  
  # sector ETF and checking for safety
  sector_etf <- readline(prompt = paste0("Find an ETF for the ", df$sector, " sector: "))
  
  while (is.na(is_supported_ticker(sector_etf)) || 
         !is_supported_ticker(sector_etf)) {
    
    cat("That ETF is not available for use. Please try again.")
    
    sector_etf <- readline(
      prompt = paste0("Find an ETF for the ", df$sector, " sector: ")
    )
  }
  
  # industry ETF and safety check 
  industry_etf <- readline(prompt = paste0("Find an ETF for the ", df$industry,
                                           " industry: "))
  
  while (is.na(is_supported_ticker(industry_etf)) || 
         !is_supported_ticker(industry_etf)) {
    
    cat("That ETF is not available for use. Please try again.")
    
    industry_etf <- readline(
      prompt = paste0("Find an ETF for the ", df$industry, " industry: ")
    )
  }
  
  final_output <- list(
    stock_selection  = ticker_rows,
    sector_etf   = sector_etf,
    industry_etf = industry_etf
  )
  
  # FINAL OUTPUT!
  return(final_output)
  
}


final_output <- select_your_analysis(df)


############## INTRA DAY DATA FUNCTION #############

# # Sign up for API
# riingo_browse_signup()
# 
# # Save your API token as RIINGO_TOKEN = token_here (no ""), and restart R
# usethis::edit_r_environ()

intra_day_data <- function(final_output){
  
  # extracting from list 
  user_stock_all <- final_output[[1]] 
  
  user_stock <- user_stock_all$ticker
  
  date_trade <- as.Date(user_stock_all$date)
  
  sector_etf <- final_output[[2]]
  
  industry_etf <- final_output[[3]]
  
  
  # info about riingo and Tiingo API 
  link <- "https://business-science.github.io/riingo/index.html"
  text <- paste0("To access intrastock data, we used the Tiingo API. 
  To learn more about this API's R documentation, please visit: ", link)
  
  cat(strwrap(text, width = 80), collapse = "\n")
  
  readline(prompt = "Press enter")
  
  # stock data 
  stock_data <- riingo_iex_prices(user_stock, 
                                  resample_frequency = "1min",
                                  start_date = date_trade,
                                  end_date = date_trade)
  
  stock_data$date <- with_tz(stock_data$date, "America/New_York")
  
  # sector data 
  sector_data <- riingo_iex_prices(sector_etf, 
                                  resample_frequency = "1min",
                                  start_date = date_trade,
                                  end_date = date_trade) 
  sector_data$date <- with_tz(sector_data$date, "America/New_York")
  
  # industry data 
  industry_data <- riingo_iex_prices(industry_etf, 
                                   resample_frequency = "1min",
                                   start_date = date_trade,
                                   end_date = date_trade) 
  industry_data$date <- with_tz(industry_data$date, "America/New_York")
  
  # final dataset 
  all_stock_data <-  bind_rows(stock_data, sector_data, industry_data)
  
  final_data <- list(all_stock_data = all_stock_data, user_stock = user_stock_all)
  
  
  return(final_data)
  
}


intra_day_data(final_user_selection)

try <- df %>% 
  filter(ticker == "CZFS") %>% 
  mutate(before = datetime - minutes(30),
         after = datetime + minutes(30))

test_2 <- riingo_iex_prices(try$ticker, resample_frequency = "1min") 

test_2$date <- with_tz(test_2$date, "America/New_York")

test_2 <- test_2 %>% 
  filter(date > try$before, date < try$after)
  


test <- riingo_iex_prices("CZFS", resample_frequency = "1min",
                          start_date = "2025-12-4",
                          end_date = "2025-12-4")

write.csv(test, "test_date.csv")




