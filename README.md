# What is the impact of insider trading on a stock's price?

Created by Marcos Montoya Andrade and Gavin Schilling 


### Purpose

This project seeks to explore how much stock prices respond to insider trading
reports. User are able to scrape daily insider trading reports, clean their files, 
select their stock to analyze along with their control groups, and run a simpel 
DiD analysis. 


### Files 

`insider_trading_data.R` contains the `get_insider_trading()` function. 

`stock_price_data.R` contains the `clean_insider_data()`, `get_stock_info()`,
`select_your_analysis()`, and `intra_day_data()` functions. 

`insider_did_analysis.R` TBD GAVING WILL WRITE



### Functions 

`get_insider_trading()` scrapes FinViz, which reports all SEC insider trading 
reports with time stamps of release 

`clean_insider_data()` cleans the FinViz data, prepares it for further analysis, 
and has user select what insider trading reports they want to merge

`get_stock_info()` this is a function used within the `select_your_analysis()`
function. It scrapes StockAnalysis.com for a stock's sector and industry. 

`select_your_analysis()` runs users through selecting a stock from their cleaned
FinViz data. User can select type of trade (buy, sale, proposed sale), the metrics
they want to pick their stocks, and the final selection. 

`intra_day_data()`





### How to use 
















