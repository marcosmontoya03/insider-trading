# What is the impact of insider trading on a stock's price?

Created by Marcos Montoya Andrade and Gavin Schilling 


### Purpose

This project seeks to explore how much stock prices respond to insider trading
reports. User are able to scrape daily insider trading reports, clean their files, 
select their stock to analyze along with their control groups, and run a simple 
DiD analysis. The DiD analysis will use industry and sector ETFs to control for 
general trends in the market. 


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
they want to pick their stocks, the final stock selection, and control ETFs. 

`intra_day_data()` this function uses the Tiingo API to scrape minute-by-minute 
stock data for both the selected stock and ETFs on the date of the insider trading 
report.





### How to use 

1. Run the `get_insider_trading()` function. This will give all the insider
trading reports released on the day you run the function (and before the time
you run the function). This will give you a data frame

2. Load that data frame into the `clean_insider_data()` function to clean the 
data. Select your minute threshold to merge insider trading reports for the same 
stock that were released close to each other. 

3. Now, you will load the cleaned datset into the `select_your_analysis()` function.
First, you will select if you will you want to look at insider trading reports for
buys, sales, or proposed sales. Then, you will select if you want to see the top stocks 
based on the cost of the stock, the numbe of shares bought, or the total value
of the insider trade.

This will pull up a df based on your metrics with the industry and sector for each 
stock. You can choose one stock based on whatever preferences you have, and 
you will choose between one event if there multiple. 

Then, you will will research ETFs based on the industry and sector of your selected
stock. You will type your selected ETFs (in all capital letters) into the prompts.

You will have your final output ready for further analysis (a list).

4. TBD GAVIN WILL WRITE 















