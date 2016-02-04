# Portfolio-Graphs
## Modern Portfolio Theory Analysis of investment portfolios

### Input
- start and end date for analysis
- your portfolio (tickers and shares) that yahoo finance has daily historical prices for each day in the analysis window
- market sharpe ratio & libor if you want to change those.
- benchmark portfolio (tickers and weights) 
  - Ideally use a benchmark that represents the entire world of investable assets (this produces most sensible expected returns)

You need to be connected to the interenet to access yahoo.

### What it does 

It'll get all the price histories and compute the daily price gains/losses and use those to estimate risk, correlations, and even beta against the benchmark and therefore expected return.

The model is very simple, taking past histories of each ticker as the covariance matrix of expected future co-risk.

### Output

It produces some tables and useful graphs you can use to manage your portfolio, like: 

#### Backtest your portfolio (green) vs. benchmark (blue)

![Backtest vs. benchmark](https://cloud.githubusercontent.com/assets/7470980/12826979/70826f40-cb32-11e5-85c8-65788af1b875.png)

#### Risk Vectors of each holding

![Risk Vectors](https://cloud.githubusercontent.com/assets/7470980/12826978/70810d76-cb32-11e5-9273-b6589233d935.png)

#### Correlation clusters

![Correlation Clusters](https://cloud.githubusercontent.com/assets/7470980/12826977/7080d3ba-cb32-11e5-9041-7a727d9c219b.png)

#### Mispricing

![Mispricing](https://cloud.githubusercontent.com/assets/7470980/12826980/7083919a-cb32-11e5-891e-a16efa34c104.png)
