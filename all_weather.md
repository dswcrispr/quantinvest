All weather & Global dynamic asset allocation strategy
================

Comparing all weather portfolio strategy by Ray Dalio with Global
dynamic asset allocation
strategy.(reference:<https://github.com/hyunyulhenry/GDAA>)

## All weather portfolio by Ray Dalio

Asstet allocation that is supposed to be able to weather any economic
storm(inflation, low economic growth, etc), manage risk exposure, and
make steady returns over the long term.

The all weather portfolio asset allocation consists of 40% US long-term
bonds, 30% US\_stock ETF, 15% US intermediate-term bonds, 7.5% Gold ETF,
and 7.5% Commodity ETF. And this strategy makes rebalancing annually.

### Download ETF price data and data cleansing

``` r
ticker = c('VTI', # All US stocks
           'TLT', # US long-term bonds
           'IEF', # US intermediate-term bonds
           'GLD', # Gold ETF
           'DBC'  # Commodities ETF
           
)
getSymbols(ticker, src = 'yahoo')
```

    ## [1] "VTI" "TLT" "IEF" "GLD" "DBC"

``` r
# Extracting adjusted prices and binding
prices = do.call(cbind, lapply(ticker, function(x) Ad(get(x)))) %>% setNames(ticker)

# Calculating daily returns
rets = Return.calculate(prices) %>% na.omit() %>% window(., start = '2008-01-02')
```

### Correlation Plot

``` r
cor(rets) %>% corrplot(method = 'color', type = 'upper',
                       addCoef.col = 'black', number.cex = 0.7,
                       tl.cex = 0.6, tl.srt = 45, tl.col = 'black',
                       col = colorRampPalette(c('blue', 'white', 'red'))(200),
                       mar = c(0, 0, 0.5, 0))
```

![](all_weather_files/figure-gfm/corplot-1.png)<!-- -->

### Prices trend

![](all_weather_files/figure-gfm/trend_1-1.png)<!-- -->

### Backtesting with ‘Return.portfolio’ function

``` r
portfolio_all_g = Return.portfolio(R = rets, weights = c(0.3, 0.4, 0.15, 0.075, 0.075),
                                   rebalance_on = 'years', verbose = T)

# Calculating turnover
Port_all_turnover = xts(rowSums(abs(portfolio_all_g$BOP.Weight - 
                                  stats::lag(portfolio_all_g$EOP.Weight)),
                                  na.rm = T), order.by = index(portfolio_all_g$BOP.Weight))

# Suppose trading fee would be 0.3% for each trading
fee = 0.003

# Calculating net returns reflecting trading fee
Port_all_net = portfolio_all_g$returns - (Port_all_turnover * fee)

names(Port_all_net) = 'Returns_allweather'
```

### Backtesting results

![](all_weather_files/figure-gfm/result-1.png)<!-- -->

![](all_weather_files/figure-gfm/chart-1.png)<!-- -->

### Summarization(All weather)

|             | Cummulative\_return | Annualized\_return | Max\_drawdown | Annualized\_std |
| ----------- | ------------------: | -----------------: | ------------: | --------------: |
| All Weather |            1.617357 |           0.076683 |     0.1448201 |         0.07593 |

Table1: Summarization(All weather

## GDAA(Global Dynamic Asset Allocation) Strategy

1.  Choose 5 ETFs with high returns in the past 3\~12 months among 10
    global ETFs

2.  Make minimum variance portfolio with chosen 5 ETFs with minimum
    weights 10% and maximum weights 30% constraints

3.  Rebalance portfolio monthly

### Download ETF price data and data cleansing

``` r
ticker = c('QQQ', # US Nasdaq ETF
           'IEV', # European stocks
           'EWJ', # Japan stocks
           'EEM', # Emerging markets stocks
           'TLT', # US long-term bonds
           'IEF', # US intermediate-term bonds
           'IYR', # US Reits ETF
           'RWX', # Global Reits ETF 
           'GLD', # Gold ETF
           'DBC'  # Commodities ETF
)

getSymbols(ticker, src = 'yahoo')
```

    ##  [1] "QQQ" "IEV" "EWJ" "EEM" "TLT" "IEF" "IYR" "RWX" "GLD" "DBC"

``` r
# Extracting adjusted prices and binding
prices = do.call(cbind, lapply(ticker, function(x) Ad(get(x)))) %>% setNames(ticker)

# Calculating daily returns
rets = Return.calculate(prices) %>% na.omit()
```

### Correlation plot

``` r
cor(rets) %>% corrplot(method = 'color', type = 'upper',
                       addCoef.col = 'black', number.cex = 0.7,
                       tl.cex = 0.6, tl.srt = 45, tl.col = 'black',
                       col = colorRampPalette(c('blue', 'white', 'red'))(200),
                       mar = c(0, 0, 0.5, 0))
```

![](all_weather_files/figure-gfm/corplot1-1.png)<!-- -->

### Prices trend

![](all_weather_files/figure-gfm/trend_2-1.png)<!-- -->

### Portfolio structuring

``` r
# Extracting indices of end of each month
ep = endpoints(rets, on = 'months')

# Create list to save each month's weight 
weights = list()

# Setting lookback period as 12
lookback = 12

# Setting initial weight as 0
wt_zero = rep(0, 10) %>% setNames(colnames(rets))

# Code for calculating weights at the end of past each month
for (i in (lookback + 1) : length(ep)) {
  
  # Calculating momentum(cummulative returns for each stock in the past 3 ~ 12 months      and making rank) 
  sub_ret = lapply(3 : 12, function(x) {
    scale(rank(Return.cumulative(rets[c(ep[i - x] : ep[i]), ] )))
  })
    
  # Choose 5 stocks with high returns, 'rank' function imposes rank with ascending         order 
  K = (rank(- apply(do.call(cbind, sub_ret), 1, sum), ties.method = 'first') <= 5)
  
  # Make Var-Cov matrix for 5 chosen stocks' returns 
  covmat = cov(rets[c(ep[i - 12] : ep[i]), K])
  
  wt = wt_zero
  
  # Create minimum variance portfolio with weight constraints(max = 0.3, min = 0.1)
  wt[K] = optimalPortfolio(covmat, control = list(type = 'minvol', 
                                                  constraint = 'user',
                                                  LB = rep(0.10, 5),
                                                  UB = rep(0.30, 5)))
  
  # Saving calculated weights at the end of the each month on 'weights' list
  weights[[i]] = xts(t(wt), order.by = index(rets[ep[i]]))
}

weights = do.call(rbind, weights)
```

### Backtesting with ‘Return.portfolio’ function

``` r
portfolio_gdaa_g = Return.portfolio(R = rets, weights,
                             rebalance_on = 'months',
                             verbose = T)

# Calculating turnover
Port_gdaa_turnover = xts(rowSums(abs(portfolio_gdaa_g$BOP.Weight - 
                                       stats::lag(portfolio_gdaa_g$EOP.Weight)),
                            na.rm = T), order.by = index(portfolio_gdaa_g$BOP.Weight))

# Suppose trading fee would be 0.3% for each trading
fee = 0.003

# Calculating net returns reflecting trading fee
Port_gdaa_net = portfolio_gdaa_g$returns - (Port_gdaa_turnover * fee)

names(Port_gdaa_net) = 'Returns_gdaa'
```

### Backtesting results

![](all_weather_files/figure-gfm/result_gdaa-1.png)<!-- -->

![](all_weather_files/figure-gfm/chart_gdaa-1.png)<!-- -->

### Dynamics of weights

|            |       QQQ |       IEV |       EWJ |       EEM |       TLT | IEF |       IYR |       RWX |       GLD |       DBC |
| ---------- | --------: | --------: | --------: | --------: | --------: | --: | --------: | --------: | --------: | --------: |
| 2018-02-28 | 0.1000000 | 0.2106387 | 0.2893613 | 0.1000000 | 0.0000000 | 0.0 | 0.0000000 | 0.0000000 | 0.0000000 | 0.3000000 |
| 2018-03-29 | 0.1000000 | 0.0000000 | 0.2000000 | 0.1000000 | 0.0000000 | 0.0 | 0.0000000 | 0.3000000 | 0.0000000 | 0.3000000 |
| 2018-04-30 | 0.1000000 | 0.0000000 | 0.2000000 | 0.1000000 | 0.0000000 | 0.0 | 0.0000000 | 0.3000000 | 0.0000000 | 0.3000000 |
| 2018-05-31 | 0.1000000 | 0.0000000 | 0.2000000 | 0.1000000 | 0.0000000 | 0.0 | 0.0000000 | 0.3000000 | 0.0000000 | 0.3000000 |
| 2018-06-29 | 0.1000000 | 0.0000000 | 0.1000000 | 0.0000000 | 0.0000000 | 0.0 | 0.2093452 | 0.2906548 | 0.0000000 | 0.3000000 |
| 2018-07-31 | 0.1000000 | 0.1000000 | 0.0000000 | 0.0000000 | 0.0000000 | 0.0 | 0.2531713 | 0.2468287 | 0.0000000 | 0.3000000 |
| 2018-08-31 | 0.1000000 | 0.0000000 | 0.0000000 | 0.0000000 | 0.3000000 | 0.0 | 0.1131106 | 0.2454821 | 0.0000000 | 0.2414073 |
| 2018-09-28 | 0.1000000 | 0.0000000 | 0.1000000 | 0.0000000 | 0.0000000 | 0.3 | 0.2200584 | 0.0000000 | 0.0000000 | 0.2799416 |
| 2018-10-31 | 0.1000000 | 0.0000000 | 0.0000000 | 0.0000000 | 0.0000000 | 0.3 | 0.1603286 | 0.0000000 | 0.3000000 | 0.1396714 |
| 2018-11-30 | 0.1000000 | 0.0000000 | 0.0000000 | 0.0000000 | 0.2085009 | 0.3 | 0.1000000 | 0.0000000 | 0.2914991 | 0.0000000 |
| 2018-12-31 | 0.0000000 | 0.0000000 | 0.0000000 | 0.0000000 | 0.2027635 | 0.3 | 0.1000000 | 0.1372325 | 0.2600040 | 0.0000000 |
| 2019-01-31 | 0.0000000 | 0.0000000 | 0.0000000 | 0.0000000 | 0.2083364 | 0.3 | 0.1000000 | 0.1447297 | 0.2469339 | 0.0000000 |
| 2019-02-28 | 0.0000000 | 0.0000000 | 0.0000000 | 0.0000000 | 0.2006469 | 0.3 | 0.1000000 | 0.2024411 | 0.1969119 | 0.0000000 |
| 2019-03-29 | 0.1000000 | 0.0000000 | 0.0000000 | 0.0000000 | 0.3000000 | 0.3 | 0.1000000 | 0.2000000 | 0.0000000 | 0.0000000 |
| 2019-04-30 | 0.1000000 | 0.0000000 | 0.0000000 | 0.1116096 | 0.3000000 | 0.3 | 0.1883904 | 0.0000000 | 0.0000000 | 0.0000000 |
| 2019-05-31 | 0.0000000 | 0.0000000 | 0.0000000 | 0.0000000 | 0.2172947 | 0.3 | 0.1000000 | 0.2200055 | 0.1626998 | 0.0000000 |
| 2019-06-28 | 0.1101337 | 0.0000000 | 0.0000000 | 0.0000000 | 0.2651473 | 0.3 | 0.1000000 | 0.0000000 | 0.2247190 | 0.0000000 |
| 2019-07-31 | 0.1192641 | 0.0000000 | 0.0000000 | 0.0000000 | 0.3000000 | 0.3 | 0.1000000 | 0.0000000 | 0.1807359 | 0.0000000 |
| 2019-08-30 | 0.1369427 | 0.0000000 | 0.0000000 | 0.0000000 | 0.2766771 | 0.3 | 0.1007342 | 0.0000000 | 0.1856461 | 0.0000000 |
| 2019-09-30 | 0.1500557 | 0.0000000 | 0.0000000 | 0.0000000 | 0.2610092 | 0.3 | 0.1066340 | 0.0000000 | 0.1823010 | 0.0000000 |
| 2019-10-31 | 0.1000000 | 0.0000000 | 0.0000000 | 0.0000000 | 0.3000000 | 0.0 | 0.1000000 | 0.3000000 | 0.2000000 | 0.0000000 |
| 2019-11-29 | 0.1000000 | 0.0000000 | 0.2533259 | 0.0000000 | 0.3000000 | 0.0 | 0.1000000 | 0.0000000 | 0.2466741 | 0.0000000 |
| 2019-12-31 | 0.1000000 | 0.1000000 | 0.2448793 | 0.0000000 | 0.0000000 | 0.0 | 0.2551207 | 0.0000000 | 0.3000000 | 0.0000000 |
| 2020-01-31 | 0.1110605 | 0.3000000 | 0.0000000 | 0.0000000 | 0.3000000 | 0.0 | 0.1000000 | 0.0000000 | 0.1889395 | 0.0000000 |
| 2020-02-28 | 0.2465704 | 0.0000000 | 0.0000000 | 0.0000000 | 0.2029599 | 0.3 | 0.1000000 | 0.0000000 | 0.1504696 | 0.0000000 |
| 2020-03-31 | 0.1000000 | 0.0000000 | 0.2028028 | 0.0000000 | 0.2139725 | 0.3 | 0.0000000 | 0.0000000 | 0.1832247 | 0.0000000 |
| 2020-04-30 | 0.1000000 | 0.0000000 | 0.1846293 | 0.0000000 | 0.2352358 | 0.3 | 0.0000000 | 0.0000000 | 0.1801349 | 0.0000000 |
| 2020-05-29 | 0.1003342 | 0.0000000 | 0.1901933 | 0.0000000 | 0.2362474 | 0.3 | 0.0000000 | 0.0000000 | 0.1732251 | 0.0000000 |
| 2020-06-30 | 0.1380667 | 0.0000000 | 0.0000000 | 0.1000000 | 0.2564651 | 0.3 | 0.0000000 | 0.0000000 | 0.2054682 | 0.0000000 |
| 2020-07-31 | 0.1307707 | 0.0000000 | 0.0000000 | 0.1000000 | 0.2524695 | 0.3 | 0.0000000 | 0.0000000 | 0.2167598 | 0.0000000 |
| 2020-08-31 | 0.1000000 | 0.0000000 | 0.2000000 | 0.1000000 | 0.3000000 | 0.0 | 0.0000000 | 0.0000000 | 0.3000000 | 0.0000000 |
| 2020-09-30 | 0.1000000 | 0.0000000 | 0.2000000 | 0.1000000 | 0.3000000 | 0.0 | 0.0000000 | 0.0000000 | 0.3000000 | 0.0000000 |
| 2020-10-30 | 0.1000000 | 0.0000000 | 0.2000000 | 0.1000000 | 0.0000000 | 0.3 | 0.0000000 | 0.0000000 | 0.3000000 | 0.0000000 |
| 2020-11-30 | 0.1583065 | 0.1000000 | 0.3000000 | 0.1416935 | 0.0000000 | 0.0 | 0.0000000 | 0.0000000 | 0.0000000 | 0.3000000 |
| 2020-12-31 | 0.1689521 | 0.1000000 | 0.3000000 | 0.1310479 | 0.0000000 | 0.0 | 0.0000000 | 0.0000000 | 0.0000000 | 0.3000000 |
| 2021-01-20 | 0.1650595 | 0.1000000 | 0.3000000 | 0.1349405 | 0.0000000 | 0.0 | 0.0000000 | 0.0000000 | 0.0000000 | 0.3000000 |

Table2: Weights

### Summarization(GDAA)

|      | Cummulative\_return | Annualized\_return | Max\_drawdown | Annualized\_std |
| ---- | ------------------: | -----------------: | ------------: | --------------: |
| GDAA |            2.024025 |          0.0904978 |     0.1926419 |       0.1059553 |

Table3: Summarization(GDAA)

## All weather vs GDAA

Compare results of both strategies

``` r
portfolios = na.omit(cbind(Port_all_net, Port_gdaa_net)) %>% setNames(c('All weather', 'GDAA'))

charts.PerformanceSummary(portfolios, main = 'All weather vs GDAA')
```

![](all_weather_files/figure-gfm/compare-1.png)<!-- -->

### Summarization(All Weather vs GDAA)

|             | Annualized\_return | Cummulative\_return | Max\_drawdown | Annualized\_std |
| ----------- | -----------------: | ------------------: | ------------: | --------------: |
| All Weather |          0.0766830 |            1.617357 |     0.1448201 |       0.0759300 |
| GDAA        |          0.0904978 |            2.024025 |     0.1926419 |       0.1059553 |
