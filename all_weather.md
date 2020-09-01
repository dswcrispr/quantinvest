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
| All Weather |            1.568736 |          0.0774485 |     0.1448201 |       0.0759644 |

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

|            |       QQQ |       IEV |       EWJ |     EEM |       TLT | IEF |       IYR |       RWX |       GLD |       DBC |
| ---------- | --------: | --------: | --------: | ------: | --------: | --: | --------: | --------: | --------: | --------: |
| 2017-09-29 | 0.1486668 | 0.1809961 | 0.3000000 | 0.10000 | 0.0000000 | 0.0 | 0.0000000 | 0.2703371 | 0.0000000 | 0.0000000 |
| 2017-10-31 | 0.1666574 | 0.2000438 | 0.3000000 | 0.10000 | 0.0000000 | 0.0 | 0.0000000 | 0.0000000 | 0.0000000 | 0.2332987 |
| 2017-11-30 | 0.1589130 | 0.1964485 | 0.3000000 | 0.10000 | 0.0000000 | 0.0 | 0.0000000 | 0.0000000 | 0.0000000 | 0.2446385 |
| 2017-12-29 | 0.1452439 | 0.2279022 | 0.3000000 | 0.10000 | 0.0000000 | 0.0 | 0.0000000 | 0.0000000 | 0.0000000 | 0.2268539 |
| 2018-01-31 | 0.1131743 | 0.2452578 | 0.3000000 | 0.10000 | 0.0000000 | 0.0 | 0.0000000 | 0.0000000 | 0.0000000 | 0.2415679 |
| 2018-02-28 | 0.1000000 | 0.2106364 | 0.2893636 | 0.10000 | 0.0000000 | 0.0 | 0.0000000 | 0.0000000 | 0.0000000 | 0.3000000 |
| 2018-03-29 | 0.1000000 | 0.0000000 | 0.2000000 | 0.10000 | 0.0000000 | 0.0 | 0.0000000 | 0.3000000 | 0.0000000 | 0.3000000 |
| 2018-04-30 | 0.1000000 | 0.0000000 | 0.2000000 | 0.10000 | 0.0000000 | 0.0 | 0.0000000 | 0.3000000 | 0.0000000 | 0.3000000 |
| 2018-05-31 | 0.1000000 | 0.0000000 | 0.2000000 | 0.10000 | 0.0000000 | 0.0 | 0.0000000 | 0.3000000 | 0.0000000 | 0.3000000 |
| 2018-06-29 | 0.1000000 | 0.0000000 | 0.1000000 | 0.00000 | 0.0000000 | 0.0 | 0.2093497 | 0.2906503 | 0.0000000 | 0.3000000 |
| 2018-07-31 | 0.1000000 | 0.1000000 | 0.0000000 | 0.00000 | 0.0000000 | 0.0 | 0.2531757 | 0.2468243 | 0.0000000 | 0.3000000 |
| 2018-08-31 | 0.1000000 | 0.0000000 | 0.0000000 | 0.00000 | 0.3000000 | 0.0 | 0.1131126 | 0.2454792 | 0.0000000 | 0.2414082 |
| 2018-09-28 | 0.1000000 | 0.0000000 | 0.1000000 | 0.00000 | 0.0000000 | 0.3 | 0.2200583 | 0.0000000 | 0.0000000 | 0.2799417 |
| 2018-10-31 | 0.1000000 | 0.0000000 | 0.0000000 | 0.00000 | 0.0000000 | 0.3 | 0.1603287 | 0.0000000 | 0.3000000 | 0.1396713 |
| 2018-11-30 | 0.1000000 | 0.0000000 | 0.0000000 | 0.00000 | 0.2085015 | 0.3 | 0.1000000 | 0.0000000 | 0.2914985 | 0.0000000 |
| 2018-12-31 | 0.0000000 | 0.0000000 | 0.0000000 | 0.00000 | 0.2027647 | 0.3 | 0.1000000 | 0.1372327 | 0.2600026 | 0.0000000 |
| 2019-01-31 | 0.0000000 | 0.0000000 | 0.0000000 | 0.00000 | 0.2083378 | 0.3 | 0.1000000 | 0.1447301 | 0.2469321 | 0.0000000 |
| 2019-02-28 | 0.0000000 | 0.0000000 | 0.0000000 | 0.00000 | 0.2006484 | 0.3 | 0.1000000 | 0.2024418 | 0.1969098 | 0.0000000 |
| 2019-03-29 | 0.1000000 | 0.0000000 | 0.0000000 | 0.00000 | 0.3000000 | 0.3 | 0.1000000 | 0.2000000 | 0.0000000 | 0.0000000 |
| 2019-04-30 | 0.1000000 | 0.0000000 | 0.0000000 | 0.11161 | 0.3000000 | 0.3 | 0.1883900 | 0.0000000 | 0.0000000 | 0.0000000 |
| 2019-05-31 | 0.0000000 | 0.0000000 | 0.0000000 | 0.00000 | 0.2172960 | 0.3 | 0.1000000 | 0.2200062 | 0.1626979 | 0.0000000 |
| 2019-06-28 | 0.1101339 | 0.0000000 | 0.0000000 | 0.00000 | 0.2651487 | 0.3 | 0.1000000 | 0.0000000 | 0.2247175 | 0.0000000 |
| 2019-07-31 | 0.1192643 | 0.0000000 | 0.0000000 | 0.00000 | 0.3000000 | 0.3 | 0.1000000 | 0.0000000 | 0.1807357 | 0.0000000 |
| 2019-08-30 | 0.1369434 | 0.0000000 | 0.0000000 | 0.00000 | 0.2766808 | 0.3 | 0.1007327 | 0.0000000 | 0.1856431 | 0.0000000 |
| 2019-09-30 | 0.1500563 | 0.0000000 | 0.0000000 | 0.00000 | 0.2610118 | 0.3 | 0.1066329 | 0.0000000 | 0.1822990 | 0.0000000 |
| 2019-10-31 | 0.1000000 | 0.0000000 | 0.0000000 | 0.00000 | 0.3000000 | 0.0 | 0.1000000 | 0.3000000 | 0.2000000 | 0.0000000 |
| 2019-11-29 | 0.1000000 | 0.0000000 | 0.2533260 | 0.00000 | 0.3000000 | 0.0 | 0.1000000 | 0.0000000 | 0.2466740 | 0.0000000 |
| 2019-12-31 | 0.1000000 | 0.1000000 | 0.2448795 | 0.00000 | 0.0000000 | 0.0 | 0.2551205 | 0.0000000 | 0.3000000 | 0.0000000 |
| 2020-01-31 | 0.1110608 | 0.3000000 | 0.0000000 | 0.00000 | 0.3000000 | 0.0 | 0.1000000 | 0.0000000 | 0.1889392 | 0.0000000 |
| 2020-02-28 | 0.2465705 | 0.0000000 | 0.0000000 | 0.00000 | 0.2029620 | 0.3 | 0.1000000 | 0.0000000 | 0.1504675 | 0.0000000 |
| 2020-03-31 | 0.1000000 | 0.0000000 | 0.2028034 | 0.00000 | 0.2139735 | 0.3 | 0.0000000 | 0.0000000 | 0.1832231 | 0.0000000 |
| 2020-04-30 | 0.1000000 | 0.0000000 | 0.1846297 | 0.00000 | 0.2352366 | 0.3 | 0.0000000 | 0.0000000 | 0.1801336 | 0.0000000 |
| 2020-05-29 | 0.1003343 | 0.0000000 | 0.1901937 | 0.00000 | 0.2362482 | 0.3 | 0.0000000 | 0.0000000 | 0.1732238 | 0.0000000 |
| 2020-06-30 | 0.1380670 | 0.0000000 | 0.0000000 | 0.10000 | 0.2564659 | 0.3 | 0.0000000 | 0.0000000 | 0.2054671 | 0.0000000 |
| 2020-07-31 | 0.1307710 | 0.0000000 | 0.0000000 | 0.10000 | 0.2524701 | 0.3 | 0.0000000 | 0.0000000 | 0.2167589 | 0.0000000 |
| 2020-08-31 | 0.1000000 | 0.0000000 | 0.2000000 | 0.10000 | 0.3000000 | 0.0 | 0.0000000 | 0.0000000 | 0.3000000 | 0.0000000 |

Table2: Weights

### Summarization(GDAA)

|      | Cummulative\_return | Annualized\_return | Max\_drawdown | Annualized\_std |
| ---- | ------------------: | -----------------: | ------------: | --------------: |
| GDAA |            1.782313 |          0.0865176 |     0.1926421 |        0.106168 |

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
| All Weather |          0.0774485 |            1.568736 |     0.1448201 |       0.0759644 |
| GDAA        |          0.0865176 |            1.782313 |     0.1926421 |       0.1061680 |
