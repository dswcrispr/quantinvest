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
portfolio_all_g = Return.portfolio(R = rets,
                                   weights = c(0.3, 0.4, 0.15, 0.075, 0.075),
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
|-------------|--------------------:|-------------------:|--------------:|----------------:|
| All Weather |            1.806179 |          0.0766178 |     0.1448199 |       0.0759251 |

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
|------------|----------:|----------:|----------:|----------:|----------:|----:|----------:|----------:|----------:|----------:|
| 2019-02-28 | 0.0000000 | 0.0000000 | 0.0000000 | 0.0000000 | 0.2006476 | 0.3 | 0.1000000 | 0.2024414 | 0.1969110 | 0.0000000 |
| 2019-03-29 | 0.1000000 | 0.0000000 | 0.0000000 | 0.0000000 | 0.3000000 | 0.3 | 0.1000000 | 0.2000000 | 0.0000000 | 0.0000000 |
| 2019-04-30 | 0.1000000 | 0.0000000 | 0.0000000 | 0.1116098 | 0.3000000 | 0.3 | 0.1883902 | 0.0000000 | 0.0000000 | 0.0000000 |
| 2019-05-31 | 0.0000000 | 0.0000000 | 0.0000000 | 0.0000000 | 0.2172958 | 0.3 | 0.1000000 | 0.2200061 | 0.1626981 | 0.0000000 |
| 2019-06-28 | 0.1101341 | 0.0000000 | 0.0000000 | 0.0000000 | 0.2651479 | 0.3 | 0.1000000 | 0.0000000 | 0.2247180 | 0.0000000 |
| 2019-07-31 | 0.1192644 | 0.0000000 | 0.0000000 | 0.0000000 | 0.3000000 | 0.3 | 0.1000000 | 0.0000000 | 0.1807356 | 0.0000000 |
| 2019-08-30 | 0.1369431 | 0.0000000 | 0.0000000 | 0.0000000 | 0.2766801 | 0.3 | 0.1007329 | 0.0000000 | 0.1856438 | 0.0000000 |
| 2019-09-30 | 0.1500561 | 0.0000000 | 0.0000000 | 0.0000000 | 0.2610115 | 0.3 | 0.1066332 | 0.0000000 | 0.1822993 | 0.0000000 |
| 2019-10-31 | 0.1000000 | 0.0000000 | 0.0000000 | 0.0000000 | 0.3000000 | 0.0 | 0.1000000 | 0.3000000 | 0.2000000 | 0.0000000 |
| 2019-11-29 | 0.1000000 | 0.0000000 | 0.2533255 | 0.0000000 | 0.3000000 | 0.0 | 0.1000000 | 0.0000000 | 0.2466745 | 0.0000000 |
| 2019-12-31 | 0.1000000 | 0.1000000 | 0.2448779 | 0.0000000 | 0.0000000 | 0.0 | 0.2551221 | 0.0000000 | 0.3000000 | 0.0000000 |
| 2020-01-31 | 0.1110609 | 0.3000000 | 0.0000000 | 0.0000000 | 0.3000000 | 0.0 | 0.1000000 | 0.0000000 | 0.1889391 | 0.0000000 |
| 2020-02-28 | 0.2465709 | 0.0000000 | 0.0000000 | 0.0000000 | 0.2029607 | 0.3 | 0.1000000 | 0.0000000 | 0.1504684 | 0.0000000 |
| 2020-03-31 | 0.1000000 | 0.0000000 | 0.2028030 | 0.0000000 | 0.2139727 | 0.3 | 0.0000000 | 0.0000000 | 0.1832244 | 0.0000000 |
| 2020-04-30 | 0.1000000 | 0.0000000 | 0.1846295 | 0.0000000 | 0.2352359 | 0.3 | 0.0000000 | 0.0000000 | 0.1801346 | 0.0000000 |
| 2020-05-29 | 0.1003347 | 0.0000000 | 0.1901929 | 0.0000000 | 0.2362475 | 0.3 | 0.0000000 | 0.0000000 | 0.1732249 | 0.0000000 |
| 2020-06-30 | 0.1380669 | 0.0000000 | 0.0000000 | 0.1000000 | 0.2564655 | 0.3 | 0.0000000 | 0.0000000 | 0.2054676 | 0.0000000 |
| 2020-07-31 | 0.1307709 | 0.0000000 | 0.0000000 | 0.1000000 | 0.2524696 | 0.3 | 0.0000000 | 0.0000000 | 0.2167594 | 0.0000000 |
| 2020-08-31 | 0.1000000 | 0.0000000 | 0.2000000 | 0.1000000 | 0.3000000 | 0.0 | 0.0000000 | 0.0000000 | 0.3000000 | 0.0000000 |
| 2020-09-30 | 0.1000000 | 0.0000000 | 0.2000000 | 0.1000000 | 0.3000000 | 0.0 | 0.0000000 | 0.0000000 | 0.3000000 | 0.0000000 |
| 2020-10-30 | 0.1000000 | 0.0000000 | 0.2000000 | 0.1000000 | 0.0000000 | 0.3 | 0.0000000 | 0.0000000 | 0.3000000 | 0.0000000 |
| 2020-11-30 | 0.1583077 | 0.1000000 | 0.3000000 | 0.1416923 | 0.0000000 | 0.0 | 0.0000000 | 0.0000000 | 0.0000000 | 0.3000000 |
| 2020-12-31 | 0.1689533 | 0.1000000 | 0.3000000 | 0.1310467 | 0.0000000 | 0.0 | 0.0000000 | 0.0000000 | 0.0000000 | 0.3000000 |
| 2021-01-29 | 0.1672773 | 0.1000000 | 0.3000000 | 0.1327227 | 0.0000000 | 0.0 | 0.0000000 | 0.0000000 | 0.0000000 | 0.3000000 |
| 2021-02-26 | 0.1908197 | 0.1000000 | 0.3000000 | 0.1091803 | 0.0000000 | 0.0 | 0.0000000 | 0.0000000 | 0.0000000 | 0.3000000 |
| 2021-03-31 | 0.1255532 | 0.1744468 | 0.3000000 | 0.1000000 | 0.0000000 | 0.0 | 0.0000000 | 0.0000000 | 0.0000000 | 0.3000000 |
| 2021-04-30 | 0.1271331 | 0.2423838 | 0.0000000 | 0.1325354 | 0.0000000 | 0.0 | 0.1979476 | 0.0000000 | 0.0000000 | 0.3000000 |
| 2021-05-28 | 0.0000000 | 0.1000000 | 0.0000000 | 0.1116867 | 0.0000000 | 0.0 | 0.1883133 | 0.3000000 | 0.0000000 | 0.3000000 |
| 2021-06-30 | 0.1000000 | 0.1000000 | 0.0000000 | 0.0000000 | 0.0000000 | 0.0 | 0.2158577 | 0.2841423 | 0.0000000 | 0.3000000 |
| 2021-07-30 | 0.1000000 | 0.1000000 | 0.0000000 | 0.0000000 | 0.0000000 | 0.0 | 0.2230897 | 0.2810674 | 0.0000000 | 0.2958428 |
| 2021-08-31 | 0.1000000 | 0.1000000 | 0.0000000 | 0.0000000 | 0.0000000 | 0.0 | 0.2341940 | 0.3000000 | 0.0000000 | 0.2658060 |
| 2021-09-30 | 0.1000000 | 0.1000000 | 0.2314021 | 0.0000000 | 0.0000000 | 0.0 | 0.3000000 | 0.0000000 | 0.0000000 | 0.2685979 |
| 2021-10-29 | 0.1178058 | 0.1000000 | 0.0000000 | 0.0000000 | 0.0000000 | 0.0 | 0.2859022 | 0.2597116 | 0.0000000 | 0.2365803 |
| 2021-11-30 | 0.1000000 | 0.2559906 | 0.0000000 | 0.0000000 | 0.3000000 | 0.0 | 0.1895413 | 0.0000000 | 0.0000000 | 0.1544681 |
| 2021-12-31 | 0.1000000 | 0.2764003 | 0.0000000 | 0.0000000 | 0.3000000 | 0.0 | 0.1798085 | 0.0000000 | 0.0000000 | 0.1437912 |
| 2022-01-03 | 0.1000000 | 0.2372232 | 0.0000000 | 0.0000000 | 0.0000000 | 0.0 | 0.2627768 | 0.0000000 | 0.3000000 | 0.1000000 |

Table2: Weights

### Summarization(GDAA)

|      | Cummulative\_return | Annualized\_return | Max\_drawdown | Annualized\_std |
|------|--------------------:|-------------------:|--------------:|----------------:|
| GDAA |            2.386792 |          0.0928863 |     0.1926418 |       0.1068098 |

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
|-------------|-------------------:|--------------------:|--------------:|----------------:|
| All Weather |          0.0766178 |            1.806179 |     0.1448199 |       0.0759251 |
| GDAA        |          0.0928863 |            2.386792 |     0.1926418 |       0.1068098 |
