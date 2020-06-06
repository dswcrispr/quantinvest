All weather & Global dynamic asset allocation strategy
================

Comparing all weather portfolio strategy by Ray Dalio with Glbal dynamic
asset allocation
strategy.(reference:<https://github.com/hyunyulhenry/GDAA>)

### All weather portfolio by Ray Dalio

Asstet allocation that is supposed to be able to weather any economic
storm(inflation, low economic growth, etc), manage risk exposure, and
make steady returns over the long term.

The all weather portfolio asset allocation consists of 40% US long-term
bonds, 30% US\_stock ETF, 15% US intermediate-term bonds, 7.5% Gold ETF,
and 7.5% Commodity ETF. And this strategy makes rebalancing annually.

#### Download ETF price data and data cleansing

``` r
ticker = c('VTI', # All US stocks
           'TLT', # US long-term bonds
           'IEF', # US intermediate-term bonds
           'GLD', # Gold ETf
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

#### Correlation Plot

``` r
cor(rets) %>% corrplot(method = 'color', type = 'upper',
                       addCoef.col = 'black', number.cex = 0.7,
                       tl.cex = 0.6, tl.srt = 45, tl.col = 'black',
                       col = colorRampPalette(c('blue', 'white', 'red'))(200),
                       mar = c(0, 0, 0.5, 0))
```

![](all_weather_files/figure-gfm/corplot-1.png)<!-- -->

#### Prices trend

![](all_weather_files/figure-gfm/trend_1-1.png)<!-- -->

#### Backtesting with ‘Return.portfolio’ function

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

#### Backtesting results

![](all_weather_files/figure-gfm/result-1.png)<!-- -->

![](all_weather_files/figure-gfm/chart-1.png)<!-- -->

#### Summarization(All weather)

|             | Cummulative\_return | Annualized\_return | Max\_drawdown | Annualized\_std |
| ----------- | ------------------: | -----------------: | ------------: | --------------: |
| All Weather |            1.407484 |          0.0736708 |     0.1448199 |       0.0761641 |

Table1: Summarization(All weather

### GDAA(Global Dynamic Asset Allocation) Strategy

1.  Choose 5 ETFs with high returns in the past 3\~12 months among 10
    global ETFs

2.  Make minimum variance portfolio with chosen 5 ETFs with minimum
    weights 10% and maximum weights 30% constraints

3.  Rebalance portfolio monthly

#### Download ETF price data and data cleansing

``` r
ticker = c('VTI', # All US stocks
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

    ##  [1] "VTI" "IEV" "EWJ" "EEM" "TLT" "IEF" "IYR" "RWX" "GLD" "DBC"

``` r
# Extracting adjusted prices and binding
prices = do.call(cbind, lapply(ticker, function(x) Ad(get(x)))) %>% setNames(ticker)

# Calculating daily returns
rets = Return.calculate(prices) %>% na.omit()
```

#### Correlation plot

``` r
cor(rets) %>% corrplot(method = 'color', type = 'upper',
                       addCoef.col = 'black', number.cex = 0.7,
                       tl.cex = 0.6, tl.srt = 45, tl.col = 'black',
                       col = colorRampPalette(c('blue', 'white', 'red'))(200),
                       mar = c(0, 0, 0.5, 0))
```

![](all_weather_files/figure-gfm/corplot1-1.png)<!-- -->

#### Prices trend

![](all_weather_files/figure-gfm/trend_2-1.png)<!-- -->

#### Portfolio structuring

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

#### Backtesting with ‘Return.portfolio’ function

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

#### Backtesting results

![](all_weather_files/figure-gfm/result_gdaa-1.png)<!-- -->

![](all_weather_files/figure-gfm/chart_gdaa-1.png)<!-- -->

#### Dynamics of weights

|            |       VTI |       IEV |       EWJ | EEM |       TLT | IEF |       IYR |       RWX |       GLD |       DBC |
| ---------- | --------: | --------: | --------: | --: | --------: | --: | --------: | --------: | --------: | --------: |
| 2017-07-31 | 0.3000000 | 0.1000000 | 0.2994066 | 0.1 | 0.0000000 | 0.0 | 0.0000000 | 0.2005934 | 0.0000000 | 0.0000000 |
| 2017-08-31 | 0.3000000 | 0.1000000 | 0.2000000 | 0.1 | 0.0000000 | 0.0 | 0.0000000 | 0.0000000 | 0.3000000 | 0.0000000 |
| 2017-09-29 | 0.3000000 | 0.1000000 | 0.3000000 | 0.1 | 0.0000000 | 0.0 | 0.0000000 | 0.2000000 | 0.0000000 | 0.0000000 |
| 2017-10-31 | 0.3000000 | 0.1058331 | 0.3000000 | 0.1 | 0.0000000 | 0.0 | 0.0000000 | 0.0000000 | 0.0000000 | 0.1941669 |
| 2017-11-30 | 0.3000000 | 0.1012610 | 0.2915774 | 0.1 | 0.0000000 | 0.0 | 0.0000000 | 0.0000000 | 0.0000000 | 0.2071616 |
| 2017-12-29 | 0.3000000 | 0.1126800 | 0.2980167 | 0.1 | 0.0000000 | 0.0 | 0.0000000 | 0.0000000 | 0.0000000 | 0.1893033 |
| 2018-01-31 | 0.3000000 | 0.1037275 | 0.2967853 | 0.1 | 0.0000000 | 0.0 | 0.0000000 | 0.0000000 | 0.0000000 | 0.1994872 |
| 2018-02-28 | 0.3000000 | 0.1036824 | 0.1963176 | 0.1 | 0.0000000 | 0.0 | 0.0000000 | 0.0000000 | 0.0000000 | 0.3000000 |
| 2018-03-29 | 0.1678454 | 0.0000000 | 0.1321546 | 0.1 | 0.0000000 | 0.0 | 0.0000000 | 0.3000000 | 0.0000000 | 0.3000000 |
| 2018-04-30 | 0.1314922 | 0.0000000 | 0.1685078 | 0.1 | 0.0000000 | 0.0 | 0.0000000 | 0.3000000 | 0.0000000 | 0.3000000 |
| 2018-05-31 | 0.1338929 | 0.0000000 | 0.1661071 | 0.1 | 0.0000000 | 0.0 | 0.0000000 | 0.3000000 | 0.0000000 | 0.3000000 |
| 2018-06-29 | 0.1000000 | 0.0000000 | 0.1000000 | 0.0 | 0.0000000 | 0.0 | 0.2037782 | 0.2962218 | 0.0000000 | 0.3000000 |
| 2018-07-31 | 0.1000000 | 0.1000000 | 0.0000000 | 0.0 | 0.0000000 | 0.0 | 0.2507946 | 0.2492054 | 0.0000000 | 0.3000000 |
| 2018-08-31 | 0.1641841 | 0.0000000 | 0.0000000 | 0.0 | 0.3000000 | 0.0 | 0.1000000 | 0.2112503 | 0.0000000 | 0.2245656 |
| 2018-09-28 | 0.1057626 | 0.0000000 | 0.1000000 | 0.0 | 0.0000000 | 0.3 | 0.2192068 | 0.0000000 | 0.0000000 | 0.2750306 |
| 2018-10-31 | 0.1420530 | 0.0000000 | 0.0000000 | 0.0 | 0.0000000 | 0.3 | 0.1360437 | 0.0000000 | 0.3000000 | 0.1219033 |
| 2018-11-30 | 0.1396579 | 0.0000000 | 0.0000000 | 0.0 | 0.1964442 | 0.3 | 0.1000000 | 0.0000000 | 0.2638979 | 0.0000000 |
| 2018-12-31 | 0.0000000 | 0.0000000 | 0.0000000 | 0.0 | 0.2027635 | 0.3 | 0.1000000 | 0.1372328 | 0.2600037 | 0.0000000 |
| 2019-01-31 | 0.0000000 | 0.0000000 | 0.0000000 | 0.0 | 0.2083361 | 0.3 | 0.1000000 | 0.1447304 | 0.2469335 | 0.0000000 |
| 2019-02-28 | 0.1418584 | 0.0000000 | 0.0000000 | 0.0 | 0.2200914 | 0.3 | 0.1000000 | 0.0000000 | 0.2380502 | 0.0000000 |
| 2019-03-29 | 0.1000000 | 0.0000000 | 0.0000000 | 0.0 | 0.3000000 | 0.3 | 0.1000000 | 0.2000000 | 0.0000000 | 0.0000000 |
| 2019-04-30 | 0.1972981 | 0.0000000 | 0.0000000 | 0.1 | 0.3000000 | 0.3 | 0.1027019 | 0.0000000 | 0.0000000 | 0.0000000 |
| 2019-05-31 | 0.0000000 | 0.0000000 | 0.0000000 | 0.0 | 0.2172949 | 0.3 | 0.1000000 | 0.2200065 | 0.1626987 | 0.0000000 |
| 2019-06-28 | 0.1681037 | 0.0000000 | 0.0000000 | 0.0 | 0.2448221 | 0.3 | 0.1000000 | 0.0000000 | 0.1870742 | 0.0000000 |
| 2019-07-31 | 0.1801246 | 0.0000000 | 0.0000000 | 0.0 | 0.2782800 | 0.3 | 0.1000000 | 0.0000000 | 0.1415954 | 0.0000000 |
| 2019-08-30 | 0.1974381 | 0.0000000 | 0.0000000 | 0.0 | 0.2559442 | 0.3 | 0.1000000 | 0.0000000 | 0.1466177 | 0.0000000 |
| 2019-09-30 | 0.2165916 | 0.0000000 | 0.0000000 | 0.0 | 0.2409678 | 0.3 | 0.1000000 | 0.0000000 | 0.1424407 | 0.0000000 |
| 2019-10-31 | 0.0000000 | 0.0000000 | 0.1661672 | 0.0 | 0.3000000 | 0.0 | 0.1000000 | 0.2782451 | 0.1555877 | 0.0000000 |
| 2019-11-29 | 0.1665988 | 0.0000000 | 0.1979515 | 0.0 | 0.3000000 | 0.0 | 0.1000000 | 0.0000000 | 0.2354496 | 0.0000000 |
| 2019-12-31 | 0.1000000 | 0.1035466 | 0.2542407 | 0.0 | 0.0000000 | 0.0 | 0.2422127 | 0.0000000 | 0.3000000 | 0.0000000 |
| 2020-01-31 | 0.2916081 | 0.1368060 | 0.0000000 | 0.0 | 0.3000000 | 0.0 | 0.1000000 | 0.0000000 | 0.1715860 | 0.0000000 |
| 2020-02-28 | 0.3000000 | 0.0000000 | 0.0000000 | 0.0 | 0.1954117 | 0.3 | 0.1000000 | 0.0000000 | 0.1045883 | 0.0000000 |
| 2020-03-31 | 0.1000000 | 0.0000000 | 0.2046341 | 0.0 | 0.2096465 | 0.3 | 0.0000000 | 0.0000000 | 0.1857194 | 0.0000000 |
| 2020-04-30 | 0.1000000 | 0.0000000 | 0.1835562 | 0.0 | 0.2304615 | 0.3 | 0.0000000 | 0.0000000 | 0.1859823 | 0.0000000 |
| 2020-05-29 | 0.1000000 | 0.0000000 | 0.1863170 | 0.0 | 0.2307673 | 0.3 | 0.0000000 | 0.0000000 | 0.1829157 | 0.0000000 |
| 2020-06-05 | 0.1000000 | 0.0000000 | 0.1841164 | 0.0 | 0.2265925 | 0.3 | 0.0000000 | 0.0000000 | 0.1892911 | 0.0000000 |

Table2: Weights

#### Summarization(GDAA)

|      | Cummulative\_return | Annualized\_return | Max\_drawdown | Annualized\_std |
| ---- | ------------------: | -----------------: | ------------: | --------------: |
| GDAA |            1.381675 |          0.0755971 |     0.1631828 |        0.106658 |

Table3: Summarization(GDAA)

### All weather vs GDAA

Compare results of both strategies

``` r
portfolios = na.omit(cbind(Port_all_net, Port_gdaa_net)) %>% setNames(c('All weather', 'GDAA'))

charts.PerformanceSummary(portfolios, main = 'All weather vs GDAA')
```

![](all_weather_files/figure-gfm/compare-1.png)<!-- -->

#### Summarization(All Weather vs GDAA)

|             | Annualized\_return | Cummulative\_return | Max\_drawdown | Annualized\_std |
| ----------- | -----------------: | ------------------: | ------------: | --------------: |
| All Weather |          0.0736708 |            1.407484 |     0.1448199 |       0.0761641 |
| GDAA        |          0.0755971 |            1.381675 |     0.1631828 |       0.1066580 |
