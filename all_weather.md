All weather & Global dynamic asset allocation 전략
================

레이달리오의 all weather 포트폴리오 전략과 Global dynamic asset allocation 전략에 대한 벡테스트 결과를 비교 벡테스팅 방법 및 코드는 <https://github.com/hyunyulhenry/GDAA> 을 참조

### 레이달리오의 all weather 포트폴리오

인플레이션, 저성장 등의 가능한 경제상황에 대체로 대응할 수 있게 고안된 포트폴리오. 미 전체 주식 종목에 대한 ETF 30%, 미 장기채 ETF 40%, 미 중기채 ETF 15%, 금 ETF 7.5%, 원자재 ETF 7.5%로 구성되며 리밸런싱은 매년 수행

#### ETF 가격정보 다운로드 및 데이터 정리

``` r
ticker = c('VTI', # 미국 상장 주식 전체(시가 가중평균)
           'TLT', # 미국 장기 국채
           'IEF', # 미국 중기채
           'GLD', # 금
           'DBC' # 원자
           
)
getSymbols(ticker, src = 'yahoo')
```

    ## [1] "VTI" "TLT" "IEF" "GLD" "DBC"

``` r
# 수정 주가만 뽑아서 묶기
prices = do.call(cbind, lapply(ticker, function(x) Ad(get(x))))

# 일별 수익률 계산
rets = Return.calculate(prices) %>% na.omit()
```

#### Return.portfolio 함수를 통한 백테스트

``` r
portfolio = Return.portfolio(R = rets, weights = c(0.3, 0.4, 0.15, 0.075, 0.075),
                             rebalance_on = 'years',
                             verbose = T)

# 회전율 계산
Port_turnover = xts(rowSums(abs(portfolio$BOP.Weight - stats::lag(portfolio$EOP.Weight)),
                            na.rm = T), order.by = index(portfolio$BOP.Weight))

# 매 거래시 수수료가 0.3%라 가정
fee = 0.003

# 수익률을 반영한 순수익 계산
Port_net = portfolio$returns - (Port_turnover * fee)

names(Port_net) = 'Returns_allweather'
```

#### 백테스트 결과

![](all_weather_files/figure-markdown_github/result-1.png)

![](all_weather_files/figure-markdown_github/chart-1.png)

#### 결과 요약

|             | 연평균수익률 | 누적수익률 | 최대낙폭 | 연평균표준편차 |
|-------------|:------------:|:----------:|:--------:|:--------------:|
| All Weather |   0.0751583  |  1.567624  |  0.14482 |    0.0713472   |
