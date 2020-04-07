# Search for stocks
# https://finviz.com/screener.ashx?v=111&f=cap_mid
# https://quant.stackexchange.com/questions/7761/a-simple-formula-for-calculating-implied-volatility
# https://www.rmetrics.org/downloads/9783906041025-basicr.pdf
library(RQuantLib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(fOptions)
library(quantmod)

# Calculate black scholes
#https://www.rmetrics.org/downloads/9783906041025-basicr.pdf

## Calculate historic volatiliy

spy <- read.table("./historic_data/SPY.csv", check.names=FALSE,  header=TRUE,sep=",")

daily_vol <- spy %>%
    mutate(P_i=Close/lag(Close))%>%
    mutate(X_i=log(P_i)) %>%
    mutate(X = (X_i - mean(X_i, na.rm = TRUE))^2) %>%
    summarise(sd = sd(X_i,na.rm = TRUE)) 

#252 is the number of trading days in a year
annual_vol <- daily_vol*sqrt(252)

v_t <- annual_vol * sqrt(23/252)
#vt <- daily_vol * sqrt(23)

# Probability that spy closes below 300 when current price is at 257
spy_current <- 257
pnorm(log(300/spy_current),sd=annual_vol$sd*sqrt(23/252))
# Probability that spy closes above 300 when current price is at 257
1-pnorm(log(300/spy_current),sd=annual_vol$sd*sqrt(23/252))

BlackScholes <- function(type=c("call", "put"), underlying, strike, maturity, volatility, riskFreeRate, costOfCarry=NULL )
{

    type = match.arg(type)
    if(is.null(costOfCarry))
        costOfCarry = riskFreeRate

    impl<- function(S, X, T, r, b, sigma)
    {

        d1 <- ( log(S/X) + (b+sigma*sigma/2)*T ) / (sigma*sqrt(T))
        d2 <- d1 - sigma*sqrt(T)
        call <- S*exp((b-r)*T)*pnorm(d1) - X*exp(-r*T)*pnorm(d2)
        put <- X*exp(-r*T)*pnorm(-d2) - S*exp((b-r)*T)*pnorm(-d1)
        return(list(call=call, put=put))
    }
    
    bs <- impl(underlying,strike,maturity,riskFreeRate,costOfCarry,volatility)

    param <- list(type=type,
                  underlying=underlying,
                  strike=strike,
                  maturity=maturity,
                  volatility=volatility,
                  riskFreeRate=riskFreeRate,
                  costOfCarry=costOfCarry)


    result = list();
    result <- list(parameters=param, price=if(type == "call") bs$call else bs$put)
    class(result) <- c("bsoption", "list")
    result
}

blackScholesImp <- function(S, k, Tm, r, sigma){
    
  d1 <- (log(S/k) + (r+(sigma^2)/2)*(Tm))/(sigma*sqrt(Tm))
  d2 <- (log(S/k) + (r-(sigma^2)/2)*(Tm))/(sigma*sqrt(Tm))
  
  call <- S*pnorm(d1) - k*exp(-r*Tm)*pnorm(d2)
  put <- k*exp(-r*Tm)*pnorm(-d2)- S*pnorm(-d1)

  return(list("call"=call,"put"=put))
}

blackScholes <- function(S, k, Tm, r, sigma)
{
    sapply(S,function(x) {
        blackScholesImp(x,k,Tm,r,sigma)})
}

buyCall <- function(spot, strike, Tm, r, sigma)
{
    blackScholes(spot, strike, Tm, r, sigma)[,1]$call
}

sellCall <- function(spot,strike, Tm,r,sigma)
{
   -1 * blackScholes(spot, strike, Tm, r, sigma)[,1]$call
}

buyPut <- function(spot, strike, Tm, r, sigma)
{
    -1*blackScholes(spot, strike, Tm, r, sigma)[,1]$put
}

sellPut <- function(spot, strike, Tm, r, sigma)
{
    blackScholes(spot, strike, Tm, r, sigma)[,1]$put
}

bullCall <- function(long_strike, short_strike, spot, Tm, r, sigma)
{
    spot_range <- seq(long_strike*(1-0.5),short_strike*(1+0.5),0.01)

    long_call <- buyCall(spot,long_strike, Tm,r,sigma)
    short_call <- sellCall(spot,short_strike,Tm,r,sigma)

    long_profile <- blackScholes(spot_range,long_strike,0,r,sigma)
    short_profile <- blackScholes(spot_range,short_strike,0,r,sigma)[,1]$call

 #   profile <- long_profile - long_call + short_call + short_profile


    list("spotRange" = spot_range,
         "longCallCost" = long_call,
         "shotCallCost" = short_call,
         "profile" = profile,
         "debit" = long_call + short_call,
         "long_profile"=long_profile)
}

spy <- 246.08
long <- 240
short <- 250 
Tm <- 14/365
r <- 0.01
sigma <- 0.455


bullSpread <- bullCall(long, short, spy, Tm, r, sigma)

spot <- seq(0,300)
call_sp <- sapply(spot,function(x) {
    black_scholes(x,250,1,.03,0.15)})

vol <- seq(0,1,0.01)
call_vol <- sapply(vol,function(x) {
    black_scholes(2540.21,2525, 18/365,0.01,x)})


int <- seq(0,1,0.01)
call_int <- sapply(int,function(x) {
    black_scholes(2540.21,2525, 19/365,x,0.604)})


theta <- function(TypeFlag, S, X, Time, r, b, sigma)
{
    d1 = (log(S/X) + (b + sigma*sigma/2)*Time)/(sigma*sqrt(Time))
    d2 = d1 - sigma*sqrt(Time)
    NDF <- function(x) exp(-x*x/2)/sqrt(8*atan(1))
    
    Theta1 = -(S*exp((b - r)*Time)*NDF(d1)*sigma)/(2*sqrt(Time))
    if (TypeFlag == "c")
        theta = Theta1 - (b - r)*S*exp((b - r)*Time)*pnorm(+d1) - r*X*exp(-r*Time)*pnorm(+d2)
    else if (TypeFlag == "p")
        theta = Theta1 + (b - r)*S*exp((b - r)*Time)* pnorm(-d1) + r*X*exp(-r*Time)*pnorm(-d2)
    theta
}

EuropeanOption("put", underlying=259.39, strike=263, dividendYield=0.0, riskFreeRate = 0.10, maturity = 39/365, volatility = 0.40)

EuropeanOptionImpliedVolatility("call", value=5.58,underlying=259.39, strike=263, dividendYield=0.0, riskFreeRate=0.0, maturity=39/360,volatility=0.10)

blackScholes(259.39, 263, 39/365,0.10,0.40)

bs <- BlackScholes(type="put", underlying = 259.39, strike = 263, maturity = 39/365, volatility = 0.40, riskFreeRate = 0.10)

do.call(BlackScholes,bs$parameters)

theta("c",S=259.39,X=263,Time=39/365,r=0.00,b=0.0,sigma=0.40)
theta("c",S=259.39,X=263,Time=39/365,r=0.00,b=0.0,sigma=0.40)/365


GBSOption(TypeFlag = "c", S = 259.39, X =263, Time = 39/365, r = 0.10, sigma = 0.40,b=0.10)
sapply(c('delta', 'gamma', 'vega', 'theta', 'rho'), function(greek) 
   GBSGreeks(Selection = greek, TypeFlag = "c", S = 259.39, X = 263, 
             Time = 39/365, r = 0.00, b = 0.00, sigma = 0.40))

black_scholes(2540.21,2545, 18/252,0.01,0.63)



#Calculate Bull Call Price Spread

# Parameters
spot_range <-seq(2500,2600,1)
long_strike <- 2545
short_strike <- 2580
multiplicator <- 100
spot <- 2540.21

long_call <- sapply(spot_range,function(x) {
    black_scholes(x,long_strike, 0/365,0.01,0.64)}) * multiplicator

short_call <- sapply(spot_range,function(x) {
    black_scholes(x,short_strike, 0/365,0.01,0.64)}) * multiplicator

long_call_buyin <- black_scholes(spot,long_strike, 18/365,0.01,0.64) * multiplicator
short_call_buyin <- black_scholes(spot,short_strike, 18/365,0.01,0.64) * multiplicator

price <- long_call_buyin - short_call_buyin

plot(spot_range,long_call[1,]
     -long_call_buyin[1] +
     (-1*short_call[1,])+
     short_call_buyin[1], type="l")

plot(spot,call_sp[1,])

plot(int,call_int[1,])

plot(vol,call_vol[1,])


bla <- getSymbols("AAPL",src="yahoo")

getSymbols("AAPL", from='2000-01-01',to='2015-09-25')
