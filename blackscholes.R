# Search for stocks
# https://finviz.com/screener.ashx?v=111&f=cap_mid
# https://quant.stackexchange.com/questions/7761/a-simple-formula-for-calculating-implied-volatility
# https://www.rmetrics.org/downloads/9783906041025-basicr.pdf
library(RQuantLib)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(fOptions)
library(quantmod)
require(utils)

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
    type <- match.arg(type)
    if(is.null(costOfCarry))
        costOfCarry <- riskFreeRate

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

BlackScholesArray <- function(type, underlying, strike, maturity, volatility, riskFreeRate, costOfCarry=NULL)
{
    if(is.null(costOfCarry))
        costOfCarry <- riskFreeRate

    df <- expand.grid(type=type,
                      underlying=underlying,
                      strike=strike,
                      maturity=maturity,
                      volatility=volatility,
                      riskFreeRate=riskFreeRate,
                      costOfCarry=costOfCarry,
                      stringsAsFactors = FALSE)

    result <- mapply(function(type, underlying, strike, maturity, volatility, riskFreeRate, costOfCarry) {
        bs <- BlackScholes(type, underlying, strike, maturity, volatility, riskFreeRate, costOfCarry)
        g <- Greeks(bs)
        c(price=bs$price, delta=g$delta, gamma=g$gamma, theta=g$theta)
    },
    df$type, df$underlying,df$strike,df$maturity,df$volatility,df$riskFreeRate,df$costOfCarry)
    cbind(df,as.data.frame(t(result)))
}


CalculateDelta <- function(type=c("call", "put"), underlying, strike, maturity, volatility, riskFreeRate, costOfCarry=NULL )
{
    type <- match.arg(type)
    if(is.null(costOfCarry))
        costOfCarry <- riskFreeRate

    impl <- function(S, X, T, r, b, sigma) {
        d1 = (log(S/X) + (b + sigma*sigma/2)*T)/(sigma*sqrt(T))
        call <- exp((b - r)*T)*pnorm(d1)
        put <- exp((b - r)*T)*(pnorm(d1) - 1)

        return (list(call=call,put=put))
    }
    delta <- impl(underlying,strike,maturity,riskFreeRate,costOfCarry,volatility)
    if(type == "call")
        return(delta$call)
    else
        return(delta$put)
}

CalculateGamma <- function(type=c("call", "put"), underlying, strike, maturity, volatility, riskFreeRate, costOfCarry=NULL )
{
    type = match.arg(type)
    if(is.null(costOfCarry))
        costOfCarry <- riskFreeRate

    impl <- function(S, X, T, r, b, sigma) {
        d1 <- (log(S/X) + (b + sigma*sigma/2)*T)/(sigma*sqrt(T))
        gamma <- (exp((-d1^2)/2)/sqrt(2*pi))/(S*sigma*sqrt(T))
    }
    gamma <- impl(underlying,strike,maturity,riskFreeRate,costOfCarry,volatility)
    gamma
}

CalculateTheta <- function(type=c("call", "put"), underlying, strike, maturity, volatility, riskFreeRate, costOfCarry=NULL )
{
    type = match.arg(type)
    if(is.null(costOfCarry))
        costOfCarry <- riskFreeRate
    
    impl <- function(S, X, T, r, b, sigma)
    {
        d1 <- (log(S/X) + (b + sigma*sigma/2)*T)/(sigma*sqrt(T))
        d2 <- d1 - sigma*sqrt(T)
        NDF <- function(x) exp(-x*x/2)/sqrt(8*atan(1))
    
        Theta1 <- -(S*exp((b - r)*T)*NDF(d1)*sigma)/(2*sqrt(T))

        call <- Theta1 - (b - r)*S*exp((b - r)*T)*pnorm(+d1) - r*X*exp(-r*T)*pnorm(+d2)
        put <- Theta1 + (b - r)*S*exp((b - r)*T)* pnorm(-d1) + r*X*exp(-r*T)*pnorm(-d2)
        return(list(call=call,put=put))
    }
    theta <- impl(underlying,strike,maturity,riskFreeRate,costOfCarry,volatility)

    if(type == "call")
        return(theta$call)
    else
        return(theta$put)
}

CalculateRho <- function(price, type=c("call", "put"), underlying, strike, maturity, volatility, riskFreeRate, costOfCarry=NULL )
{
    type = match.arg(type)
    if(is.null(costOfCarry))
        costOfCarry <- riskFreeRate

    impl <- function(price, S, X, T, r, b, sigma) {
        d1 <- (log(S/X) + (b + sigma*sigma/2)*T)/(sigma*sqrt(T))
        d2 <- d1 - sigma*sqrt(T)

        if (b != 0)
            call <- T*X*exp(-r*T)*pnorm(d2)
        else
            call <- -T*price

        if (b != 0)
            put <- -T*X*exp(-r*T)*pnorm(-d2)
        else
            put <- -T*price
        return(list(call=call,put=put))
    }
    
    rho <- impl(price,underlying,strike,maturity,riskFreeRate,costOfCarry,volatility)

    if(type == "call")
        return(rho$call)
    else
        return(rho$put)
}

CalculateVega <- function(type=c("call", "put"), underlying, strike, maturity, volatility, riskFreeRate, costOfCarry=NULL )
{
    if(is.null(costOfCarry))
        costOfCarry <- riskFreeRate

    impl <- function(S, X, T, r, b, sigma)
    {
        NDF <- function(x) exp(-x*x/2)/sqrt(8*atan(1))
        d1 <- (log(S/X) + (b + sigma*sigma/2)*T)/(sigma*sqrt(T))
        vega <- S*exp((b - r)*T)*NDF(d1)*sqrt(T)
        vega
    }
    vega <- impl(underlying,strike,maturity,riskFreeRate,costOfCarry,volatility)
    return(vega)
}

Delta.bsoption <- function(x)
{
    do.call(CalculateDelta,x$parameters)
}

Gamma.bsoption <- function(x)
{
    do.call(CalculateGamma,x$parameters)
}

Theta.bsoption <- function(x)
{
    do.call(CalculateTheta,x$parameters)
}

Vega.bsoption <- function(x)
{
    do.call(CalculateVega,x$parameters)
}

Rho.bsoption <- function(x)
{
    params <- x$parameters
    params[["price"]] <- x$price
    do.call(CalculateRho,params)
}

Greeks.bsoption <- function(x)
{
    list(delta=Delta(x),
         gamma=Gamma(x),
         theta=Theta(x)/365,
         vega=Vega(x)/100,
         Rho=Rho(x)/100)
}

Delta <- function(x)
{
    UseMethod("Delta",x)
}

Gamma <- function(x)
{
    UseMethod("Gamma",x)
}

Theta <- function(x)
{
    UseMethod("Theta",x)
}

Vega <- function(x)
{
    UseMethod("Vega",x)
}

Rho <- function(x)
{
    UseMethod("Rho",x)
}

Greeks <- function(x)
{
    UseMethod("Greeks",x)
}

Order <- function(operation=c("sell","buy"),type=c("call", "put"), underlying, strike, maturity, volatility, riskFreeRate, costOfCarry=NULL )
{
    type <- match.arg(type)
    operation <- match.arg(operation)
    if(is.null(costOfCarry))
        costOfCarry <- riskFreeRate
    if(operation == "sell")
    {
        bs <- BlackScholes(type,underlying, strike, maturity, volatility, riskFreeRate, costOfCarry)
        bs$price <- bs$price * -1
        return(bs)
    }
    else
    {
        return(BlackScholes(type,underlying, strike, maturity, volatility, riskFreeRate, costOfCarry))
    }
}        

BearPut <- function(longStrike, shortStrike, underlying, maturity, volatility, riskFreeRate, costOfCarry=NULL)
{
    spotRange <- seq(shortStrike*(1-0.05),longStrike*(1+0.05),0.1)

    longPut <- BlackScholesArray(type="put", underlying, longStrike, maturity, volatility, riskFreeRate, costOfCarry)
    shortPut <- BlackScholesArray(type="put", underlying, shortStrike, maturity, volatility, riskFreeRate, costOfCarry)

    shortArray <- BlackScholesArray(type="put", underlying = spotRange, strike = shortStrike, c(0.01/365,1/365,5/365,10/365), volatility, riskFreeRate)
    longArray <- BlackScholesArray(type="put", underlying = spotRange, strike = longStrike,  c(0.01/365,1/365,5/365,10/365), volatility, riskFreeRate)

    longArray$price <- longArray$price - longPut$price
    shortArray$price <- shortArray$price - shortPut$price

    longArray$price <- longArray$price - shortArray$price

    #profile <- longArray$price - longPut$price - shortArray$price + shortPut$price

     list("spotRange" = spotRange,
          "longPut" = longPut,
          "shortPut" = shortPut,
          "profile" = longArray,
          "debit" = longPut$price - shortPut$price)
}

xospy <- 246.08
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

EuropeanOption("call", underlying=259.39, strike=263, dividendYield=0.0, riskFreeRate = 0.10, maturity = 39/365, volatility = 0.40)

xoEuropeanOptionImpliedVolatility("call", value=5.58,underlying=259.39, strike=263, dividendYield=0.0, riskFreeRate=0.0, maturity=39/360,volatility=0.10)

blackScholes(259.39, 263, 39/365,0.10,0.40)

bs <- BlackScholesArray("call", seq(200,300,0.1),270.00, maturity = seq(0/365,39/365,1/365), volatility = 0.40, riskFreeRate = 0.10)

bs$maturity <- bs$maturity * 365

plot_mean <- ggplot(bs, aes(x = underlying, y = price)) +
    geom_line(aes(color=factor(maturity))) 



Order(operation="sell",type="put", underlying = 259.39, strike = 263, maturity = 39/365, volatility = 0.40, riskFreeRate = 0.10)

bs <- BlackScholes(type="put", underlying = 259.39, strike = 263, maturity = 39/365, volatility = 0.40, riskFreeRate = 0.10)

bss <- BlackScholesPriceArray(type="put", seq(230,270,1), strike = 263, maturity = 39/365, volatility = 0.40, riskFreeRate = 0.10)

b <- BearPut(267,266,273,39/365,0.40,0.10)
plot_mean <- ggplot(b$profile, aes(x = underlying, y = price)) +
    geom_line(aes(color=factor(maturity))) 


Delta(bs)
Theta(bs)
Vega(bs)
Rho(bs)
Gamma(bs)

Greeks(bs)

bs <- BlackScholes(type="put", underlying = 258.39, strike = 263, maturity = 39/365, volatility = 0.40, riskFreeRate = 0.10)

bs <- BlackScholes(type="call", underlying = 259.39, strike = 263, maturity = 39/365, volatility = 0.40, riskFreeRate = 0.10)
Delta(bs)
Theta(bs)/365
Vega(bs)
Rho(bs)
Gamma(bs)

do.call(BlackScholes,bs$parameters)
theta("c",S=259.39,X=263,Time=39/365,r=0.00,b=0.0,sigma=0.40)
theta("c",S=259.39,X=263,Time=39/365,r=0.00,b=0.0,sigma=0.40)/365


GBSOption(TypeFlag = "c", S = 259.39, X =263, Time = 39/365, r = 0.10, sigma = 0.40,b=0.10)
sapply(c('delta', 'gamma', 'vega', 'theta', 'rho'), function(greek) 
   GBSGreeks(Selection = greek, TypeFlag = "c", S = 259.39, X = 263, 
             Time = 39/365, r = 0.10, b = 0.10, sigma = 0.40))

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
