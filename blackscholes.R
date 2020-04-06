# Search for stocks
# https://finviz.com/screener.ashx?v=111&f=cap_mid

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


black_scholes(2540.21,2525, 18/365,0.01,0.64)
black_scholes(2540.21,2545, 18/365,0.01,0.63)

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

