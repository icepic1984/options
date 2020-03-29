# Search for stocks
# https://finviz.com/screener.ashx?v=111&f=cap_mid

black_scholes <- function(S, k, Tm, r, sigma){
  values <- c(1)
  d1 <- (log(S/k) + (r+(sigma^2)/2)*(Tm))/(sigma*sqrt(Tm))
  d2 <- (log(S/k) + (r-(sigma^2)/2)*(Tm))/(sigma*sqrt(Tm))
  
  values[1] <- S*pnorm(d1) - k*exp(-r*Tm)*pnorm(d2)
  values[2] <- k*exp(-r*Tm)*pnorm(-d2)- S*pnorm(-d1)

  values
}

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

