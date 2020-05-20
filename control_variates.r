S <- 40    #Current Price
sigma <- 0.3 #volatility
K <- 45      #Strike Price
r <- 0.02     #risk free interest rate
T <- 0.5      #amount of years
N <- 1000    #amount of iterations

bs_call <- function()
{
    d1 <- (log(S / K) + (r + 0.5 * (sigma ^ 2)) * T) / (sigma * sqrt(T))
    
    return(S * pnorm(d1) - K * exp(-r * T) * pnorm(d1 - sigma * sqrt(T)))
}

euCall_ctrlvar <- function(S, sigma, K, r, T, N)
{
    # y is a vector of terminal prices
    # x is a vector of discounted payoffs
    y <- S * (exp(1) ^ ( (T * (r - ((sigma^2) / 2))) + (sigma * rnorm(N, 0, sqrt(T))) ) )
    x <- (exp(1)^(-r * T)) * (pmax(rep(0,N),y-K))
    
    ybar <- mean(y)
    xbar <- mean(x)
    
    mu_y <- S * exp(1)^(r * T)
    
    b <- sum((x - xbar) * (y - ybar)) / sum((x - xbar)^2)
    
    return(xbar - (b * (ybar - mu_y)))
}

opY <- matrix(nrow=(N/10), ncol=2)

opY[, 2] <- rep(bs_call(), N/10)

for(i in 1:(N/10)) {
    opY[i, 1] <- euCall_ctrlvar(S, sigma, K, r, T, i*10)
}

options(repr.plot.width = 9, repr.plot.height = 4.5)

matplot(seq(10, N, by = 10), opY, type = "l", lty = 1, 
        xlab = "Simulations", ylab = "Option Price",
        main = "Monte Carlo price vs. analytical price", col = c(4, 2))

legend("topright", legend = c("mc_call", "bs_call"), col = c(4, 2),
       pch = 20, bty = "n", cex = 0.8, y.intersp = 1.5)