S <- 40    #Current Price
sigma <- 0.3 #volatility
K <- 45      #Strike Price
r <- 0.02     #risk free interest rate
T <- 0.5      #amount of years
N <- 40    #amount of iterations

k <- 4    # amount of strata
nk <- N/k # vars per strata -> N divided by amt of Strata

bs_call <- function(S, sigma, K, r, T)
{
    d1 <- (log(S / K) + (r + 0.5 * (sigma ^ 2)) * T) / (sigma * sqrt(T))
    
    return(S * pnorm(d1) - K * exp(-r * T) * pnorm(d1 - sigma * sqrt(T)))
}

stratSampling <- function(S, sigma, K, r, T, N)
{

	unif <- t(cbind(runif(nk, 0, 1/4), runif(nk, 1/4, 1/2), runif(nk, 1/2, 3/4), runif(nk, 3/4, 1)))

	# matrix: each row 'i' is a set of random vars with distribution: x conditional on falling on strata i
	dist <- qnorm(unif)
	
	# terminal price version of matrix above
	y <- S * ( exp(T * (r - ((sigma^2) / 2)) + (sigma * sqrt(T) * dist)) )
	
	# discounted payoff version of matrix above
	x <- (exp(-r * T)) * (pmax(matrix(0, nrow=k, ncol=nk), y-K))
	
	# array: cumulative average of each column
	est <- cumsum(colSums(x)) / seq(4, N, 4)
	
}

opY <- matrix(nrow = N/4, ncol = 2)
bsc <- bs_call(S, sigma, K, r, T)

opY[, 1] <- stratSampling(S, sigma, K, r, T, N)
opY[, 2] <- rep(bsc, N/4)

options(repr.plot.width = 9, repr.plot.height = 4.5)

#matplot(seq(4, N, 4), opY, type = "l", lty = 1,
 #       xlab = "Simulations", ylab = "Option Price",
  #      main = "Monte Carlo price vs. analytical price", col = c(4,2))

#legend("topright", legend = c("mc_call", "bs_call"), col = c(4,2),
 #      pch = 20, bty = "n", cex = 0.8, y.intersp = 1.5)
	

