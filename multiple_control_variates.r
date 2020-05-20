S <- 40    #Current Price
sigma <- 0.3 #volatility
K <- 45      #Strike Price
r <- 0.02     #risk free interest rate
T <- 0.5      #amount of years
N <- 1000   #amount of iterations

md_n <- 4
mt <- 1:md_n / md_n * T

bs_call <- function(S, sigma, K, r, T)
{
    d1 <- (log(S / K) + (r + 0.5 * (sigma ^ 2)) * T) / (sigma * sqrt(T))
    
    return(S * pnorm(d1) - K * exp(-r * T) * pnorm(d1 - sigma * sqrt(T)))
}

asian_call_mult_control_variates <- function(S, sigma, K, r, T, mt, N)
{
    # generate mean vector mu (without initial log(S))
    mu <- (r - 0.5 * sigma ^ 2) * mt
    
    # save the length of mt as n_mt
    n_mt <- length(mt)
    
    # generate covariance matrix
    Sigma <- matrix(nrow = n_mt, ncol = n_mt)
    
    for (j in 1:n_mt) {
        for (k in 1:n_mt) {
            Sigma[j, k] <- sigma^2 * min(mt[j], mt[k])
        }
    }
    
    A <- t(chol(Sigma))
    
    multi_norm <- mu + ( A %*% matrix( rnorm(n_mt*N,mean=0,sd=1), n_mt , N) ) 
    
    # matrix of prices for each monitoring date
    y <- t(S * exp(multi_norm))
    
    # vector of discounted payoffs
    x <- (exp(-r * T)) * (pmax(rep(0,N), rowMeans(y)-K))
    
    mat <- cbind(y, x)
    colnames(mat) <- c("t1", "t2", "t3", "t4", "X")
    
    # vector of coefficients
    b <- coefficients(lm(X ~ t1 + t2 + t3 + t4, data = as.data.frame(mat)))
    
    # vector of expected prices at each monitoring date
    ep <- S * exp(r * mt)
    
    # vector where each element i corresponds to i simulations
    z <- cumsum(x - ((y - t(replicate(N, ep))) %*% matrix(b[2:5], ncol = 1))) / 1:N
}

N <- 1000

md_n <- 4
mt <- 1:md_n / md_n * T

opY <- matrix(nrow = N, ncol = 2)
bsc <- bs_call(40, 0.3, 45, 0.02, 0.5)

opY[, 1] <- asian_call_mult_control_variates(40, 0.3, 45, 0.02, 0.5, mt, N)
opY[, 2] <- rep(bsc, N)

options(repr.plot.width = 9, repr.plot.height = 4.5)