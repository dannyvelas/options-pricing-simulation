bs_call <- function()
{
	d1 <- (log(S / K) + (r + 0.5 * (sigma ^ 2)) * T) / (sigma * sqrt(T))

	return(S * pnorm(d1) - K * exp(-r * T) * pnorm(d1 - sigma * sqrt(T)))
}
