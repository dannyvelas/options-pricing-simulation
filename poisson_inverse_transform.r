Poisson <- function(rate, U)
{
    if(rate < 0)
        throw("Error: Lambda must be greater than or equal to zero.")
    
    if(missing(U))
        U <- runif(1, 0, 1)
    
    e <- exp(1)  # Built-in Euler's Number: (exp(1) = 2.718282)
    
    # "Small n" is the value we intend to return : The very first integer : (0, 1, 2, 3..)
    # where P(N = n) = ((e^(-rate) * (rate^(n)) / n!
    
    # In this case:
    # N = Poisson Random Variable
    # rate = mean of poisson distribution.
    
    # "n" starts at zero:
    # Meaning that on the first iteration of the sum, f <- e^(-rate) * 1
    n <- 0
    f <- e^(-rate)
    
    # While loop for n >= 1
    # Condition: F(n-1) < U < F(n)
    while( U > f )
    {
        n <- n + 1
        f <- f + (e^(-rate) * rate^(n)) / factorial(n)
    }
    
    
    # As soon as F becomes smaller than U, the while loop breaks and we return n.
    # Thus, as soon as F < U, we've found the "n" that satisfies
    # P(N = n) = ((e^(-rate) * (rate^(n)) / n! 
    return (n)
    
}


count <- 10000
sumX <- 0
rate <- 15 # random value chosen for rate


for (i in 1:count)
{
    sumX <- sumX + Geometric(rate)
}

avg = (sumX/count)

print(sprintf("The average of X within 10,000 trials is equal to: %.4f", avg))
print(sprintf("The value of the rate is: %.4f", rate))
print(sprintf("The difference of these values is: %.4f", abs(avg - rate)))
