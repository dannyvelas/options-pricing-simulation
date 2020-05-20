Exponential <- function(rate, U1)
{
    if(rate < 0)
        throw("Error: Lambda must be greater than or equal to zero.")
    
    if(missing(U1))
        U <- runif(1, 0, 1)
    
    X <- -log(U)/rate
    
    return (X)
}

# Exponential Testing Function:

# In running the Exponential Inverse Transform Method 10,000 times, the average
# of each output should nearly equal 1/rate

count <- 0
sumX <- 0
rate <- 48.3 # random value chosen for rate


for (i in 1:10000)
{
    sumX <- sumX + Exponential(rate)
    count <- count + 1
}

avg = (sumX/count)

print(sprintf("The average of X within 10,000 trials is equal to: %.4f", avg))
print(sprintf("The value of 1/rate where rate is equal to %.4f is: %.4f", rate, 1/rate))
print(sprintf("The difference of these values is: %.4f", abs(avg - 1/rate)))