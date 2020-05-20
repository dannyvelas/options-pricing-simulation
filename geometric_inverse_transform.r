Geometric <- function(p, U)
{
    # Users have the option of providing their own U, if they choose not to,
    # a uniform [0,1] U will be generated.
    if (missing(U))
        U <- runif(1, 0, 1)
        
    # The built-in natural logarithm function in R is log(x)
    # Floor is another built-in function that rounds the value of its argument
    # down to the nearest integer
    X = floor(1 + log(U)/log(1-p))
    
    return (X) 
}


# Geometric Testing Function:

# In running the Geometric Inverse Transform Method 10,000 times, the average
# of each output should nearly equal 1/p

count <- 0
sumX <- 0
p <- .3489 # random value chosen for p



for (i in 1:10000)
{
    sumX <- sumX + Geometric(p)
    count <- count + 1
}

avg = (sumX/count)

print(sprintf("The average of X within 10,000 trials is equal to: %.4f", avg))
print(sprintf("The value of 1/p where p is equal to %.4f is: %.4f", p, 1/p))
print(sprintf("The difference of these values is: %.4f", abs(avg - 1/p)))