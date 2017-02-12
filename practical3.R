# Statistics, practical 3

# 3.1 Write a function power.table that takes a vector v and a number n and
# generates a matrix with the first column containing the vector, the second column containing
# its squares, the third its cubes, and so on until the n th power. Call power.table with different v
# and n. 

power.table <- function (v, n) { 
  m = cbind(v) # Initialize the matrix m containing the vector v
  for(i in 2:n) { # for loop to run 'n' iterations
    m <- cbind (m, v^i) # Add to the matrix columns containing squares, cubes and so on until the n th power
  }
  return (m) # Return matrix
}
v <- c(2,3,4)
power.table(v, 10) # Pass two arguments to the function, vector v and number n, 10 in this case


# 3.2 runif(1) is a function that generates one uniform random number between 0 and 1. 
# Assign this number to y and test whether y^3 + y^2 + y is less than 0.5. If so it is added to
# vector a. Repeat this step until you have 10 numbers.

# Initialize vector a, y and i
a <- NULL
y <- 0
i <- 0
while (i < 10) { # While Loop is repeated while the condition is true, until get 10 numbers
  y <- runif(1)
  # Generates random numbers between 0 and 1
  if ((y^3 + y^2 + y) < 0.5) { # If this condition is true
    i <- i + 1 # Go to the next step
    a <- c(a, y) # Add y to vector a
  }
}


# 3.3 RNA consists of a sequence of nucleotides A, G, U, and C, where the first two
# are purines and the last two are pyrimidines. The length of a certain microRNA is 22, the
# probability of a purine equals 0.7, and the process of placing purines and pyrimidines is
# binomially distributed (bino).
# (a) Can you now plot the binomial density function?
# (b) What is the probability of 14 purines?
# (c) What is the probability of less than or equal to 13 purines?
# (d) What is the probability of strictly more than 10 purines? please show two ways of calculating it in R ?
# (e) How many purines do you expect? (In other words: What is the mean of the distribution?) what is the standard deviation ?
# (f) Use the command rbinom to generate a random sample of size 1000 from the distribution. Can you plot a histogram of these numbers? What does it look like?

x <- 1:22

# Length of microRNA is 22
plot(x, dbinom (x, size=22, prob=0.7), type="h", ylab="probability", xlab="purines")

dbinom(14,size=22, prob=0.7) # Probability of 14 purines

sum(dbinom(0:13, 22, 0.7)) # Probability of less than or equal to 13 purines

pbinom(10, 22, 0.7, lower.tail=FALSE)

# Calculate cumulative frequencies with pbinom, 'lower.tail=FALSE' give the probabilities P[X > x], so in this case greater than 10 purines.

sum(dbinom(11:22, 22, 0.7))

# Another way of getting the same result, sum the probabilities between 11 and 22.
mean(x) # Mean

sd(x) # Standard deviation

rbinom(1000, 22, 0.7) # Generate a random sample of 1000 from the distribution
hist(rbinom(1000, 22, 0.7)) # Plot the histogram of this sequence


# 3.4 The sequencing error in a sequencing project is on average 3 wrong base pairs
# in 100,000. A genome has length 4,000,000 base pairs. Using a Poisson distribution (pois),
# what is the probability that the sequencing error is in a range of 10 base pairs above and below the mean.

p <- 3/100000 # Probability of sequencing error
n <- 4000000 # Length of the genome
lambda <- p*n
lambda

ppois(10, lambda)
# Lower tail, probability less than or equal to 10

ppois(10, lambda, lower.tail=F) # Probability more than 10

# 3.5 Create a for-loop to plot 10 Gaussian plots (using the lines function), where the
# mean is 0 and you vary the standard deviation sd from 1 to 10 in steps of 1. You might have
# to use the parameter ylim=c(0,plot.height) in the plot command to adjust the height in the
# first plot so that all plots fit. Now, generate Gaussian plots with variable mean (from 0 to 10)
# and a standard deviation of 2. How do these two parameters change the shape of resulting curve?

x <- seq(-5,5, length=100)
i <- 1
plot(x, dnorm(x, mean=0, sd=1), ty="l", ylim=c(0, 0.5))

for (i in 1:10) { # Iterating between 1 and 10
  i <- i + 1
  lines(x, dnorm(x,mean=0, sd=i+1)) # Add new curves with increased 'standard deviation' with every iteration
}

x <- seq(-10,10, length=100)
i <- 1
plot(x, dnorm(x, mean=0, sd=2), ty="l", ylim=c(0, 0.4) )

for (i in 0:10) {
i <- i + 1
lines(x, dnorm(x,mean=i+1, sd=2)) # Add new curves with increased 'mean' with every iteration
}

# 3.6 Plot the Poisson probability distribution dpois with different lambdas (say,
# somewhere between 0.1 and 50). Remember that Poisson distribution calculates probability
# for integer x. What can you say about the value of x with the highest probability and how does it depend on lambda?
# Now, plot a Gaussian distribution on top of a Poisson distribution with lambda equal to 20.
# Which mean and standard deviation give the best fit 

x <- seq(0,20)
plot(x, dpois(x, 3), ty="l", ylim=c(0, 1))

for (l in 0.1:10) { # For loop to iterate between 0.1 and 10
  lines(x, dpois(x,l)) # Add new curves with different lambda in each iteration
}

# As Lambda increase , this makes the curves move to the right and the shape of the
# curves wider. The value of x decreases as the value of lambda increase.

