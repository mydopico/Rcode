# Statistics, practical 4


# 4.1 Set a variable k to 10, then get random sample of size k from a Gaussian with
#mean 10, and standard deviation 5. Calculate sample’s mean. Is it close to the true mean of
#10? Change k from 10 to 1000 and see whether you are getting closer to the true mean with bigger sample.

k <- 10
x <- rnorm(k,mean=10,sd=5) # Get random sample of size 10 from a gaussian distribution
k <- 1000
y <- rnorm(k,mean=10,sd=5) # Get random sample of size 1000 from a gaussian distribution
# Calculate the mean of both samples several times
mean(x)
mean(y)

#The mean of the bigger sample (y) is always closer to 10 while the mean of the sample with size=10
#has more variability. As the number of samples is higher, the mean of these distribution get closer to
#the population mean.

# 4.2 We need to investigate the above question more systematically. Set up a for-
# loop that allows you to take 1000 random samples of size k as above, then calculate the mean
# for each sample, store it in vector ms, and plot ms (you can use the function abline to plot a
# straight line on the mean value). Now calculate the standard deviation of ms. Put everything
# in a function:
#  sd.means <- function(k,m=10,s=5) {
#    ...
#  }
# that returns the standard deviation of 1000 means for flexible sample sizes k. Now it’s easy to
# call this function for various k.
# what should be the standard deviation of the 1000 means in terms of k and the original standard deviation of 5?

sd.means <- function(k, n=1000, m=10, sd=5) {
  ms <- NULL
  sd_ms <- NULL
  # i <- NULL
  for ( k in 1:n) {
    x <- rnorm(k,m,sd)
    ms <- c(ms,mean(x))
    sd_ms <- c(sd_ms, sd(ms))
  }
  plot(ms,ylim=c(-1,14), ylab="Sample mean", xlab="Sample size",abline(h=10, col="red"))
  sd1 <- sd(sd_ms)
  mean1 <- mean(ms)
}
sd.means(700) # Call the function with different k
# The sample mean get closer to 10 (true mean) with bigger sample size.

# 4.3 Suppose that we have observed that many Pima Indian woman suffer from
# diabetes. We know that obesity and diabetes are related; we might therefore hypothesize that
# this population is obese on average, where obesity is defined as BMI higher than 30.
# a. What is our research hypothesis? What is the null hypothesis?
# b. Use the sample mean as the test statistics. Suppose we know that the population standard
# deviation σ = 6. If the null hypothesis is true, can you plot the null distribution?
# c. Load the MASS and Pima.tr data by using the command: library(MASS)   data(Pima.tr)
# and calculate the mean of the sample BMI of Pima indian woman from the data.
# d. Plot the sampling distribution? You could plot it on the same range of x axis to compare to
# the null distribution plot. Do you see anything interesting?
# e. Is the observed significance level, p-value, based on your finding from the sample above
# in support of the null hypothesis or can we reject it given a significance level 0.05
# f. Now calculate is the z-score of your sample mean. Can you calculate the p-value using one sided z-test?

# Research hypothesis: Pima indian women are obese, with BMI higher than 30
# Null hypothesis: H0 : μ=30

plot(bmi,dnorm(bmi,30,6), ylab="Probability density", main="Null distribution")

bmi <- Pima.tr[,5] # Extract the bmi column and store in the variable bmi
mean(bmi) # Calculate the mean of bmi

# standard deviation error of the sampling distribution: 0.42
# mean of the sampling distribution: 32.31
plot(bmi,dnorm(bmi,32.31,0.42), ylab="Probability density", main="Sampling distribution")

# The curve of the sampling distribution is narrow compare the null distribution.


pnorm(30,32.31,6) # Calculate p-value

# [1] 0.3501187 Failed to reject null hypothesis as the p-value is above the significance level 0.05.

# population standard deviation: 6
# population mean:30
#sample mean: 32.31
z = (x - μ)/σ # Calculate z score

pnorm(z,0,1) # Calculate p-value
# [1] 0.6498813 Failed to reject null hypothesis, the p-value is greater than significance level 0.05


