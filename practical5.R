# Statistics, practical 5

# 5.1 (i) Try to guess the true mean by intuition (or using a plot for example) and perform a one-
#sample t-test using t.test (with the default options of one samples and two-sided hypothesis)
# using your guess. Try a few more possible means, when are you getting a p-value below 0.05?
# (ii) Set up a for-loop testing systematically means in a particular range. It might come handy that
# t <- t.test(x,mu= ...)
# t$p.value
# returns the actual p-value as number (no need to inspect the t.test output). (iii) What is the range
# that always gives p-values below 0.05? When you inspect the t.test output is there something
# that can help you to answer this question?

x <- c(4.07, 10.3, -13.97, 14.14, -5.6)
t.test(x,mu=2) # Perform t-test with the null hypothesis is mu=2

t.test(x,mu=-13) # Perform t-test with the null hypothesis is mu=-13

x <- c(4.07, 10.3, -13.97, 14.14, -5.6)
for (i in seq(-20,20,1)){ # For loop to perform t-test with different mu, in the range of -20 and 20
  t <- t.test(x, mu=i)
  output <- c(i,t$p.value ) # Store in vector output every mu with their p-value
  if (t$p.value < 0.05){ # choose p-value < 0.05 and print them with mu
    print(output)
  }
}

# The ranges of means,that always gives p-value below 0.05, are between -20 and -13 and between
# 17 and 20 .
#If I check the t.test output and the confidence interval, we can see these are the values outside the
# confidence interval: 95 percent confidence interval: -12.54678 16.12278

# 5.2 In lecture 5 we showed the boxplot of Gene CCND3 Cyclin D3. The gene
# expression values are obtained from row 1042 of the golub data matrix. The plot reveals that the
# ALL gene expression values of the gene are positive.
# a. What is your research hypothesis? What is the null hypothesis?
# b. Test this hypothesis using the he built-in-function t-test in R 
# c. What inference can be made about the population?

# Research/alternative hypothesis: ALL gene expression values are positive μ>0
# Null hypothesis: H0: μ=0

source("http://www.bioconductor.org/biocLite.R")
biocLite(c("hopach"))
library(hopach); data(golub)
gol.fac <- factor(golub.cl, levels=0:1, labels= c("ALL", "AML"))
t.test(golub[1042,] ~ gol.fac, var.equal = TRUE, mu=0, alternative="greater") # Two Sample t-test


# The p-value is very small, less than significance value, therefore ALL gene expression values are
# positive and we can reject the null hypothesis.

# 5.3 write your own “reduced” version of the t.test

x <-c(4.07,10.3,-13.97,14.14)
y <- c(16.38,25.03,18.45,15.25,19.74,23.1)
t.test(x,y,var=T) # t.test() command to run two sample t-test

my.t.test <- function (x,y,var=T) { # Function to calculate t-test
  # calculate t value and p value
  m = length(x)
  n = length(y)
  sp = sqrt ( ( ((m-1)*sd(x)^2) + ((n-1)*sd(y)^2) )/(m+n-2) )
  t = (mean(x) - mean(y))/ ( sp*sqrt((1/m)+(1/n)) )
  df = m+n-2 # degrees of freedom of both samples
  p.value = 2*pt(t, df) # p-value
  # return list with these two values:
  list(t=t,p.value=p.value)
}
t.result = my.t.test(x, y)


# I got the the same results of t-value and p-value with t.test() and the function.

# 5.4 (a) Use a boxplot to construct a hypothesis about the experimental effect.
# (b) Test for the equality of means by an appropriate t-test. Use the built-in-function t-test.
# (c) Test the normality of the ALL and AML expression values. First use a (Q-Q) plot and then
# use the Shapiro-Wilk test in R. Was it correct to assume normality in the previous step ?

grep("MYBL2", golub.gnames[,2])

golub[1788,] # Expression values for row 1788 that correspond to the gene MYBL2
boxplot(golub[1788,] ~ gol.fac) # Create a boxplot for ALL and AML expression values for gene MYBL2

# Research/alternative hypothesis: The gene expression values for gene MYBL2 differ between AML
# patients and ALL patients .
# Null hypothesis: H0: μ 1 ≠ μ 2B.

t.test(golub[1788,] ~ gol.fac, var.equal = TRUE) # Run t-test to compare gene expression values for gene MYBL2 between AML and ALL patients.
# Two Sample t-test

# p-value = 0.8597 is above the significance level 0.05, therefore failed to reject the null hypothesis,
# the population means do not differ .

# Q-Q plot
par(mfrow=c(1,2))
qqnorm(golub[1788,gol.fac=="ALL"], main = "Normal Q-Q Plot ALL", xlab = "Theoretical
Quantiles", ylab = "Sample Quantiles")
qqline(golub[1788,gol.fac=="ALL"])
qqnorm(golub[1788,gol.fac=="AML"], main = "Normal Q-Q Plot AML", xlab = "Theoretical
Quantiles", ylab = "Sample Quantiles")
qqline(golub[1788,gol.fac=="AML"])

# We assume normality, most of data are in the line with a few points outside in the extremes.
# Shapiro-Wilk test
# The null hypothesis is that the gene expression values for MYLB2 follow a normal distribution

shapiro.test(golub[1788, gol.fac=="ALL"]) # Run the test with the command shapiro.test()

# W = 0.9565, p-value = 0.3062 P-value greater than 0.05

shapiro.test(golub[1788, gol.fac=="AML"])
# W = 0.9202, p-value = 0.3204
# In both, ALL and AML, P-value greater than 0.05, failed to reject the null hypothesis, therefore
# assumed normal distribution.

# 5.5 (i) Implement the randomization test (10000 samples) using a for-loop to find out whether the
# two following sets of data were generated from the same distribution:
  y1 <- c(4.4, 3.25, 2.56, 8.0, 4.85, 0.5, 4.19)
  y2 <- c(7.75, 9.44, 5.41, 5.06, 3.03, 7.63, 5.91, 6.77, 4.75)
# Use the sample function in R to sample from your combined data.
# (ii) Plot the density of the 10000 t-values and indicate your original t-value on the plot


dif.mean <- mean(y2) - mean(y1) # Difference of the means of the original vectors
new <- c(y1,y2) # Combine the vectors y1 and y2
n.repetitions <- 10000 # The number of repetitions

for (i in 1:n.repetitions) { # For loop to repeat resampling
  s = sample(new,length(new)) # Take a sample from the combined vector
  Sy1 <- s[1:4] # Random sample from combined vector with same size as original vector y1
  Sy2 <- s[5:8] # Random sample from combined vector with same size as original vector y2
  dif.sample[i]<- mean(Sy2) - mean(Sy1) # Difference of all means of the samples generated with the for loop
}
pvalue = sum(abs(dif.sample) >= abs(dif.mean)) / n.repetitions
print (pvalue)
# [1] 0.1863 The p value is greater than the significance level of 0.05 and failed to reject the hypothesis, the
# samples are likely to come from the same distribution.

plot(dif.sample, dnorm(dif.sample, 0,1), ylab="Density", xlab="x")
text(x=0.06, label="original t-value=0.06181", adj=1,col="red") # text command to indicate t-value
abline(v=0.06,col="red") # Vertical line to indicate the position of this t-value


