# Statistics, practical 6

# 6.1 Create a random sample of 10 values drawn from a β(2,2) distribution. Plot a
# histogram of the data values overlaid with a line describing the distribution 
# Test this sample for normality using Shapiro-Wilk, Kolgomorov-Smirnov and Anderson-
# Darling tests. Are the data values normally distributed? What can you infer about the
# distribution from which the data values are drawn using only the evidence from the tests?

mybeta <- rbeta(10,2,2)
hist(mybeta, main="ß(2,2)", xlab="value", col="green")
lines(density(mybeta,adjust=2), col="red")


# Test for Normality
# The null hypothesis is that the sample is drawn from normal distributionShapiro-Wilks:
shapiro.test(mybeta)

# W = 0.8726, p-value = 0.1072 P-value above 0.05, therefore failed to reject the null hypothesis .
# Kolgomorov-Smirnov :
ks.test(mybeta,"pnorm",mean(mybeta), sd(mybeta))
# D = 0.2129, p-value = 0.681 alternative hypothesis: two-sided alternative hypothesis: two-sided
# P-value above 0.05, therefore failed to reject the null hypothesis

# Anderson-Darlington test :
ad.test(mybeta)
# A = 0.5159, p-value = 0.1427 P-value above 0.05, therefore failed to reject the null hypothesis
# All test failed to reject the null hypothesis, therefore we can assume the data is normally
# distributed .
  

# 6.2 For the β(2,2) distribution, increase the number of random values drawn to 50,
# to 100 and 1000. What are the p-values of each of the three normality tests in each case? Why
# do they change? For random samples of size N=40, what is the power of each of the three
# tests to detect the non-normality of data drawn from a β(3,2) distribution?

#  Normality tests for random sample of 50 :
mybeta <- rbeta(50,2,2)
# Shapiro-wilks:
  shapiro.test(mybeta)
# W = 0.9612, p-value = 0.09933 P-value above significance level 0.05, failed to reject null hypothesis

# Kolmogorov-smirnov:

  ks.test(mybeta,"pnorm",mean(mybeta), sd(mybeta))

# D = 0.126, p-value = 0.3738 alternative hypothesis: two-sided
# P-value above significance level 0.05, failed to reject null hypothesis

# Anderson-darling:
  ad.test(mybeta)
# A = 0.5553, p-value = 0.1444
# P-value above significance level 0.05, failed to reject null hypothesis

# Normality test for random sample of 100:
  mybeta2 <- rbeta(100,2,2)
# Shapiro-wilks:
  shapiro.test(mybeta2)
# W = 0.9776, p-value = 0.08594
# P-value above significance level 0.05, failed to reject null hypothesisKolmogorov-smirnov:
  ks.test(mybeta,"pnorm",mean(mybeta2), sd(mybeta2))

# D = 0.113, p-value = 0.5096 alternative hypothesis: two-sided
# P-value above significance level 0.05, failed to reject null hypothesis

# Anderson-darling:
  ad.test(mybeta2)
# A = 0.5405, p-value = 0.1616
# P-value above significance level 0.05, failed to reject null hypothesis

# Normality test for random sample of 1000:
  mybeta3 <- rbeta(1000,2,2)
# Shapiro-wilks:
  shapiro.test(mybeta3)

# W = 0.9842, p-value = 6.281e-09
# P-value below significance level 0.05, we can reject the null hypothesis

# Kolmogorov-smirnov:
  ks.test(mybeta3,"pnorm",mean(mybeta3), sd(mybeta3))

# D = 0.0432, p-value = 0.048
# alternative hypothesis: two-sided
# P-value below significance level 0.05, we can reject the null hypothesis
  
# Anderson-darling:
  ad.test(mybeta3)
# A = 3.4201, p-value = 1.48e-08
# P-value below significance level 0.05, we can reject the null hypothesis

# What are the p-values of each of the three normality tests in each case? Why
# do they change?
# We failed to reject the null hypothesis in all test with sample size 50 and 100, but the p-value
# decrease below the significance level with sample size 100.
# The sample size determines the amount of sampling error inherent in a test result. Increasing sample
# size is a way to improve the statistical power of a test.

  
# For random samples of size N=40, what is the power of each of the three
# tests to detect the non-normality of data drawn from a β(3,2) distribution?
outputS <- NULL
outputK <- NULL
outputA <- NULL
for ( i in 1:5000) {
  mybeta <- rbeta(40,3,2)
  s <- shapiro.test(mybeta)
  outputS[i] = s$p.value
  k <- ks.test(mybeta,"pnorm",mean(mybeta), sd(mybeta))
  outputK[i] = k$p.value
  a <- ad.test(mybeta)
  outputA[i] = a$p.value
}
#Fraction of non-normal data obtain from Shapiro-wilks
fractionS= sum(outputS<0.05)/length(outputS)
fractionS

#Fraction of non-normal data obtain from Kolmogorov-smirnov
fractionK= sum(outputK<0.05)/length(outputK)
fractionK

#Fraction of non-normal data obtain from Anderson-darling
fractionA= sum(outputA<0.05)/length(outputA)
fractionA

# Shapiro-wilks test is the test with more power, detecting a bigger fraction of non-normal data,
# followed by Anderson-darling and the weakest is Kolmogorov-smirnov.

# 6.3 Check the normality of the expression levels all of the 3051 genes in the Golub
#(1999) dataset for the ALL patients using the Anderson-Darling test. What fraction of the
# distributions of gene expression levels are not normal?

for ( i in 1:3051) {
  ad <- ad.test(golub[i,gol.fac=="ALL"]) # check for normality using Anderson-Darling test for
  # expression levels of all genes for ALL patients
  output[i] = ad$p.value # store in a vector the gene and the p value
}
fraction= sum(output<0.05)/length(output)
# [1] 0.3926581  fraction of non-normal data for all genes from ALL patients

# 6.4 Plot separate histograms of the expression levels for the gene CCND3 Cyclin
# D3 (row 1042 of the Golub data) for the data from ALL and for AML patients.

geneCCND3all <- golub[1042,gol.fac=="ALL"]
geneCCND3aml <- golub[1042,gol.fac=="AML"]
par(mfrow=c(2,2))
hist(geneCCND3all, breaks=10, main= "CCND3 Cyclin D3 ALL patients", xlab="Expression
level", xlim=c(-2,3), ylim=c(0,12) )
hist(geneCCND3aml, breaks=10, main= "CCND3 Cyclin D3 AML patients", xlab="Expression
level", xlim=c(-2,3), ylim=c(0,12))

# Do the expression levels differ between the ALL patients and the AML patients?
# The spread of the histogram for ALL patients looks larger than AML patients, more variability and
# the shape is left-skewed, with more data for expresssion levels concentrated between 2 and 3. Also,
# the ALL patients histogram reflect more data for expression levels than AML histogram.
# The shape of AML histogram looks with negative kurtosis with the tails spread.


 
# Test for any difference in expression levels between the two groups using the Mann-Whitney-
# Wilcox (MWW) rank sum test (wilcox.test).
wilcox.test(geneCCND3all, geneCCND3aml)

# W = 284, p-value = 6.15e-07
# The p-value is belown the level of significance 0.05, therefore failed to accept the null hypothesis.

# The null hypothesis is equality of means for both samples

# Test for any difference using the Kolgomorov-Smirnov (KS) test (ks.test).
ks.test(geneCCND3all, geneCCND3aml)

# D = 0.835, p-value = 4.792e-06
# The p-value is below the level of significance, therefore failed to accept the null hypothesis.

# The null hypothesis is that both samples are drawn from the same distribution


# 6.5 For how many of the 3051 genes of the Golub data set is the distribution of
# expression levels different between the ALL and AML patients?

for ( i in 1:3051) {
  ks <- ks.test(golub[i,gol.fac=="ALL"], golub[i,gol.fac=="AML"], exact=FALSE) # compare with ks.test ALL and AML patients for each gene
  output[i] <- ks$p.value # store in a vector the gene and the p value
  }
  # How many genes the distribution of expression levels different between the ALL and AML patients
  length(output[output<0.05])
  