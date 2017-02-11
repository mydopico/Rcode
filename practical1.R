# Statistics, Practical 1

# 1.1 Generate a vector with seq(), start at 1 and end at 9 and length of 17 numbers.

x <- seq(1,9,length=17)



## 1.2 Return the sum of all values in the vector y greater than 3
y <- 1:10
sum(y>3)


## return all values in vector y greater than 3
which(y>3)


## 1.3 Matrix m.p with the numbers from 1 to 10 in first row and their square numbers in second

v <- 1:10 
w <- v^2  
m <- rbind(v,w)


## Generate a vertical table m.log of numbers from 1 to 100 and their log values.

x <- 1:100 

y <- log(x) 

z <- cbind(x,y) 


## 1.4 Add a further row with cubic numbers to the matrix m.p.
## Then calculate the mean of all numbers from 1 to 10, of all their squares and of all their cubes


cubic <- v^3 

m.cubic <- rbind(m, cubic) 

apply(m.cubic, 1, mean) ## Use apply() to calculate the mean of all rows of matrix m.cubic
 

## 1.5 Average weights of the healthy and of the sick patients.

w.list <- list(weights= c(84.3, 103.3, 80.2,  82.0,  87.4,  73.4,  53.7,  77.5,  82.4,  98.6), status= c("H", "H", "H", "H", "H", "H", "D", "D", "D", "D"), levels=c("H", "D"))

s= mean (w.list$weights[w.list$status=="H"]) 
r= mean (w.list$weights[w.list$status=="D"])

## Second way using tapply just in one line

tapply(w.list$weights, w.list$status, mean)
 

## 1.6 Plot the weights of the patients according to patient number and coloured differently for healthy and sick.

##Extract the weights for healthy patients
h= w.list$weights[w.list$status=="H"] 

##Extract the weights for sick patients
d= w.list$weights[w.list$status=="D"] 


plot(h, ty="l", col="red", ylab="weights", main="Plot of weights",ylim=c(0,130)) 
lines(d, ty="b", col="green") 
legend('topright', c("Health","Disease"), lty=c(1,1),col=c("red","green")) 

















