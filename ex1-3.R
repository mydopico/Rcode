# Statistics, Exercise 1.3: 

# Generate a simple matrix m.p with two rows showing the numbers from 1 to 10 in 
# the first row and their square numbers in the second:
 
x <- 1:10
m.p <- rbind(x,x^2)
m.p <- matrix(m.p,2,10)
# or
m.p=rbind(1:10,(1:10)^2)

# Generate a vertical table m.log of numbers from 1 to 100 and their log values.

x <- 1:100
m.log <- cbind(x,log(x))
# or
m.log=cbind(1:100,log(1:100))
