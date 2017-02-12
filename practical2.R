
# Read in the data frame from the file study.txt
data.df <- read.table (file="study.txt", header=TRUE)

# 2.1: Using data.df, print the data (age, BMI and bp) about female patients only? 

data.df[data.df$"gender"=="female",]

# Print the data only for patients older than 43. 
data.df[data.df$"age">43,]

# Command tapply to calculate the average BMI for male and female separately.
tapply(data.df$BMI,data.df$gender, mean)

# 2.2: Using our.study list, print only the BMI 
# Get the name of the first researcher in the study ? 

our.study <- list(data=data.df, researchers=c("Lia", "Zoe", "Luca"))
d <- our.study$data

# print only the BMI
d$BMI
# or
our.study$data[ ,2]

# get the name of the first researcher in the study
g <- our.study$researchers
g[1:1]

# 2.3: Reproduce the summary by calling appropriate functions individually (min, median, etc.) 
# Extract the interquartile range IQR from the summary (compare the results to the IQR function). 
# How does it compare to the standard deviation of the dimers?

# Upload and read in the file ‘hannes_results_sorted.dat’
interfaces <- read.table("hannes_results_sorted.dat")
# Add the column headers
names(interfaces) <- c("PDBcode","type","X1","area","X2")
# Store the dimers in dimers_area and monomersin monomers_area
dimers_area <- interfaces$area[interfaces$type == "d"]
monomers_area <- interfaces$area[interfaces$type == "m"]

summary(dimers_area)

min(dimers_area)

quantile(dimers_area,0.75)

quantile(dimers_area,0.25)

median(dimers_area)

mean(dimers_area)

max(dimers_area)

IQR <- summary(dimers_area)[5] - summary(dimers_area)[2]

sd(dimers_area)

mean(monomers_area)

max(monomers_area)

min(monomers_area)

median(monomers_area)

quantile(monomers_area,0.25)

quantile(monomers_area,0.75)

# The interquartile range is a robust estimator of the standard deviation. 
# Robust means it is less prone to outliers.