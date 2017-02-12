
# Read in the data frame from the file study.txt
data.df <- read.table (file="study.txt", header=TRUE)

# 2.1: Using data.df, print the data (age, BMI and bp) about female patients only? 

data.df[data.df$"gender"=="female",]

# Print the data only for patients older than 43. 
data.df[data.df$"age">43,]

# Command tapply to calculate the average BMI for male and female separately.
tapply(data.df$BMI,data.df$gender, mean)


# 2.2: Using our.study list: 

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

# Extract interquartile range from summary and compare result to IQR function
s <- summary (monomers_area) ## Store the results of summary() in the variable 's'

s[2] ## Extract the first quartile or second element stored in the variable 's'

s[5] ## Extract the third quartile or fifth element stored in the variable s

iqr_monomers <- s[5] - s[2] # Calculate the difference between third and first quartile and store in the variable iqr_monomers

(IQR(monomers_area)) ## IQR function to know the interquartile range

## The result of iqr_monomers is the same than IQR function, because the interquartile range is the difference between the third and first quartile.
# I do the same for the dimers_area dataset:
s_d <- summary(dimers_area)

iqr_dimers <- s_d[5] - s_d[2]

(IQR(dimers_area))
# The result of iqr_dimers is the same than IQR function as expected


# Compare to the standard deviation of the dimers

(IQR(dimers_area))

sd(dimers_area)

# Both measures, IQR and sd, indicate amount of variability. IQR consider the variability within 50% of the dataset and standard 
# deviation consider every single value. In skewed distributions is not possible to calculate standard deviation from IQR,
# because there are more outliers or extreme values and standard deviation take into account variation in all values.
# With a normal distribution, not skewed, is possible to calculate the standard deviation from the IQR. In this case, it is skewed.

# The interquartile range is a robust estimator of the standard deviation. 
# Robust means it is less prone to outliers.
