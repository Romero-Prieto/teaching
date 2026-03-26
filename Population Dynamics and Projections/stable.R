#The Stable Population Model, by G Reniers and JE Romero-Prieto
#https://github.com/Romero-Prieto/teaching#
# The heading of the R script:
rm(list = ls())                                                                 #Clear all generated data if any.
#install.packages("stringr")                                                    #For string manipulation.
#install.packages("ggplot2")                                                    #For plots.
#install.packages("tidyr")                                                      #For plots.
library(stringr)
library(ggplot2)
library(tidyr)

GitHub            = "https://raw.githubusercontent.com/Romero-Prieto/teaching/main/Population%20Dynamics%20and%20Projections/Preston_7.csv"
input             = read.csv(GitHub)
input             = as.matrix(input)
print(input)

# Definitions:
# We identify the number of age groups, account for the life expectancy at the open-ended age interval, identify key inputs such as nLx and nMx, and define a vector J of mid-point ages.   
ages              = nrow(input)
e85               = 6.79
nLx               = input[,"nLx"]
nmx               = input[,"nmx"]
J                 = input[,"x"] + c(input[1:(ages - 1),"n"]/2,e85)


# Exercise 1: Lotka's $r$ and the Stable Equivalent Distribution of a Population
# Example Questions:
# a. What is the difference between age-specific maternity and age-specific fertility rates? How would you proceed if only fertility rates were available?
# While age-specific fertility rates quantify the number of births to women x to x + n in a given period of time, age-specific maternity rates quantify the number of daughters born to women in the same age and over the same period. If maternity rates are not available, fertility rates combined with a sex ratio at birth can be used as an approximation.

# b. How can you infer the radix of the life table?
# Considering that nLx values are small—close to, but always less than, the length of the age interval—we can infer that nLx represents one person-year lived. Therefore, the life table radix l0 is equal to 1. The sum of all nLx values is 78.92, which is a reasonable estimate of life expectancy at birth for the 1991 United States female population.
radix             = 1

# c. Calculate the Gross and Net Reproduction Rates (i.e., GRR and NRR) of this population.
GRR               = sum(c(input[1:(ages - 1),"n"],0)*nmx)
NRR               = sum(nLx*nmx)
GRR
NRR


# Compulsory Questions:
# a. Calculate the intrinsic growth rate for this population.
# Hint: Because Lotka’s r quantifies a rate of natural increase, it is reasonable to assume that the population is closed to migration.  
# One way to compute r is through successive approximations (e.g., using an iterative loop). We start from the *best* initial guess of r, which implicitly assumes a generation length of 27 years. The value of ris then updated while the stability condition is not satisfied (i.e., y = 1 with a small tolerance of 10^-10).
r                 = log(NRR)/27
y                 = 0
tolerance         = 10^-10
while (abs(y - 1) > tolerance) {
  y = sum(exp(-r*J)*(nLx/radix)*nmx)
  r = r + (y - 1)/27
}
r

# b. Estimate the crude birth rate (i.e., $b$) and crude death rate (i.e., $d$) of the stable equivalent population.
b                 = 1/sum(exp(-r*J)*(nLx/radix))
b
d                 = b - r
d

# c. Calculate the stable equivalent population distribution.
nCSx              = b*exp(-r*J)*(nLx/radix)
input             = cbind(input,J,nCSx)

# d. Use a plot to compare the actual *vs*. the stable equivalent age distribution of the population.
df                = as.data.frame(input[1:(ages - 1),c("J","nCx","nCSx")])
names(df)        <- c("age", "actual", "stable")

ggplot(
  pivot_longer(df, cols = c(actual, stable), names_to = "series", values_to = "value"),
  aes(x = age, y = value, color = series)
) +
  geom_line() +
  labs(x = "age", y = "proportion in 5-year age group", color = NULL) +
  theme_minimal()

# e. Calculate the mean length of generation $T$.
T                 = log(NRR)/r
T


# Exercise 2: Further Applications of the Stable Population Model
# Giggle is a successful company. It's workforce has doubled every 10 years, and it is the ambition of the management to maintain the same pace of growth. All employees are hired at their 21st birthday and retire at age 61. Retirement is not the only means by which they leave the company. For every 100 new recruits, 80 stay at least 10 years, 60 for 20 years, 40 for 30 years, and 20 for 40 years (from Hinde, A. (1998). *Demographic methods*. London, UK: Arnold Publishers).  
# Hint: Solve the questions below by invoking assumptions that hold for stable populations. Note that retirement is compulsory, so no members of the workforce fall into the open‑ended service interval.  
# Definitions:
x                 = c(0,10,20,30,40)
n                 = c(diff(x,1),0)
J                 = x + n/2 
ages              = length(x)
lx                = c(100,80,60,40,20)
nLx               = c(diff(x,1)*(lx[1:(ages - 1)] + lx[2:ages])/2,0)
age               = 21 + x
input             = as.matrix(cbind(x,n,J,age,lx,nLx))

# Compulsory Questions:
# a. By which percentage does the number of new recruits have to increase in order to guarantee the doubling time of 10 years?
r                 = log(2)/10
str_c(sprintf("%.2f", r*100),"%")

# b. During the first ten years, the employees are under the mentorship of a senior employee (aged 51-60). One senior employee cannot mentor more than 15 junior employees. If the workforce doubles every 10 years, will it be necessary to appoint external mentors? What percentage of the junior staff will have external mentors?
b                 = 1/sum(exp(-r*J)*(nLx/radix))
nCSx              = b*exp(-r*J)*(nLx/radix)
ratio             = nCSx[age == 21]/nCSx[age == 51]
external_mentor   = max(ratio - 15,0)/ratio
str_c(sprintf("%.2f", external_mentor*100),"%")

# Optional Questions:
# a. The external mentoring program turned out to be a failure. The directors want to know what the maximum growth rate is that would allow all training to be done in-house. Can you determine that?
ra                = (log(15) + log(nLx[age == 51]) - log(nLx[age == 21]))/30
str_c(sprintf("%.2f", ra*100),"%")