#Demographic Methods - Practical 3 (Life Tables), by JE Romero-Prieto, PhD#
#https://github.com/Romero-Prieto/teaching#
rm(list = ls())                                                                 #Clearing all generated data if any.#
#install.packages("stringr")
#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("gridExtra")
library(stringr)                                                                #To work with strings (i.e., any character, including letters, numbers, and symbols).#
library(ggplot2)                                                                #To draw plots.#
library(tidyverse)
library(gridExtra)

# Unspecified West African population (1975) #
x                    = c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75)       #The function c() combines values into a vector or list.
nMx                  = c(0.22650,0.03430,0.00198,0.00038,0.00180,0.00252,
                         0.00290,0.00318,0.00352,0.00390,0.00454,0.00490,
                         0.00622,0.00998,0.02066,0.06748,0.31780)

# Life table calculation, given nMx as an input and nax as a fraction of n#
radix                = 100000                                                   #100,000 is a convention, but it could be any number. If the radix = 1, then lx and ndx become probabilities, and nLx and Tx quantify years rather than person-years (i.e., when there is only one person).# 
n                    = c(diff(x,1),NA)                                          #The function "diff(x,n)" returns the nth difference of x. The first difference of ages x is the length of the age intervals. The length of the open-ended age interval is undetermined and marked as "NA".# 
nax                  = c(0.3,0.4,rep(0.5, length(nMx) - 3),NA)                  #Depending on the book, nax could represent a number of years less than n, or simply it could be a fraction of n. The value 0.3 is assumed for high infant mortality and 0.1 for low mortality. Since those are coarse assumptions, for a discussion and precise values see: Romero-Prieto, Verhulst, and Guillot (2024). Estimating 1a0 and 4a1 in a life table: A model approach... Demography 61 (3), 643–664 https://doi.org/10.1215/00703370-11330227#
sEL                  = !is.na(n)                                                #In R, "!" is used to negate a function or a statement, and "is.na()" returns a selection of NA values. Therefore, "!is.na(n)" returns a selection of age intervals that are not undetermined.# 
nqx                  = n[sEL]*nMx[sEL]/(1 + n[sEL]*(1 - nax[sEL])*nMx[sEL])     #"n[sEL]" returns the selection of elements of n satisfying the statement or condition sEL, and "n[!sEL]" the selection of n that is not satisfying sEL.
nqx[!sEL]            = 1                                                        #This is a "memento mori" condition.#
npx                  = 1 - nqx                                                  
lx                   = cumprod(c(1,npx[sEL]))*radix                             #The function "cumprod()" returns the cumulative product of the elements involved.#                             
ndx                  = lx*nqx
nLx                  = n*(lx - ndx) + n*nax*ndx
nLx[!sEL]            = ndx[!sEL]/nMx[!sEL]                                      #This is a closure condition. The open-ended interval should satisfy the following conditions: lx == ndx, Tx == nLx, and ex == 1/nMx.#
Tx                   = sum(nLx) - (cumsum(nLx) - nLx)                           #Tx is the number of person-years levied above age x, then calculated as the sum of nLx from age x to the open-ended age interval.#
Tx                   = rev(cumsum(rev(nLx)))                                    #This is an alternative way of calculating Tx.#
ex                   = Tx/lx
LT                   = data.frame(x, n, nMx, nax, nqx, npx, lx, ndx, nLx, Tx, ex) #Consolidates all functions of a life table.#

print(LT)                                                                       #Prints the full life table.#
print(LT[,c("x","ex")])                                                         #Prints the age and the life expectancy of the life table.#
ex[x == 10]                                                                     #Returns the life expectancy at age 10.


# A. Determine the following quantities: #
# 1. The life expectancy at birth? #
# R Hint: Select form the vector if life expectancy the age that is reflecting the conditions at birth. #   

# 2. The life expectancy at age 40? #
# R Hint: Select form the vector if life expectancy the age that is reflecting prospective mortality conditions at age 40. #

# 3. The probability of dying in infancy? #
# R Hint: If infancy is defined as the firs year of life, select the probability of dying below age 1. #

# 4. The number alive (in the life table) at exact age 50? #
# R Hint: lx represent an attrition process, resulting from a given number of births (the radix). You can return the value of that function at age 50. # 

# 5. The number of (life table) deaths between exact ages 5 and 10? #
# R Hint (the formal): There are two approaches to quantifying deaths within aggregated age intervals: (i) summing the number of deaths across the relevant age intervals; and (ii) differentiating the function lx at the boundaries of the aggregated age interval. #    

# R Hint (the heuristic): This is a 5-year age interval and we are dealing with an abridged life table of 5-year age intervals (except for the first five years of life). There is only one value in this life table satisfying the age interval 5 to 10, i.e., 5d5. #

# 6. The probability of surviving between exact ages 60 and 65? #
# R Hint (the formal): Use the function lx to calculate a conditional probability. The size of the cohort celebrating their 65th birthday divided by the size of the cohort who celebrated their 60th birthday, five years earlier. # 
# R Hint (the heuristic): Considering the age intervals, there is only one value in this life table satisfying this condition, i.e., 5p60. #  

# 7. The number of years lived between exact ages 1 and 5? #
# R Hint: You can sum the number of person-years between exact ages 1 and 5, and then divide by the radix of the life table as you only would have one person. #

# 8. The total number of person-years lived in this life table? #
# R Hint: Function Tx quantifies that. Adding up nLx across all ages could be an alternative solution. # 
  
# B. Plot nqx, ndx, lx and ex against age. Note that some of these quantities pertain to an age group, others to an exact age. Your plots need to reflect that. #
# R Hint: While quantities at exact ages can be plotted at a given value of x, quantities representing age intervals should be plotted at the midpoint of the interval, i.e., x + n/2. The only exception is the open-ended age interval, which should be plotted at x + eₓ. #

# C. What is the probability of dying in the open-ended age interval? Explain your answer. #
# R hint: sEL selects all ages but the open-ended age interval. !sEL negates the statement sEL and returns just the open-ended age interval. You can use !sEL to select the probability of dying in the open ended-age interval. #
