#Demographic Methods - Practical 4 (Life Tables II), by JE Romero-Prieto, PhD#
#https://github.com/Romero-Prieto/teaching#
rm(list = ls())

# Reading the data
x                    = c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85)
lx_m                 = c(100000, 97731, 97203, 96925, 96723, 96364, 95894, 95445, 
                         94958, 94343, 93436, 91883, 89212, 84660, 77483, 66830, 
                         52309, 34968, 18173)
lx_f                 = c(100000, 98179, 97884, 97752, 97658, 97512, 97313, 97068,
                         96761, 96343, 95721, 94669, 92945, 90131, 85492, 77598,
                         65075, 47402, 27701)
e10_m                = 63.885
e10_f                = 66.385
print(data.frame(x,lx_m,lx_f))

# Data preparation
lx                   = cbind(male = lx_m, female = lx_f)                        
e10                  = cbind(male = e10_m, female = e10_f)
n                    = c(diff(x,1),NA)
nax                  = c(0.1,0.4,rep(0.5, length(x) - 3),NA)
sEL                  = !is.na(n)

# Exercise 1: life table revision question (Compulsory)
# Calculate the following values and comment on your results. While you may choose to reconstruct the entire life table starting from the number of survivors, this is not required to solve questions a, b, c, d, and e.

# a. 1p0, 4p1, 5p10, 10p75.
# Hint: All these quantities depend on lx.  
lx[x == 1,]/lx[x == 0,]
lx[x == 5,]/lx[x == 1,]
lx[x == 15,]/lx[x == 10,]
lx[x == 85,]/lx[x == 75,]

# b.	1d0, 4d1, 15d50.  
# Hint: Decumulate lx to calculate the number of deaths in a life table.  
lx[x == 0,] - lx[x == 1,]
lx[x == 1,] - lx[x == 5,]
lx[x == 50,] - lx[x == 65,]

# c.	4q1, 5q5, 15q50.  
# Hint: These quantities can be calculated directly from lx.  
(lx[x == 1,] - lx[x == 5,])/lx[x == 1,]
(lx[x == 5,] - lx[x == 10,])/lx[x == 5,]
(lx[x == 50,] - lx[x == 65,])/lx[x == 50,]

# d.	1L0, 4L1, 5L5, 5L45.
# Hint: Use the general formula for nLx in closed age intervals.  
n[x == 0]*lx[x == 1,] + n[x == 0]*nax[x == 0]*(lx[x == 0,] - lx[x == 1,])
n[x == 1]*lx[x == 5,] + n[x == 1]*nax[x == 1]*(lx[x == 1,] - lx[x == 5,])
n[x == 5]*lx[x == 10,] + n[x == 5]*nax[x == 5]*(lx[x == 5,] - lx[x == 10,])
n[x == 45]*lx[x == 50,] + n[x == 45]*nax[x == 45]*(lx[x == 45,] - lx[x == 50,])

# e.	1M0, 4M1, 10M60.
# Hint: All these quantities depend on ndx and nLx.  
(lx[x == 0,] - lx[x == 1,])/(n[x == 0]*lx[x == 1,] + n[x == 0]*nax[x == 0]*(lx[x == 0,] - lx[x == 1,]))
(lx[x == 1,] - lx[x == 5,])/(n[x == 1]*lx[x == 5,] + n[x == 1]*nax[x == 1]*(lx[x == 1,] - lx[x == 5,]))
(lx[x == 60,] - lx[x == 70,])/(n[x == 60]*lx[x == 65,] + n[x == 60]*nax[x == 60]*(lx[x == 60,] - lx[x == 65,])
                           + n[x == 65]*lx[x == 70,] + n[x == 65]*nax[x == 65]*(lx[x == 65,] - lx[x == 70,]))

# f.	e0 and e1.
# Hint: Start by calculating T10, then find T85 using 75L10. Once nLx has been estimated for all ages, including the open-ended age interval, proceed to compute Tx and ex following standard life table equations.  

# Life table analysis
ndx                  = rbind(-diff(lx,1),lx[!sEL,])
nqx                  = ndx/lx
nLx                  = n*(lx - ndx) + n*nax*ndx
nLx[!sEL,]           = e10*lx[x == 10,] - colSums(nLx[sEL & x >= 10,])
nMx                  = ndx/nLx
Tx                   = matrix(1, length(x), 1)%*%colSums(nLx) - (apply(nLx, 2, cumsum) - nLx) #Matrix or vector multiplication should use the notation "%*%".
Tx                   = apply(apply(apply(nLx, 2, rev), 2, cumsum), 2, rev)      #R function "apply(input, dimension, function)" repeats the same function for each row (dimension = 1) or each column (dimension = 2).
ex                   = Tx/lx

ex[x == 0,]
ex[x == 1,]

# g.	Ask Copilot to estimate the number of years that a newborn female is expecting to live between exact ages 20 and 65.**  
# h.	Ask Copilot to estimate the number of years that a 20-year female is expecting to live between exact ages 20 and 65.**  
  

# Exercise 2: application of the stationary population model (Compulsory)
# A job training program has 150 training positions that are always filled. The program admits 30 new candidates each month. Every month, 10 trainees quit the program while 20 more find a placement. These conditions have prevailed as long as anyone can remember.
# a. What is the monthly rate of leaving the training program?  
30/150*1000                                                                     #exits per 1,000 entries.

# b. What is the probability that a trainee will leave the program by finding a placement?
20/30

# c. How long on average does a trainee stay in the program?
1/0.2                                                                           #months.

# d. Ask Copilot to solve and explain this exercise by including the problem and questions a, b, and c in the prompt.
  

# Exercise 3: additional life table questions based on the data of Exercise 1 (Optional) 
# Hint: Bear in mind that both life tables use the same radix of 100,000 individuals, which does not reflect the slight excess of male births. To aggregate the two populations into a single life table, or to compare quantities that depend on population size—such as the number of survivors or person-years lived—it would be necessary to adjust for the Sex Ratio at Birth. You can assume 105 male births per 100 female births.
# a. Calculate the Infant Mortality Rate.
SRB                  = 1.05
sum(ndx[x == 0,]*c(1.05,1))/sum(lx[x == 0,]*c(SRB,1))*1000

# b. If you were told that there had been 35,000 males aged 5-9 in Athens in mid-1981, would you be able to say how many there would be aged 10-14 exactly 5 years later? What assumptions would you have to make?
# Hint: The command "nLx[x == 5,"male"]" returns the function nLx at x == 5 for the male population.  
nLx[x == 10,"male"]/nLx[x == 5,"male"]*35000
# Assuming a closed population and keeping constant the mortality rates of 1981.  

# c. What proportion of male population is aged 5-9 in the stationary population represented by the life table in Table 1?
nLx[x == 5,"male"]/Tx[x == 0,"male"]

# d. What would be the sex ratio in the 25-29 age group in the stationary populations represented by the life tables in Table 1?
# Hint: you may need to make an assumption about the Sex Ratio at Birth.  
sprintf("%.5f",nLx[x == 25,"male"]/nLx[x == 25,"female"]*SRB)

# e. What would be the Crude Birth Rate in the stationary populations represented by the life table of Athens in 1981? and the Crude Death Rate?
sum(lx[x == 0,]*c(SRB,1))/sum(Tx[x == 0,]*c(SRB,1))*1000

# f. The two questions below pertain to girl twins were born to a woman on her 20th birthday. Her husband was exactly 5 years older than herself. You can assume that the mortality regimes of Athens in 1981 apply to each member of the family.
# 1. What is the probability that both mother and children are alive when the twins celebrate their $10^{th}$ anniversary, but that the father had died?  
  
# P(event) = P(father died) * P(mother survived) * P(one twin survived) * P(other twin survived)  
# P(event) = [1 - l35(m)/l25(m)] * [l30(f)/l20(f)] * [l10(f)/l0(f)]^2
sprintf("%.7f",(1 - lx[x == 35,"male"]/lx[x == 25,"male"])
        *(lx[x == 30,"female"]/lx[x == 20,"female"])
        *(lx[x == 10,"female"]/lx[x == 0,"female"])^2)

# 2. What is the probability that at least one child survived but only one parent is alive 10 years after the birth of the twins?  
# Hint: in order to obtain the probability that two events jointly occur, we have to multiply probabilities (AND = multiplication, assuming event independence); in other to obtain the probability that either one of two events occur we add the two probabilities (OR = addition).  

# P(at least one twin survived) = 1 - P(both twins died)
P1 = 1 - (1 - lx[x == 10,"female"]/lx[x == 0,"female"])^2
sprintf("%.10f",P1)
# P(only one parent survived) = 1 - P(both parents survived) - P(both parents died)
P2 = 1 - (lx[x == 30,"female"]/lx[x == 20,"female"])*(lx[x == 35,"male"]/lx[x == 25,"male"]) - 
  (1 - lx[x == 30,"female"]/lx[x == 20,"female"])*(1 - lx[x == 35,"male"]/lx[x == 25,"male"])
sprintf("%.10f",P2)
# P(event) = P(at least one twin survived) * P(only one parent survived)
sprintf("%.10f",P1*P2)


# Exercise 4: life tables and stationary population theory (Optional)
rm(list = ls())
x                    = c(0, 10, 25, 65, 85)
lx                   = c(1000, 950, 750, 600, 300)
ex                   = c(60.0, NA, 50.0, 17.5, 5.0)
LT                   = data.frame(x,lx,ex)
# Given the stationary population described in the table and the fact that 16.25% of the population is between exact ages 0 and 10, answer the questions:  

# Life table analysis
n                    = c(diff(x),NA)                                            #Defines the length of age intervals based on the values of x.                       
sEL                  = !is.na(n)                                                #Identifies the open-ended age interval.

ndx                  = c(-diff(lx,1),lx[!sEL])                                  #The number of deaths is calculated as the first difference of the number of survivors (no need of nMx and nqx). 
Tx                   = ex*lx                                                    #An incomplete set of Tx is calculated as the product of the life expectancy and the number of survivors.
nLx                  = c(-diff(Tx,1),Tx[!sEL])                                  #An incomplete set of nLx is calculated as the first difference of Tx.
nLx[x == 0]          = Tx[x == 0]*16.25/100                                     #10L0 accounts for 16.25% of T0.

s                    = is.na(nLx)                                               #15L10 is still missing. The variable s is created to identify the missing value.
nLx[s]               = Tx[x == 0] - sum(nLx[!s])                                #The missing value should be equal to the difference between T0 and the sum of all nLx.
Tx                   = rev(cumsum(rev(nLx)))                                    #Tx and ex are recalculated to populate their missing values. 
ex                   = Tx/lx
nMx                  = ndx/nLx                                                  #nMx is calculated as the ratio of the observed number of events ndx, to the exposure to the risk of dying, measured by the number of person-years lived within the age interval nLx.

LT                   = data.frame(x,n,nMx,lx,ndx,nLx,Tx,ex)
print(LT)

# a. What is the death rate in the age interval [65, 85) 20M65?
nMx[x == 65]

# b. What is the value of e10?
ex[x == 10]

# c. Ask Copilot to find the value of $e_{10}$, analysing the data as an expert demographer.
# d. Ask Copilot to calculate the life expectancy at birth for a theoretical population with a constant force of mortality equal to 0.02.
# e. Ask Copilot to calculate $e_{10}$ for the same theoretical population. 