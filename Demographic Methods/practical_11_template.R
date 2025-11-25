#Demographic Methods - Practical 11 (Migration), by JE Romero-Prieto, PhD#
#https://github.com/Romero-Prieto/teaching#
# The heading
rm(list = ls())                                                                 #Clearing all generated data if any.
rm(list = ls())
#install.packages("data.table")
#install.packages("tidyverse")
#install.packages("ggplot2")
library(data.table)                                                             #To make high-performance data manipulation using tables.
library(tidyr)                                                                  #To make data transformations, e.g., from wide form to long form.
library(ggplot2)                                                                #To draw plots.

# Exercise 1: Survival Method
# The data
x                    = seq(5, 35, by = 5)                                       #Returns a sequence of numbers from 5 to 35, in increments of 5.
n                    = rep(5,7)                                                 #Returns a vector that repeats the number 5 a total of 7 times.
nNx_1891             = c(1702, 1613, 1486, 1399, 1239, 1049, 916)               #The function c() combines values into a vector or list.
nNx_1901             = c(1748, 1671, 1639, 1648, 1496, 1274, 1111)
nLx                  = c(3885, 3812, 3747, 3665, 3564, 3445, 3255)
data                 = data.table(x, n, nLx, nNx_1891, nNx_1901)                #Consolidates all inputs into a single table.
as.data.frame(data)                                                             #Prints the table as a data frame.

# Example Questions:
# a. Using the 5Lx values, calculate survivorship ratios (5Sx) for a five-year period.
# Hint: this is a straightforward calculation, considering the five-year length of the age intervals.
nSx                  = tail(data[["nLx"]],-1)/head(data[["nLx"]],-1)            #Functions head(object, -index) and tail(object, -index), are used to omit the first or the last element of an object.
x                    = seq(5, 30, by = 5)                                       #Returns a sequence of numbers from 5 to 30, in increments of 5.
n                    = rep(5,6)                                                 #Returns a vector that repeats the number 5 a total of 6 times.
S5                   = data.table(x, n, nSx)                                    #Consolidates results into a single table.
print(data.frame(S5), row.names = FALSE)                                        #Prints the table as a data frame, omitting the name of the rows.

# b. Calculate survivorship ratios (10Sx) for a 10-year period.
# Hint: this can be approached as the ratio of survival across two consecutive age intervals of five years each.
nSx                  = tail(data[["nLx"]],-2)/head(data[["nLx"]],-2)            #Functions head(object, -index) and tail(object, -index), are used to omit the first two or the last two elements of an object.
x                    = seq(5, 25, by = 5)                                       #Returns a sequence of numbers from 5 to 25, in increments of 5.
n                    = rep(10,5)                                                #Returns a vector that repeats the number 5 a total of 6 times.
S10                  = data.table(x, n, nSx)                                    #Consolidates results into a single table.
print(data.frame(S10), row.names = FALSE)                                       #Prints the table as a data frame, omitting the name of the rows.

# c. Using the survivorship ratios and the population in 1891, calculate the number of survivors after five years of exposure to the risk of dying (i.e., the expected population in 1896).
# Hint: Mind the ages. The population from 5 to 9 in 1896 cannot be calculated, as the population from 0 to 4 five years earlier was not available for this exercise.  
data[["nNx_1896p"]]  =  c(NA,S5[["nSx"]]*head(data[["nNx_1891"]],-1))           #Calculates the product of the 1891 population and the 5-year survivorship ratios. The oldest cohort in 1891 should be omitted.
print(data.frame(data[, c("x", "n", "nNx_1896p")]), row.names = FALSE)          #Prints results as a data frame, omitting the name of the rows.
# Congratulations! If you have reached this point, you have learned how to project a population five years forward using a life table with five-year age intervals. This topic will be revisited in Population Dynamics and Projections.

# Compulsory Questions:
# a. Using the survivorship ratios and the population in 1891, calculate the number of survivors after 10 years of exposure to the risk of dying (i.e., the expected population in 1909).
# Hint: Mind the ages. The population from 5 to 14 in 1901 cannot be calculated, as the number of births and population from 0 to 4, 10 years earlier was not available for this exercise. Report your results using the function "print()".
data[["nNx_1901p"]]  =  c(rep(NA,2),S10[["nSx"]]*head(data[["nNx_1891"]],-2))   #Calculates the product of the 1891 population and the 10-year survivorship ratios. The oldest two cohorts in 1891 should be omitted.
print(data.frame(data[, c("x", "n", "nNx_1901p")]), row.names = FALSE)          #Prints results as a data frame, omitting the name of the rows.

# b. Using the result of the previous question and the population in 1901, calculate the estimated number of (net) migrants in 1901.
# Hint: Calculate the difference between observed and projected population in 1901.
data[["NM_1901"]]    =  data[["nNx_1901"]] - data[["nNx_1901p"]]                #Calculates net migration as the excess of the observed population in 1901 over the projected population.
print(data.frame(data[, c("x", "n", "NM_1901")]), row.names = FALSE)            #Prints results as a data frame, omitting the name of the rows.

# c. Why are these values treated as estimates? Mention one key assumption.  
# The resulting values are based on the projected number of survivors rather than the actual number (e.g., subtracting the observed number of deaths for each cohort). nLx could come from either a period or a cohort life table, and they are exact estimates if derived from a cohort life table. However, the same life table with 5-year age intervals was used to project the population 10 years forward, making an implicit assumption of a synthetic cohort (i.e, each cohort is projected using the mortality schedules of two adjacent cohorts).  
# Assumptions: Censuses are complete. The life table accurately represents the mortality conditions between the two censuses. No mortality selection on migration (i.e., migrants face the same mortality schedules).  

# d. Calculate the total sum of (net) migrants aged 15 to 39 in 1901.
# Hint: Sum all estimates of net migration, either by collapsing the table or by using the "sum()" function.
print(data.frame(data[, list(NM_1901 = sum(NM_1901, na.rm = TRUE))]), row.names = FALSE) #Collapses and prints the variable containing age-specific estimates of net migration, omitting NA values.

# e. Use the survivorship ratios to project the 1901 population 10 years backward.
# Hint: Divide the population by the survivorship ratios to project the population backward.
data[["nNx_1891p"]]  =  c(1/S10[["nSx"]]*tail(data[["nNx_1901"]],-2),rep(NA,2)) #Projects the population backward.
print(data.frame(data[, c("x", "n", "nNx_1891p")]), row.names = FALSE)          #Prints results as a data frame, omitting the name of the rows.


# Exercise 2: Migration Expectations (Application of Sullivan’s Method to Migration Data)
# The data
GitHub               = "https://raw.githubusercontent.com/Romero-Prieto/teaching/main/Demographic%20Methods/practical_11.csv"
data                 = read.csv(GitHub)                                         #Reads a table of age-specific mobility data along with two columns from a life table.
data                 = as.data.table(data)                                      #Defines data as a table, which is convenient for data analysis.
e85                  = 6.106                                                    #Declares a scalar representing life expectancy at age 85.
print(data.frame(data), row.names = FALSE)                                      #Prints inputs as a data frame, omitting the name of the rows.

# Example Questions:
# a. Using the columns lx and nax, calculate the number of person-years lived in the age interval (nLx).
# Hint: Reuse code from previous lectures.
x                    = data[["x"]]                                              #Extracts a vector containing the exact ages from the data.
lx                   = data[["lx"]]                                             #Extracts a vector containing the number of survivors from the data.
nax                  = data[["nax"]]                                            #Extracts a vector containing the number of years lived by those who died within the age interval. Observe that this value is always less than one, indicating that it is expressed as a proportion of n.
n                    = c(diff(x,1),NA)                                          #Calculates n using the exact ages (x).
sEL                  = !is.na(n)                                                #Identifies which ages do not correspond to the open-ended age interval.
ndx                  = c(-diff(lx,1),lx[!sEL])                                  #Calculates the number of deaths in a life table.
nLx                  = n*(lx - ndx) + n*nax*ndx                                 #Calculates nLx for closed age intervals.
nLx[!sEL]            = e85*lx[!sEL]                                             #Calculates nLx for the open-ended age interval, using the life expectancy at age 85.
print(data.frame(x, n, nax, lx, ndx, nLx), row.names = FALSE)                   #Consolidates an incomplete life table and prints the table as a data frame, omitting the name of the rows.

# b. Calculate the number of person-years lived above age x (Tx) and the life expectancy (ex), to complete a life table.
Tx                   = rev(cumsum(rev(nLx)))                                    #Calculates Tx.
ex                   = Tx/lx                                                    #Calculates ex.
print(data.frame(x, n, nax, lx, ndx, nLx,Tx, ex), row.names = FALSE)            #Consolidates a full life table and prints the table as a data frame, omitting the name of the rows.

# Compulsory Questions:
# a. If you aim to collect similar data for a different country, what question would you add to a population census or survey?  
# What was your place of residence 12 months ago? a. Same county b. Different county, same state c. Different state d. Different country

# b. Why are mobility data missing for individuals less than one year old? How are mobility data collected for children who can be included in a survey or census but cannot be actually interviewed?  
# Since they were not alive 12 months ago, the population less than one year old at the time of the survey is not eligible for migration questions. Surveys and censuses may rely on proxy respondents, with this information provided by parents or legal guardians.

# c. In your opinion, what would be the reason for reporting mobility data for ages 15–19 and 60–64 using irregular breakdowns?  
# The need to provide detailed rates for ages affected by college enrollment and retirement. Two reasons for internal migration in the United States.

# d. Which types of migratory movements are missing from these data?  
# The table does not include migratory movements from abroad or those representing outmigration. International out-migrants cannot be interviewed; however, these movements may be reported by family members who remain in the country.

# e. Calculate the migration rates.  
# Hint: As usual, age-specific migration rates can be calculated by dividing the number of moves during the past year by the mid-year population. The reported moves are mutually exclusive; therefore, the total number of internal moves is equal to the sum of all categories. Create a subsidiary table of migratory movements and divide it by the mid-year population to estimate rates. Define the total migration rate as the sum of all categories. Report your results using the function "print()".
rates                = data[,c("within_counties","within_states","between_states")]/data[["population"]] #Calculates internal migration rates by dividing the number of events within the last 12 months by the mid-year population, assuming no more than one movement during the year.
rates[["all"]]       = rowSums(rates)                                           #Calculates a new column adding all categories of internal migration.
print(data.frame(x, n, rates), row.names = FALSE)                               #Prints results as a data frame, omitting the name of the rows.

# f. Calculate the lifetime migration expectancy at age 1 for migratory movements within counties, within states, between states, and for all internal migration movements. Interpret your results.  
# Hint: Following Sullivan’s method, multiply the table of migration rates by the number of person-years lived within the age interval (nLx) to estimate the person-years lived while changing place of residence. Then, calculate the corresponding number of person-years lived above age x and the lifetime expectancy associated with each category of internal migration. To do so, reuse code from previous lectures to calculate Tx as the reverse cumulative sum of multiple columns, each representing a category of internal migration.
nLx_M                = rates*nLx                                                #Calculates the person-years lived while changing place of residence, using Sullivan's assumptions.
Tx_M                 = apply(apply(apply(nLx_M, 2, rev), 2, cumsum), 2, rev)    #Calculates the corresponding values of Tx.
ex_M                 = Tx_M/lx                                                  #Calculates migration expectations.
print(data.frame(x, n, ex_M), row.names = FALSE)                                #Prints results as a data frame, omitting the name of the rows.
# Interpretation: The U.S. population is expected to live round(ex[x == 1],3) years at age 1, of which round(ex_M[x == 1,"all"],3) years will involve changing their place of residence. round(ex_M[x == 1,"between_states"],3) years migrating from one state to another, r round(ex_M[x == 1,"within_states"] + ex_M[x == 1,"within_counties"],3) years migrating within the same state, and round(ex_M[x == 1,"within_counties"],3) migrating within the same county. Given the assumption of one movement per calendar year, a lifetime migration expectancy of 6 years indicate 6 moves in life.


# Optional Questions:
# a. An expert suggests that 2020–21 is a problematic period for analyzing migration due to the COVID-19 pandemic and the resulting lockdowns and mobility restrictions. Propose a research design to test this hypothesis, quantifying the effect of the pandemic on lifetime migration expectancy (use 250 words or fewer).  
# Age-specific migration rates during the COVID-19 pandemic can be compared with those observed before and after the pandemic. Since mortality increased during the same period, the difference in lifetime migration expectancy can be decomposed into two main effects: the decline in migration rates and the rise in mortality rates. Data requirements include age-specific migration rates and life tables for periods before and after the COVID-19 pandemic. Inasmuch as year-to-year differences may be small, statistical methods should be applied to estimate confidence intervals (e.g., bootstrapping techniques for migration survey estimates).  

## b. Plot the (internal) migration rates.  
xn                   = x + n/2                                                  #Calculates the midpoint of each age interval.
xn[!sEL]             = x[!sEL] + ex[!sEL]                                       #Calculates the midpoint of the open-ended age interval as a function of life expectancy within that interval. 
df                   = tail(data.frame(xn, rates), -1)                          #Creates a data frame that omits the first row.
df                   = df %>% pivot_longer(cols = c(within_counties, within_states, 
                                                    between_states, all), 
                                           names_to = "migration", values_to = "rate") #Reshapes the data frame from wide format to long format.

ggplot(df, aes(x = xn, y = rate, color = migration)) + 
  geom_line(linewidth = 1) + geom_point(size = 2) + 
  labs(title = "Age-specific migration rates", x = "Age", y = "rate") +  
  theme_minimal()                                                               #Plots the migration rates.


# Exercise 3: Researching Migration (Optional)  
# The Living Standards and Measurement Surveys (LSMS) have been conducted by the World Bank since 1980 in order to improve data collection systems in low- and middle-income countries. The first Ghana LSMS was undertaken in 1988-89 and a sample of about 3,200 households was randomly selected. The survey included the following 10 questions on migration that are posed to all household members 7 years of age and older:
# 1. Were you born in [PRESENT PLACE OF RESIDENCE]? 
# 2. Have you ever lived anywhere else? 
# 3. At the time of your birth, was your birthplace a city, large town, medium town, small town, large village, small village, or other?
# 4. How old were you when you left your place of birth for the first time to live somewhere else? (recorded in years)
# 5. What was the main reason you left?
# a. To follow or join family b. Work related c. Marriage d. School e. Adventure f. Escape family problems g. Other
# 6. How long have you lived in [PRESENT PLACE OF RESIDENCE] since your last move? (recorded in years or months if less than 1 year)
# 7. What was the main reason you came to [PRESENT PLACE OF RESIDENCE]?
# a. To follow or join family b. Work related c. Marriage d. School e. Adventure f. Escape family problems g. Other
# 8. From which region of the country were you coming from? (including somewhere else in Africa or somewhere outside of Africa)
# 9. Was the place you were living before coming here a city, large town, medium town, small town, large village, small village, or other?
# 10. How many different places have you lived in for periods of more than 3 months in your life?
  
# Comment on this migration module in the LSMS. What is good about it, what is not so good? What would you do differently? 
# Good:
# The instrument includes questions about the reasons for migration.   

# Not so good:
# The main purpose of question 2 is to determine eligibility for questions 3–10.  
# Question 3 could be used to estimate lifetime migration. Considering that people may migrate for childbirth—particularly in low- and middle-income countries to access medical care or family support—it would be better to ask for the mother’s place of residence at the time of birth.  
# The age at first migration (i.e., question 4) and the duration of the last settlement (i.e., question 6) could be useful for event history analysis. However, individuals may migrate several more times, which are not captured by this survey instrument.  
# Question 8 is redundant and can be inferred from the place of birth (i.e., question 3).  
# Question 9 is also redundant.  
# The number of movements (i.e., question 10) is not useful unless the timing of these movements is probed or established.  

# Different approach:
# I would define a retrospective period of 1 or 5 years before the date of the interview, and I would probe for the number of movements, previous places of residence (name of municipality, state, and urban/rural), and the associated dates of migration.  
# I would retain a question about the reasons for migration, but I would include additional relevant options such as violence, civil unrest, catastrophic natural events, adverse climate conditions, lack of economic opportunities, and retirement.    
# I would ask for the mother’s place of residence at the time of birth.  

# Appendix
# The R Demographer’s Corner: Syntax Examples for Fast Calculations
x                    = seq(0, 9, 1)                                             #Defines x as a sequence of numbers from 0 to 9, in increments of 1.
x
r                    = rep(3, length(x))                                        #Defines r as a vector of the same dimension, indicating a constant value of 3.
r
diff(x, 1)                                                                      #Computes the first difference of the vector x.
rev(x)                                                                          #Reverses the order of the vector x.
head(x, 2)                                                                      #Returns the first two elements of the vector x.
head(x, -2)                                                                     #Returns all elements of vector x except the last two.
tail(x, 3)                                                                      #Returns the last three elements of the vector x (i.e., tail is the opposite of head).
tail(x, -3)                                                                     #Returns all elements of vector x except the first three.
c(2, 5, 4, 7)                                                                   #Consolidates individual elements (either numbers or strings) into a single vector.
c("a", "b", "c")                                                                #Idem.
cumsum(r)                                                                       #Returns the cumulative sum of the vector r.
rowSums(data.frame(x, r))                                                       #Returns the row-wise sum of a table composed of vectors x and r.
colSums(data.frame(x, r))                                                       #Returns the column-wise sum of a table composed of vectors x and r.
apply(data.frame(x, r), 2, cumsum)                                              #Executes a specified function across a set of rows or columns (i.e., dimension 1 or 2) of a given table-like object, for example, the cumulative sum by columns of a table consisting of x and r.                                     
y                    = sample(1:25, length(x))                                  #Returns a sample of a given length (e.g., the same of x), selecting random values from 1 to 25.
z                    = sample(1:25, length(x))                                  #Idem.
df                   = data.frame(x, y, z)                                      #Consolidates a data frame using variables x, y, and z.
df                                                                              #Reports the data frame.
df                   = df %>% pivot_longer(cols = c(y, z), 
                                           names_to = "variable", 
                                           values_to = "value")                 #Reshapes the data frame from wide format to long format. Returns a column vector of y and z, each associated with the corresponding values of the variable x.
print(as.data.frame(df), row.names = FALSE)                                     #Prints the resulting data frame, omitting row names.
ggplot(df, aes(x = x, y = value, color = variable)) + geom_line() + theme_minimal() #Plots y and z as functions of x in the same figure.
