#Demographic Methods - Practical 2 (Mortality and Standardisation)
#https://github.com/Romero-Prieto/teaching#

# Synopsis
# In this practical, you will learn how to retrieve data from an online repository (GitHub) to estimate crude death rates, age-specific mortality rates, and standardised death rates. These calculations will be used to answer a number of questions about direct and indirect standardisation. Building your R programming skills, this activity will demonstrate how to work with long- and wide-format data and how to present your results using basic plots.

# The heading of the R Script
rm(list = ls())                                                                 #To clear all data if any.
install.packages("ggplot2")                                                     #If not yet installed, to install the package ggplot2 for plots.
library(ggplot2)                                                                #To load the package.

GitHub               = "https://raw.githubusercontent.com/Romero-Prieto/teaching/main/Demographic%20Methods/practical_2.csv" #declare the URL from where the data will be downloaded.
data                 = read.csv(GitHub)                                         #Retrieves the data from a GitHub repository.

# Data inspection
# We have already used the function **class(*object*)** to determine the class of an object. The class of an object defines the different properties, associated methods, and programming rules. Knowing the class of data, we can inspect their content using the function **print(*object*)**, as shown below.
class(data)                                                                     #Determines the class of the data. 
print(data)                                                                     #Prints the data in the console.

# Data preparation
countries            = sort(unique(data[ , "Country"]))                         #Defines a character variable listing the names of the countries in alphabetical order.
countries                                                                       #To print the names of the countries.                          

ages                 = length(unique(data[ , "x"]))                             #Creates a numeric variable informing the number of age intervals. This number can be used for calculations.
ages                                                                            #To print the number of age intervals.

data[, "nMx"]        = data[, "Deaths"]/data[, "Population"]                    #Creates a new column with age-specific mortality rates calculated from input data.

data[, "age"]        = data[, "x"] + data[, "n"]/2                              #Creates a new column with midpoint ages (for plots).
tail(data, 5)                                                                   #To print the last 5 rows of the data frame, showing the open-ended age intervals.

sEL                  = (data[, "x"] == data[ages, "x"])                         #Identifies all rows of the data frame corresponding to the open-ended age intervals.
data[sEL, "age"]     = data[sEL, "x"] + 1/data[sEL, "nMx"]                      #Calculates the mid-point of each open-ended age interval.
data[sEL, ]                                                                     #To print the rows of the data frame corresponding to the open-ended age intervals.

# Demographic analysis
nNx                  = unstack(data, Population ~ Country)                      #Creates a wide-form data frame of mid-year populations, where each column corresponds to a country (in alphabetical order) and each row corresponds to an age interval.
nDx                  = unstack(data, Deaths ~ Country)                          #Creates a wide-form data frame with number of deaths, using the same dimensions.
nMx                  = unstack(data, nMx ~ Country)                             #Creates a wide-form data frame with age-specific mortality rates.

W                    = sweep(nNx, 2, colSums(nNx), "/")                         #Creates a wide-form data frame with age-specific weights. Note that we cannot simply use nNx/colSums(nNx). Instead, we can use the function sweep(object, margin, vector, function) to apply a function to each column (margin = 2) of nNx, using the vector of total populations for each country.
print(W)                                                                        #To inspect age-specific weights for each country.
colSums(W)                                                                      #To check that the sum of weights for each country is equal to 1.


# Exercise 1: Age-specific mortality rates and direct age-standardisation
# a.	Compute age-specific death rates for both countries and plot those on one graph. Discuss the differences between the two countries.
# Hint: Age-specific mortality rates can be calculated directly by dividing the number of deaths by the mid-year population for the same calendar year. R does not require any special notation for this calculation.
nMx                  = nDx/nNx

# Hint for plotting: the function **ggplot()** has a special structure to control the features of a plot. The basic structure is **ggplot(*name of the database*, aes(x = *name of the variable, x-axis*, y = *name of the variable, y-axis*, color = *name of the variable defining the groups of the plot*, group = *name of the variable defining the groups of the plot*)) +  geom_line() +  geom_point()**. Use the command **help("ggplot")** to identify some other inputs that will control the features of a plot such as the scale, labels, etc.  
# If plotting a figure becomes challenging or time-consuming, you can either skip this part—to be discussed later during the solution— or ask Copilot to provide a code example.
lnM = ggplot(data, aes(x = age, y = nMx, color = Country, group = Country)) +
  geom_line() + geom_point() + 
  labs(title = "Age-Specific Mortality Rates", x = "Age", y = "nMx (log scale)") + 
  theme_minimal() + 
  theme(legend.position = "right", legend.direction = "horizontal", legend.title = element_blank()) + 
  scale_y_log10() + 
  scale_x_continuous(breaks = seq(0, 90, 10))

M = ggplot(data, aes(x = age, y = nMx, color = Country, group = Country)) +
  geom_line() + geom_point() + 
  labs(title = "Age-Specific Mortality Rates", x = "Age", y = "nMx") + 
  theme_minimal() + 
  theme(legend.position = "right", legend.direction = "horizontal", legend.title = element_blank()) +
  scale_x_continuous(breaks = seq(0, 90, 10))

plot(lnM)                                                                       #To plot nMx on a log scale.
plot(M)                                                                         #To plot nMx on a linear scale.

# b.	Compute the (unstandardised) crude death rates, CDR, for both countries and discuss the results.
# Hint: Crude Death Rates can be calculated by dividing the total number of deaths by the total number of people. As a convention, death rates could be reported in thousands. You can use the R function colSums(nDx) to calculate the total number of deaths for each country, considering each country is represented by one row of nDx.
CDR                  = colSums(nDx)/colSums(nNx)*1000                           #Calculates the crude death rate for each country.
sprintf("CDR in %s is %.2f deaths per 1,000", countries, CDR)                   #Reports the crude death rate for each country in a formatted string.

# c.	Compute the standardised death rates for both countries (use the average age distribution of the two countries as the standard); and discuss the results.
# The average age distribution can be computed in the following manner: (i) compute the relative age distribution for each country (i.e., the proportion of the population in each age group); and (ii) take the average of the two proportions for each age group.   
# Hint: The standard is calculated as the average of $W$, and $W$ has been previously calculated. You can use the R function rowSums(W) to calculate the sum by rows and then divide by the number of countries. Finally, standardised rates can be calculated as the weighted average of the age-specific mortality rates.
standard             = rowSums(W)/length(countries)                             #Calculates a standard as the average age distribution across countries.
SDR                  = colSums(nMx*standard)*1000                               #Calculates the standardised death rate for each country.
sprintf("the SDR is %s: %.2f deaths per 1,000", countries, SDR)                 #Reports the standardised death rate for each country in a formatted string.

# d.	How would the standardised rates be different if we had used the Swedish age distribution as the standard? What if we had used the Kazakh age distribution?
# Hint: You can repeat the previous step, using as the standard the column of W specific to each country.
SDR_Sweden           = colSums(nMx*W[, "Sweden"])*1000                          #Calculates the standardised death rate using the Swedish age distribution.
SDR_Kazakhstan       = colSums(nMx*W[, "Kazakhstan"])*1000                      #Calculates the standardised death rate using the Kazakh age distribution.
tABle                = t(data.frame(CDR, SDR, SDR_Sweden, SDR_Kazakhstan))      #To consolidate all results in one table.
print(tABle)                                                                    #To print the table of results.


# Exercise 2: Indirect standardisation
# Let’s assume that we didn’t know the age distribution of deaths for Kazakhstan, but that we had an estimate of the total number of deaths 64,572, and the age distribution of the population.
# In such circumstances, we can no longer conduct direct age-standardisation but can still compare the mortality regime in the two populations via indirect standardisation. Compute the Comparative Mortality Ratio, CMR (sometimes also referred to as the Standardised Mortality Ratio, SMR), and interpret the result.  
# Hint: You can use the function sum(nDx) to calculate the "observed number of deaths" in Kazakhstan (i.e., the second column of nDx). To calculate the "expected number of deaths", assuming Kazakhstan had Sweden’s age-specific mortality rates (nMx), compute the sum of the product of Kazakhstan’s nNx and Sweden’s nMx.
SMR                 = sum(nDx[, "Kazakhstan"])/sum(nMx[, "Sweden"]*nNx[, "Kazakhstan"]) #To calculate the standardised mortality ratio (SMR) for Kazakhstan, using Sweden's age-specific mortality rates as the standard.
sprintf("SMR is %.2f", SMR)                                                     #To report the SMR in a formatted string.


# Optional steps to save your results
getwd()                                                                         #To identify the default working directory.

setwd("~/Documents/Demographic_Methods/Practical_2")                            #To change the working directory to a specific folder. Works if these folders exist in your computer. You can also create a new folder using the function dir.create(path).
getwd()                                                                         #To identify the new working directory.

ggsave("lnM.png", plot = lnM, width = 5, height = 4)                            #To save the log-scale plot of age-specific mortality rates to a PNG file in the current working directory.
ggsave("M.png", plot = M, width = 5, height = 4)                                #To repeat the same command for the linear-scale plot of age-specific mortality rates.

pATh                 = "~/Documents/Demographic_Methods/Practical_2/"           #To define the path to a specific folder where the plots will be saved. You can change this path to any other location on your computer.
ggsave(paste0(pATh, "lnM.png"), plot = lnM, width = 5, height = 4)              #To save the log-scale plot of age-specific mortality rates to a PNG file in a specific folder.
ggsave(paste0(pATh, "M.png"), plot = M, width = 5, height = 4)                  #To repeat the same command for the linear-scale plot of age-specific mortality rates.