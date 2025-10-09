#Cohort Component Model of Projections (female dominant perspective, closed population), by JE Romero-Prieto, PhD#
#https://github.com/Romero-Prieto/teaching#

rm(list = ls()) #Clear all generated data if any.#
#install.packages("readxl")#
#install.packages("stringr")#
library(readxl) #Uses a library to read excel data.#
library(stringr) #Uses a library to work with strings.#
input             = read_excel("~/Documents/Lectures/Projections/Preston 6.xlsx") #Reads the data of Preston et al. 2001. "Population projections." Pp. 117-137 in Demography: measuring and modeling population processes. Malden, MA: Blackwell Publishers.#
input             = as.matrix(input) #Read the data as a matrix.#
female            = input[,c("age","nLx_f","nFx","nNx_f")] 
View(female)

#Some important definitions#
year              = 1993.0 #January 1st population.#
ages              = nrow(female) #The number of age groups.#
radix             = 100000 #The radix of a life table.#
n                 = 5 #The length of the age intervals—which is the same for all.# 
SRB               = 1.05 #The Sex Ratio at Birth.#

nLx               = female[,"nLx_f"]
nSx               = c(nLx[1]/(n*radix),nLx[2:(ages - 1)]/nLx[1:(ages - 2)],nLx[ages]/sum(nLx[(ages - 1):ages])) #Calculation of survivorship ratios.#
nFx               = female[,"nFx"]
nGx               = (nFx + c(nFx[2:ages]*nSx[2:ages],0))*n/2 #Incorporates specific assumptions about fertility.#
lESliE            = matrix(0,ages,ages) #Defines a space for the Leslie matrix.#
lESliE[1,]        = nGx*1/(1 + SRB)*nSx[1] #Inserts multipliers to project the first age group—depending on the age distribution of women in reproductive ages.#
lESliE[ages,ages] = nSx[ages] #Inserts multipliers to project (part of) the open-ended age group.#

for (x in 1:(ages - 1)) {
  lESliE[x + 1,x] = nSx[x + 1] #Inserts multipliers to project the remaining age groups.#
}
current           = female[,"nNx_f"]
projected         = lESliE%*%current #Matrix multiplication simplify the projection to one single operation. Note the command for matrix multiplication using R.#

#Let's produce a simple table of results, projecting the population 3 consecutive periods holding fertility and mortality constant (i.e., the Leslie matrix is always the same).#
female            = input[,c("age","nNx_f")] #Retains only relevant information.#
colnames(female)[colnames(female) == "nNx_f"] <- str_c("nNx_",sprintf("%.1f", year)) #Renames the current population using the reference year.#

for (t in seq(year, year + 2*n, n)) {
  projected = lESliE%*%female[,str_c("nNx_",sprintf("%.1f", t))] #Projects the population n years using the most recent data.#
  colnames(projected)[1] <- str_c("nNx_",sprintf("%.1f", t + n)) #Gives a name to the projected data.#
  female = cbind(female,projected) #Concatenates the table to include the new set of projections.#
}

print(female)

#Problem set 1:#
#a. Calculate the life expectancy at birth of this population.#
#b. Calculate the Total Fertility Rate of this population.#
#c. Calculate the Net Reproduction Rate of this population.#
#d. Calculate the projected number of births from 1993.0 to 1998.0 by age group of the mother.#
#e. Calculate the projected total number of births from 1993.0 to 1998.0#
#f. Calculate how many are projected to be male births.#
#g. Calculate the projected number of births from 1993.0 to 1998.0 by cohort of the mother.#

#Solution:#
#a. Calculate the life expectancy at birth of this population.#
sum(nLx)/radix
#b. Calculate the Total Fertility Rate of this population.#
sum(nFx*n)
#c. Calculate the Net Reproduction Rate of this population.#
sum(nFx*nLx/radix)*(1/(1 + SRB))
#d. Calculate the projected number of births from 1993.0 to 1998.0 by age group of the mother.#
#Short answer.#
current           = female[,"nNx_1993.0"]
B                 = n*nFx*(current + lESliE%*%current)/2
#Same answer with a fancy table.#
femaleS           = female[,c("age",str_c("nNx_",sprintf("%.1f", year)),str_c("nNx_",sprintf("%.1f", year + n)))]
B                 = matrix(rowSums(femaleS[,c(2,3)])/2*n*nFx,ages,1)
colnames(B)[1] <- str_c("B[",sprintf("%.1f", year),",",sprintf("%.1f", year + n),")")
femaleS           = cbind(femaleS,B)
#e. Calculate the projected total number of births from 1993.0 to 1998.0#
sum(B)
#f. Calculate how many are projected to be male births.#
sum(B)*(SRB/(1 + SRB))
#g. Calculate the projected number of births from 1993.0 to 1998.0 by cohort of the mother.#
B                 = matrix(rowSums(femaleS[,c(2,3)])/2*n*nFx,ages,1)
x                 = femaleS[,"age"]
cohort            = matrix(paste(as.character(year - x - n), as.character(year - x - 1), sep = "-"),ages)
colnames(cohort)[1] <- "Birth Cohort"
B                 = matrix(femaleS[,str_c("nNx_",sprintf("%.1f", year))]*nGx,ages,1)
colnames(B)[1] <- str_c("Bc[",sprintf("%.1f", year),",",sprintf("%.1f", year + n),")")
femaleC           = data.frame(cohort,B)
print(femaleS)
print(femaleC)
