#Demographic Methods - Practical 1 (plotting a population pyramid), by JE Romero-Prieto, PhD#

#1. Heading of the program: installing/loading packages or libraries and reading the input data.#
rm(list = ls())                                                                 #Clearing all generated data if any.#
#install.packages("readxl")#                                                    #Packages should be installed only once. Hashtags can be used to add comments or to deactivate lines.#
#install.packages("stringr")#
#install.packages("ggplot2")#
library(readxl)                                                                 #To read excel data.#
library(stringr)                                                                #To work with strings (i.e., any character, including letters, numbers, and symbols).#
library(ggplot2)                                                                #To draw plots.#
library(scales)                                                                 #To format numbers.#
pathfile             = "~/Documents/Demographic_Methods/Practical_1/"           #To define the path.#
input                = read_excel(paste0(pathfile,"France_1967.xlsx"),"Sheet1") #Imports data from an excel file. The function paste0( , , , ) concatenates as many strings as specified, without any spaces in between.#

#2. Some data preparation.#
input                = as.matrix(input)                                         #Reads the data as a matrix, which is convenient for data manipulation.#
year                 = 1967                                                     #Declares the year of data.#
ages                 = nrow(input)                                              #Returns the number of rows or age groups. ncol(input) will return the number of columns or variables.#
data                 = expand.grid(Age = input[,"Age"], Gender = c("Male", "Female")) #Creates a data frame of one record per age-sex.#
data[,"Cohort"]      = year - data[,"Age"]                                      #Generates a variable cohort, using the age and year. data[,"Age"] corresponds to the column "Age" of a matrix named data.#
data[,"Population"]  = rbind(matrix(-input[,"Male"]/1000,ages,1),matrix(input[,"Female"]/1000,ages,1)) #Allocates the inputs to the data frame. Note Males use negative values. rbin(A,B) is a function to concatenate by rows. cbin(A,B) will do the same by columns.#
age_ticks            = seq(input[1,"Age"], input[ages,"Age"], by = 5)           #Generates a sequence of ages using 5-year intervals.#
cohort_ticks         = year - age_ticks                                         #Generates equivalent tick marks from a cohort perspective.#
population_ticks     = seq(-500, 500, 100)                                      #Generates a sequence of population tick marks. seq(initial,final,step) returns any sequence given these three arguments. If the step is not declared, it is assumed to be 1.#
fill_colours         = c("Male"    = rgb(0.45,0.65,0.20,0.35), 
                         "Female"  = rgb(0.00,0.55,0.65,0.35))                  #Allocates colors for each sex, using four numbers between [0,1]. The first three define the color, the last is the transparency.#

#3. The main task.#                                                             #ggplot is a popular library to make a variety of plots. Type help("ggplot") to get the documentation and some examples. Uses an additive structure to control more characteristics of the plot, e.g., ggplot() + coord_flip() + ... + theme_minimal()#
p = ggplot(data, aes(x = Age, y = Population, fill = interaction(Gender))) +
  geom_bar(stat = "identity", position = "identity", width = 1) +
  coord_flip() +                                                                #Flip for standard pyramid layout
  scale_y_continuous(                                                           #To set the tick marks for population counts.#
    breaks = population_ticks,
    labels = function(x) format(abs(x), big.mark = ",", scientific = FALSE)
  ) +
  scale_x_continuous(                                                           #To set the tick marks for the secondary x-axis (age and cohort).#
    breaks = age_ticks,
    sec.axis = sec_axis(~ year - ., name = "Cohort", breaks = cohort_ticks),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = fill_colours) +
  labs(title = "France 1967",                                                   #Defines the title.#
       x = "Age",                                                               #Defines the label of the main x-axis (vertical).#
       y = "Population (in thousands)",                                         #Defines the label of the y-axis (horizontal).#
       fill = "Sex") +                                                          #Defines the title of the legend.#
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8),
        legend.position = "top")

ggsave(paste0(pathfile,"France_1967.png"), plot = p, width = 5, height = 4)     #Exports the figure to a specific location.#



# All ages considered #
sum(input[,"Male"])/sum(input[,"Female"])

# Under-5 population #
sum(input[input[,"Age"] < 5,"Male"])/sum(input[input[,"Age"] < 5,"Female"])

# Age 0, which is close to the SRB #
sum(input[input[,"Age"] == 0,"Male"])/sum(input[input[,"Age"] == 0,"Female"])

# Every single age between 0 and 100+ #
cumsum(input[,"Male"])/cumsum(input[,"Female"])

