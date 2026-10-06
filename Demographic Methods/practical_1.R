#2057 Demographic Methods - Practical 1 (Population Composition and Demographic Rates)
#https://github.com/Romero-Prieto/teaching#

# Synopsis
#In this practical you will learn how to pull data from an online repository (GitHub) to create a population pyramid; to calculate excess population of each sex; and to answer a number of questions about the sex and age structure of that population (France, 1967), identifying characteristic shapes, population irregularities, and some of their underlying causes. This analysis is followed by an optional exercise related to demographic rates.  

# Population Composition
# The heading of the R Script
rm(list = ls())                                                                 #To clear all generated data if any.
install.packages("ggplot2")                                                     #To draw plots.
install.packages("ggtext")                                                      #To extend ggplot2's text capabilities.
library(ggplot2)                                                                #To load the package.
library(ggtext)
GitHub               = "https://raw.githubusercontent.com/Romero-Prieto/teaching/main/Demographic%20Methods/practical_1.csv" #To specify the URL from which the data will be downloaded.
data                 = read.csv(GitHub)                                         #To upload the data.
Country              = "France"                                                 #To declare some relevant characteristics such us the country and year.
year                 = 1967

# Data inspection and preparation
class(data)                                                                     #To identify the class of the object.
head(data, 5)                                                                   #To print the first 5 rows of the data frame.
tail(data, 5)                                                                   #To print the last 5 rows of the data frame. As described, we are dealing with a data frame consisting of three columns: Age, Male, and Female, and 101 rows. Age is reported in single years (at last birthday) and ranges from 0 to 100 years.

data[, "Excess_M"]   = pmax(data[, "Male"], data[, "Female"]) - data[, "Female"] #To create a new column containing the excess male population for each age group. The function pmax(vector_1, vector_2, ..., vector_n) can be used to calculate the maximum population across sexes for each age. Keep in mind that data[, "Female"] extracts the "Female" column from the data frame, while data[, "Excess_M"] = ... creates a new column named "Excess_M". Same result would be found using the **\$** notation, to the form **data\$Female**.
data[, "Excess_F"]   = pmax(data[, "Male"], data[, "Female"]) - data[, "Male"]  #To create a new column containing the excess female population for each age group.
data[, "Male"]       = -data[, "Male"]                                          #To swap the sign of the male population values for plotting purposes.
data[, "Excess_F"]   = -data[, "Excess_F"]                                      #To swap the sign of other variables at the left side of the figure for plotting purposes.

# Plotting the population pyramid
age_ticks            = seq(data[1, "Age"], data[nrow(data), "Age"], by = 5)     #To define the age tick marks, using the function seq().
cohort_ticks         = year - age_ticks                                         #To define the corresponding cohort tick marks, using the year of the population and the age ticks.
population_ticks     = seq(-500, 500, 100)                                      #To define the population tick marks, using a set of arbitrary values that are appropriate for France in 1967.
alpha                = 0.40                                                     #To define the level of transparency for the colours, using a value between 0 (fully transparent) and 1 (fully opaque).
fill_colours         = c("Excess_F" = rgb(1.00, 0.00, 0.00, alpha),
                         "Excess_M" = rgb(1.00, 0.00, 0.00, alpha),
                         "Male"     = rgb(0.45, 0.65, 0.20, alpha), 
                         "Female"   = rgb(0.00, 0.55, 0.65, alpha))             #To define the colours for each population in the data frame, using the function rgb() to specify the light intensities of red, green, and blue, as well as the level of transparency.

long                 = reshape(data, direction = "long",
                               varying = c("Excess_F", "Excess_M", "Male", "Female"),
                               v.names = "Population",
                               timevar = "Sex",
                               times   = c("Excess_F", "Excess_M", "Male", "Female")) #To reshape the data frame from wide to long format, using the function reshape().
long[, "Population"] = long[, "Population"]/1000                                #To improve the readability of the figure, rescaling the size of the population.
View(long)

ggplot(long, aes(x = Age, y = Population, fill = interaction(Sex))) +
  geom_col(width = 1) +
  coord_flip()                                                                  #To plot an unformatted population pyramid from minimal inputs, using the function ggplot() to define the data and aesthetic mappings, geom_col() to add the bars, and coord_flip() to convert them into horizontal bars.

  
p = ggplot(long, aes(x = Age, y = Population, fill = interaction(Sex))) +       #To define a ggplot object, p, following a specific format.
  geom_col(width = 1) +                                                         #To define the type of plot, in this case a bar plot.
  coord_flip() +                                                                #To flip the axes, so that the bars are horizontal.
  scale_y_continuous(
    breaks = population_ticks,                                                  #To define the y-axis tick marks, using the population tick marks defined earlier.
    labels = function(x) format(abs(x), big.mark = ",", scientific = FALSE)) +  #To define the y-axis tick labels, using a custom function to format the population values with commas as thousands separators and without scientific notation.
  scale_x_continuous(
    breaks   = age_ticks,                                                       #To define the x-axis tick marks, using the age tick marks defined earlier.
    sec.axis = sec_axis(~ year - ., name = "Cohort", breaks = cohort_ticks),    #To define a secondary x-axis for the cohort, using the sec_axis() function to create a transformation of the primary axis and specify its name and tick marks.
    expand   = c(0, 0)) +                                                       #To remove the padding around the x-axis, using the expand argument to set the lower and upper limits to zero.
  scale_fill_manual(
    values = fill_colours,                                                      #To define the fill colours for each population, using the fill_colours vector defined earlier.
    breaks = c("Male", "Female", "Excess_M"),                                   #To define the order of the legend items, using the breaks argument to specify the levels of the interaction variable.
    labels = c("Male", "Female", "Population Deficit")) +                       #To define the labels for each legend item, using the labels argument to specify the text to display.
  labs(
    title   = paste0(Country, ", ", year),                                      #To define the title of the plot. The paste0() function is used to concatenate the country and year variables without any spaces or separators.
    x       = "Age",                                                            #To define the x-axis label.
    y       = "Population (in thousands)",                                      #To define the y-axis label.    
    fill    = "",                                                               #To define the legend title, in this case an empty string to remove the title.
    caption = "LSHTM - Demographic Methods 2057, using the <i>Human Mortality Database<i>") + #To define the caption of the plot.      
  theme_minimal() +                                                             #To apply a minimal theme to the plot, removing background elements and grid lines.
  theme(
    axis.text.y      = element_text(size = 7),                                  #To define the size of the y-axis tick labels, using the element_text() function to specify the font size.
    axis.text.x      = element_text(size = 7),
    legend.text      = element_text(size = 7),
    legend.key.size  = unit(0.25, "cm"),                                        #To define the size of the legend keys, specifying the size in centimetres.
    legend.position  = "bottom",                                                #To define the position of the legend (at the bottom of the plot).
    legend.margin    = margin(0, 0, 0, 0),                                      #To define the margin around the legend.
    plot.caption     = element_markdown(hjust = 0, size = 6),                   #To define the caption of the plot, using the element_markdown() function to allow for HTML formatting and specifying the horizontal justification and font size.
    panel.grid.major = element_line(linewidth = 0.20),                          #To define the appearance of the major grid lines, specifying the line width.
    panel.grid.minor = element_line(linewidth = 0.10))

p = p +
  annotate("text", x = 70, y = -200, label = "a", size = 4) +
  annotate("text", x = 50, y = -280, label = "b", size = 4) +
  annotate("text", x = 50, y =  280, label = "b", size = 4) +
  annotate("text", x = 56, y = -285, label = "c", size = 4) +
  annotate("text", x = 26, y = -320, label = "d", size = 4) +
  annotate("text", x = 26, y =  320, label = "d", size = 4) +
  annotate("text", x = 20, y = -460, label = "e", size = 4) +
  annotate("text", x = 20, y =  460, label = "e", size = 4)                     #To add annotations to the plot, placing text labels at specific coordinates (x, y) on the plot. Each annotation corresponds to a population irregularity identified in the population pyramid.

plot(p)                                                                         #To display the plot in the RStudio Plots pane.

# Exercise 1: Population Composition (examples)
#Describe the most plausible causes of the population irregularities observed in `r Country` in `r year`, as indicated by labels a to e, and answer questions f to g.

# a. The deficit in the male population 70+ in 1967.
#Military losses in WWI

# b. The deficit in male and female births around 1916, resulting in a dent in the population pyramid at approximately age 51 in 1967.
#Fertility postponement during WWI and the flu epidemic of 1918.

# c. The deficit in the male population aged 45-69 in 1967.
#Military losses in WWII.

# d. The deficit in male and female births around 1942, resulting in a dent in the population pyramid at approximately age 25 in 1967.
#Fertility postponement during WWII.

# e. The increase in the number of births around 1947, resulting in a population hump at approximately age 20 in 1967.
#Baby boom after WWII, thus the war could cause a tempo distortion.

# f. True or false: "The male-to-female sex ratio of `r Country` in `r year` was approximately 1.05." Explain your answer.
#False: the sex ratio at birth is usually around 1.05, but male mortality rates are higher for men than for women and that usually results in sex ratios in the population as a whole that are below unity. The sex structure of late 20th century France was also heavily affected by the war which has had many more male than female casualties.

# g. What could be the cause of the excess male population at all ages below 40?
#In a closed population, an excess male population is expected to disappear by early adulthood as a result of higher male mortality. Therefore, an excess male population sustained at all ages below 40 may be indicative of sex-selective migration at working ages, such as relatively higher male in-migration or relatively higher female out-migration.

# Questions requiring some R calculations (examples)
# 1. Calculate the sex ratio for the population as a whole.
data                 = abs(data[, c("Age", "Male", "Female")]) 
sum(data[, "Male"])/sum(data[, "Female"])

# 2. Calculate the sex ratio for the under-5 population.
sum(data[data[, "Age"] < 5, "Male"])/sum(data[data[, "Age"] < 5, "Female"])

# 3. Calculate the sex ratio at age 0 and explain why this value is close to, but not equal to, the sex ratio at birth.
sum(data[data[, "Age"] == 0, "Male"])/sum(data[data[, "Age"] == 0, "Female"])
#While population at age 0 is a stock, the number births constitute a flow. Therefore, the age-0 population depends on the number of births and the age distribution of deaths during the first year of life. Boys could experience higher mortality than girls during the first year of life. Migration may also play a small role.

# 4. Calculate the sex ratio below each age, from 0 to 100+
cumsum(data[,"Male"])/cumsum(data[, "Female"])


# Exercise 2: Demographic Rates (Optional)
# The following rates were recorded for a fictive population:
CBR                  = 0.0145
CDR                  = 0.0078
CGR                  = 0.0085
# Compute the Crude Rate of Natural Increase (CRNI) and the Crude Rate of Net Migration (CRNM).
CRNI                 = CBR - CDR                                                #To compute the Crude Rate of Natural Increase (CRNI) as the difference between the Crude Birth Rate (CBR) and the Crude Death Rate (CDR).
sprintf("the CRNI is %.2f per 1,000", CRNI*1000)                                #To report the CRNI in a more readable format.

CRNM                 = CGR - CRNI                                               #To compute the Crude Rate of Net Migration (CRNM) as the difference between the Crude Growth Rate (CGR) and the Crude Rate of Natural Increase (CRNI).
sprintf("the CRNM is %.2f per 1,000", CRNM*1000)                                #To report the CRNM in a more readable format.


# Optional steps to save your results
getwd()                                                                         #To identify the current working directory, which is the default location where files will be saved if no specific path is provided.

setwd("~/Documents/Demographic_Methods/Practical_1")                            #To change the working directory to a specific folder.
ggsave("France_1967.png", plot = p, width = 5, height = 4)                      #To save the plot to a file named "France_1967.png" in the current working directory, specifying the dimensions of the saved image in inches.

paste0(Country, "_", year, ".png")                                              #To construct the name of the output file using the country and year variables, resulting in "France_1967.png".
ggsave(paste0(Country, "_", year, ".png"), plot = p, width = 5, height = 4)     #To save the plot again, but using the constructed name.

pATh                 = "~/Documents/Demographic_Methods/Practical_1/"           #To define the path to a specific folder where the output files will be saved.
ggsave(paste0(pATh, Country, "_", year, ".png"), plot = p, width = 5, height = 4) #To save the plot again, but using the specified path and constructed name.
