############################### Project Name #########################################

# Script for a 2 sample, independent t-test, with two indepdent groups of variables (test v control)
# Comparing the change (diff) of each group for statistically significant results

# Project Name: 2 sample t-test

# Data source: Sample data from a website test

############################################################################################

# begin by setting the working directory for the files
setwd("C:/Users/******/Documents/R")

#Load the packages and libraries, or d/l any using install.packages("???")
library(dplyr)
library(magrittr)
library(tidyr)
library(readr)
library(stringr)
library(nortest)
library(car)
library(ggplot2)
library(gridExtra)


# Begin by loading in the dataset using the appropriate method

#data1 <- file.choose() # Allows for manual selection from folder location, paired with below
#data <- read_csv(data1)  
data1 <- read_csv("web_data.csv") #loads in the CSV to a tibble, easier to work with
#data1 <- read.csv("file_name.tsv", sep = "\t", header = TRUE)
#data1 <- read.xls("file_name.xlsx")
#data1 <- XLGetRange(sheet = "sheet1", range = "A1:B21", header = TRUE) #only used when Excel workbook is open


#Open the data view
View(data1)

### Basic summary functions to understand the data
names(data1)
head(data1)
str(data1)
summary(data1)

colnames(data1) <- c("url", "start", "end", "group")  #rename columns for simplicity and ease

### Convert any columns to a factor, or other data types, if needed (as.numeric, as.integer, etc.)
data1[,"group"] <- lapply(data1[,"group"],as.factor)  #converts specified column to factor
str(data1)  #double check data types to ensure change happened


#####The next group of code is for transforming and cleaning the data if needed#####


### Create new variables with filtered or subsets of the original dataset
#newdata1 <- data1 %>% filter(data1$column.name == "variable1")  # filter data by rows, returning only rows with a certain variable
#newdata2 <- data1 %>% arrange(column.name, column.name)  # arrange data by rows, returning only rows that match that variable
#newdata3 <- data1 %>% select(CountryName)  # select specific columns, return only specific columns with that variable
#newdata3a <- data1 %>% select(-CountryName, -ZIPcode) # removes specified columns from data frame

### Combine and Compare data sets (like a lookup table)
#newdata8 <- data1 %>% semi_join(data1, lookup_table1, by="column.name")  # shows rows of data that DO match the other lookup table
#newdata9 <- anti_join(data1, lookup_table1, by="column.name")  # shows rows of data that DO NOT match the other table

### If you have start and end results for test and control, calculate the difference and create enw variable
data1 <- mutate(data1, diff = (end - start))  # creates new column based on combination of existing columns
 
### Remove any rows that have an N/A value within a specific column
data1 <- data1 %>% drop_na(start)  #removes rows with NA in specific column
data1 <- data1 %>% drop_na(end)
data1 <- data1 %>% drop_na(diff)
summary(data1)  # will show summary of new data frame, including number of NA's in each column


#####Data transformation and cleansing section complete#####

# Create separate variables for the test and control groups
control <- data1 %>% filter(group =="control")
test <- data1 %>% filter(group =="test")


#Significance testing with some descriptive stats computations for each group (test & control)
data1 %>% group_by(group) %>%
  summarise(min = min(diff),
            first_qu = quantile(diff, 0.25),
            median = median(diff),
            mean = mean(diff),
            third_qu = quantile(diff, 0.75),
            max = max(diff),
            sd = sd(diff))

#Draw a Boxplox of control and test for the difference in observations
ggp <- ggplot(data = data1, mapping = aes(x=group, y=diff, fill=group)) + 
  geom_boxplot()

print(ggp)

#Draw a stripchart with connect line of gain by group

diff_mean <- data1 %>% group_by(group) %>% summarise(diff=mean(diff))

diff_mean

ggp <- ggplot(data = data1, mapping = aes(x=group, y=diff)) + 
  geom_jitter(position = "dodge", color="darkblue") + 
  geom_point(data=diff_mean, mapping = aes(x=group, y=diff), colour="red", group=1) + 
  geom_line(data=diff_mean, mapping = aes(x=group, y=diff), colour="red", group=1)

print(ggp)

#If the red line is parallel, then the means of each group are equal (or close to)#

#Now we can check the normality of the group changes

ggp1 <- ggplot(data = control, mapping = aes(sample = diff)) + 
  stat_qq(color="brown2", size=2) +
  geom_abline(intercept=mean(control$diff),slope=sd(control$diff), color="red", linetype=2) +
  ylab("Control") + ggtitle("q-q plot of Control Group")

ggp2 <- ggplot(data = test, mapping = aes(sample = diff)) + 
  stat_qq(color="#00C094", size=2) +
  geom_abline(intercept=mean(test$diff),slope=sd(test$diff), color="red", linetype=2) +
  ylab("Test") + ggtitle("q-q plot of Test Group")

grid.arrange(ggp1, ggp2, nrow = 2)

#You want all ofthe points to rest along the diagnol line to indicate normal distribution
   #Right skew - points bend up and to the left
   #Right skew - Points bedn down and to the right
   #short tails - S curve from top left to bottom right, following along the line
   #long tails - S curve from bottom left to top right, more variance than expected

#Perform Anderson-Draling test to check for normality
tapply(data1$diff, data1$group, ad.test)
### If p-value is <0.05 data is not normal, if >= 0.05 then it IS normal
### If <0.05 then with 95% confidence the data does not fit the normal disitribution

#Test the homoscedasticity assumption, where the variances in 2 groups are equal
var.test(diff ~ group, data = data1) #still unsure how tor ead results

bartlett.test(diff ~ group, data = data1) #Compare K-squared value to the qchisq critical value
qchisq(.95, 1) #plug in the df from the bartlett.test
#if the K-squared is larger than the qchisq, than there is a significant diff. in variances
#if the p-value is less than 0.05 this would confirm a significant diff. in variances

leveneTest(diff ~ group, data = data1) #if p-value is >0.05, our group variances can be treated as =
#if <0.05 we have unequal variances and have violated the assumption of homogeneity of variances

t.test(diff ~ group, data = data1, var.equal = TRUE) #look for a t-value larger than 2, and a p-value smaller
#than 0.05 to reject the null hypothesis that the 2 group means are equal

#Finally a linear model approach
mod <- lm(diff ~ group, data = data1)
summary(mod)

#At this point you should be able to reject or accept the Null hypothesis

### Once completing your code, export a CSV to a specified location
write.csv(data1, "/Users/**computerUserName**/Desktop/dataName.csv")
