# Load libraries 
library(tidyverse)
library(dplyr)
library(ggplot2)

# Read in data using Import Dataset or by 
telecom <- read.csv('C:/Users/broadwmc/Desktop/telecom.csv')

# Scope out dataset 
head(telecom) # first x lines
tail(telecom) # last x lines
summary(telecom)
dim(telecom) # dimensions of dataset
colnames(telecom)
str(telecom) # structure of dataset 

# Notice data types. Factors: https://www.stat.berkeley.edu/~s133/factors.html#targetText=Conceptually%2C%20factors%20are%20variables%20in,refered%20to%20as%20categorical%20variables
## R data types: https://swcarpentry.github.io/r-novice-inflammation/13-supp-data-structures/

# Replace non-standard values with standard missing value type, NA
## https://dplyr.tidyverse.org/
telecom %>% # magrittr's piping 
  mutate(TotalCharges = replace(TotalCharges, TotalCharges == "na", NA)) %>%
  mutate(TotalCharges = replace(TotalCharges, TotalCharges == "N/A", NA))
telecom

telecom <- telecom %>% # You have to assign changes to a value for the changes to take permanent place
  mutate(TotalCharges = replace(TotalCharges, TotalCharges == "na", NA)) %>%
  mutate(TotalCharges = replace(TotalCharges, TotalCharges == "N/A", NA))
telecom 

# Cleaning MonthlyCharges column
is.na(telecom$MonthlyCharges) # use is.na() function instead of is.nan(), it'll detect both NA and NaN values (but not na, n/a, or any variation thereof)
telecom %>%
  summarise(count = sum(is.na(MonthlyCharges))) ## get count of missing values 

# Replacing NAs in MonthlyCharges column with median value of MonthlyCharges 
## https://stats.stackexchange.com/questions/143700/which-is-better-replacement-by-mean-and-replacement-by-median
telecom <- telecom %>%
  mutate(MonthlyCharges
         = replace(MonthlyCharges,
                   is.na(MonthlyCharges),
                   median(MonthlyCharges, na.rm = TRUE))) #na.rm = remove na's 

# Cleaning PaymentMethod column
## Part 1: replacing "--" with NA
is.na(telecom$PaymentMethod) # R doesn't catch any of the missing values

telecom <- telecom %>%
  mutate(PaymentMethod = replace(PaymentMethod, PaymentMethod ==  "--", NA))
is.na(telecom$PaymentMethod)
telecom$PaymentMethod

## Part 1a: Regex (regular expressions)
## https://www.ntu.edu.sg/home/ehchua/programming/howto/Regexe.html#targetText=Regular%20Expression%20(Regex)%20Syntax,characters%2C%20metacharacters%20(such%20as%20.
telecom$PaymentMethod <- sub("^\\s+$|^$", "NA", telecom$PaymentMethod)
telecom$PaymentMethod <- na_if(telecom$PaymentMethod, 'NA')

## replace NA with "unavailable"
telecom <- telecom %>%
  mutate(PaymentMethod = replace(PaymentMethod, is.na(PaymentMethod), "unavailable"))
telecom$PaymentMethod

# Exercise: Try and transform "yes" to "Y" and "no" to "N" in the Churn column using the gsub() function 
## http://rfunction.com/archives/2354 
gsub("yes", "Y", telecom$Churn)
gsub("no", "N", telecom$Churn)

# -------------------------------------------------------

# Exploratory Data Analysis (EDA):
## Filter
telecom %>%
  filter(Churn == "yes") %>%
  count()

## Arrange
telecom %>% 
  arrange(desc(MonthlyCharges))
  
ggplot(telecom, aes(x= reorder(customerID,-MonthlyCharges),MonthlyCharges, fill = Churn))+
  geom_bar(stat ="identity") +
  xlab("ID") # visualize it using ggplot2 



