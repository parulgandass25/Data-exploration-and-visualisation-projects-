# 1st Tast of the assignment
library(ggplot2)

#Reading the dataset into R
data <- read.csv("assignment-02-data-formated.csv")

#changing datatype of value to numeric
data =data %>% mutate(new_value=  gsub("%", "", paste(data$value)))
data=transform(data,new_value = as.numeric(new_value))

#gglot for bleaching varies over the years for each type of coral and for each site
 

