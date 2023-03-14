library(ggplot2)
library(readr)
library(dplyr)
library(tidyverse)
library(moderndive)
library(gapminder)
library(sjPlot)
library(jtools)   
library(ggpubr)
library(GGally)
library(janitor)

#Import data into R
coffee <- read.csv("https://raw.githubusercontent.com/martacasero7/DAS-Group-14/main/dataset14.csv")

#Clean the data by removing missing values.
coffee <- na.omit(coffee)

#Select the data
coffee <- coffee %>%
  select(aroma, flavor, acidity, category_two_defects, altitude_mean_meters, Qualityclass)

coffee$Qualityclass <- ifelse(coffee$Qualityclass == "Poor", 0, 1)

# Define a function to remove outliers.
##This code defines a function called "remove_outliers", which takes two arguments: a data frame (df) and a variable name (var_name). The function calculates the lower and upper bounds for the variable using the first and third quartiles (quantiles) and the interquartile range (IQR). It then subsets the data frame by removing any rows where the variable value is outside of the lower and upper bounds. Finally, the function returns the modified data frame.)
remove_outliers <- function(df, var_name) {
  quantiles <- quantile(df[[var_name]], probs=c(0.25,0.75), na.rm=FALSE)
  IQR_val <- IQR(df[[var_name]])
  Lower_val <- quantiles[1]-1.5*IQR_val
  Upper_val <- quantiles[2]+1.5*IQR_val
  df <- subset(df, df[[var_name]] > Lower_val & df[[var_name]] < Upper_val)
  return(df)
}

## Call the above function for each variable that needs to remove outliers.
coffee <- remove_outliers(coffee, "aroma")
coffee <- remove_outliers(coffee, "flavor")
coffee <- remove_outliers(coffee, "acidity")
coffee <- remove_outliers(coffee, "category_two_defects")
coffee <- remove_outliers(coffee, "altitude_mean_meters")

coffee$Qualityclass <- as.factor(coffee$Qualityclass)

#Visualize the data

ggplot(data = coffee, aes(x = Qualityclass, y = aroma, fill = Qualityclass)) +
  geom_boxplot() +
  labs(x = "Qualityclass", y = "Aroma") +
  theme(legend.position = "none")

ggplot(data = coffee, aes(x = Qualityclass, y = flavor, fill = Qualityclass)) +
  geom_boxplot() +
  labs(x = "Qualityclass", y = "Flavor") +
  theme(legend.position = "none")

ggplot(data = coffee, aes(x = Qualityclass, y = acidity, fill = Qualityclass)) +
  geom_boxplot() +
  labs(x = "Qualityclass", y = "Acidity") +
  theme(legend.position = "none")

ggplot(data = coffee, aes(x = Qualityclass, y = category_two_defects, fill = Qualityclass)) +
  geom_boxplot() +
  labs(x = "Qualityclass", y = "Category_two_defects") +
  theme(legend.position = "none")

ggplot(data = coffee, aes(x = Qualityclass, y = altitude_mean_meters, fill = Qualityclass)) +
  geom_boxplot() +
  labs(x = "Qualityclass", y = "Altitude_mean_meters") +
  theme(legend.position = "none")

#Show original counts
coffee %>%
  tabyl(aroma, Qualityclass) %>%
  adorn_percentages() %>%
  adorn_pct_formatting() %>%
  adorn_ns()

coffee %>%
  tabyl(flavor, Qualityclass) %>%
  adorn_percentages() %>%
  adorn_pct_formatting() %>%
  adorn_ns()

coffee %>%
  tabyl(acidity, Qualityclass) %>%
  adorn_percentages() %>%
  adorn_pct_formatting() %>%
  adorn_ns()

coffee %>%
  tabyl(category_two_defects, Qualityclass) %>%
  adorn_percentages() %>%
  adorn_pct_formatting() %>%
  adorn_ns()

coffee %>%
  tabyl(altitude_mean_meters, Qualityclass) %>%
  adorn_percentages() %>%
  adorn_pct_formatting() %>%
  adorn_ns()

#GLM
##Fit the GLM model
model1 <- glm(Qualityclass ~ aroma + flavor + acidity + category_two_defects + altitude_mean_meters,
             data = coffee,
             family = binomial(link = "logit"))
summ(model1)

##Remove the values of category_two_defects and fit the GLM model
model2 <- glm(Qualityclass ~ aroma + flavor + acidity + altitude_mean_meters,
             data = coffee,
             family = binomial(link = "logit"))

summ(model2)

model2 %>%
  coef() %>%
  exp()

#Add the fitted values to the coffee data frame.
coffee <- coffee %>%
  mutate(Qualityclass_fitted = fitted(model2))
