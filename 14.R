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
## Remove outliers from a data frame
### df: a data frame
### var_name: the name of the column containing the variable of interest
### return: the data frame with outliers removed

remove_outliers <- function(df, var_name) {
  # Calculate the lower and upper bounds
  q <- quantile(df[[var_name]], probs=c(0.25, 0.75), na.rm=FALSE)
  iqr <- IQR(df[[var_name]])
  lower <- q[1] - 1.5 * iqr
  upper <- q[2] + 1.5 * iqr
  
  # Subset the data frame to remove outliers
  df <- subset(df, df[[var_name]] > lower & df[[var_name]] < upper)
  
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
