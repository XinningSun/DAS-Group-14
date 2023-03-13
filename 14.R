library(ggplot2)
library(readr)
library(dplyr)
library(tidyverse)
library(moderndive)
library(gapminder)
library(sjPlot)
library(stats)
library(jtools)
library(reshape2)    
library(ggpubr)

#Import data into R
coffee <- read.csv("https://raw.githubusercontent.com/martacasero7/DAS-Group-14/main/dataset14.csv")
#Clean the data by removing missing values.
coffee <- na.omit(coffee)
#
coffee$Quality_class <- ifelse(coffee$Qualityclass == "Good", "yes", "no")

coffee.select <- coffee %>%
  select(aroma, flavor, acidity, category_two_defects, altitude_mean_meters, harvested, Quality_class)

coffee.data <- coffee.select %>%
  mutate_if(is.character, as.factor)

coffee.data$harvested <- factor(coffee.data$harvested)

levels(coffee.data$Quality_class) <- c("Poor", "Good")

rownames(coffee.data) <- NULL
#GLM
model <- glm(Quality_class ~ aroma + flavor + acidity + category_two_defects + altitude_mean_meters + harvested, 
             data = coffee.data,
             family = binomial(link = "logit"))
