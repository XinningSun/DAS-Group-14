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

#Select the data
coffee.select <- coffee %>%
  select(aroma, flavor, acidity, category_two_defects, altitude_mean_meters, Qualityclass)
coffee.select$Qualityclass <- as.factor(coffee.select$Qualityclass)

#GLM
model <- glm(Qualityclass ~ aroma + flavor + acidity +category_two_defects + altitude_mean_meters,
             data = coffee.select,
             family = binomial(link = "logit"))
summ(model)
plot_model(model, show.values = TRUE,
           title = "", show.p = FALSE, value.offset = 0.25)
