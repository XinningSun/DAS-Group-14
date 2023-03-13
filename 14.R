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

#将数据导入R
coffee <- read.csv("https://raw.githubusercontent.com/martacasero7/DAS-Group-14/main/dataset14.csv")
#清洗数据，删除缺失值
coffee <- na.omit(coffee)
#因子化变量
coffee$Quality_class <- ifelse(coffee$Qualityclass == "Good", "yes", "no")

coffee.select <- coffee %>%
  select(aroma, flavor, acidity, category_two_defects, altitude_mean_meters, harvested, Quality_class)

coffee.data <- coffee.select %>%
  mutate_if(is.character, as.factor)

coffee.data$harvested <- factor(coffee.data$harvested)

levels(coffee.data$Quality_class) <- c("Poor", "Good")

rownames(coffee.data) <- NULL
#GLM分析
model <- glm(Quality_class ~ aroma + flavor + acidity + category_two_defects + altitude_mean_meters + harvested, 
             data = coffee.data,
             family = binomial(link = "logit"))