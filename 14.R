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
library(GGally)
library(janitor)

#Import data into R
coffee <- read.csv("https://raw.githubusercontent.com/martacasero7/DAS-Group-14/main/dataset14.csv")

#Clean the data by removing missing values.
coffee <- na.omit(coffee)

#Select the data
coffee <- coffee %>%
  select(aroma, flavor, acidity, category_two_defects, altitude_mean_meters, Qualityclass)
coffee$Qualityclass <- as.factor(coffee$Qualityclass)
levels(coffee$Qualityclass) <- c("Good","Poor")

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

#
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
model <- glm(Qualityclass ~ aroma + flavor + acidity +category_two_defects + altitude_mean_meters,
             data = coffee,
             family = binomial(link = "logit"))

summ(model)

plot_model(model, show.values = TRUE,
           title = "", show.p = FALSE, value.offset = 0.25)

model %>%
  coef() %>%
  exp()

plot_model(model,show.values = TRUE,
           title = "Odds(Good/Poor)",show.p = FALSE)

coffee <- coffee %>%
  mutate(Qualityclass_fitted = fitted(model))
