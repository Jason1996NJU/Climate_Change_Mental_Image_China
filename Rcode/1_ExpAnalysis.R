rm(list = ls())
library(tidyverse)
library(lme4)
library(sjstats)
library(knitr) 
library(sjPlot)

# ---
# R_sripts to reproduce results in :

# perceptions of Climate Change in China
# Authors: Jianxun Yang, Dimitrios Gounaridis, Miaomiao Liu, Jun Bi, Joshua P. Newell (2021-04-10)
# Contact: yangjx@smail.nju.edu.cn
# ---


# 1 - Data loading and wrangling
setwd("C:/Users/jianxuny/Documents/GitHub/Climate_Change_Mental_Image_China/Data")  # change to your data path
data <- read.csv('coded_data.csv', header = T, stringsAsFactors = F) 
# Note that our data is already in the long format
# See more detailed variable explanation in our paper

# 2 - 

# 3 - Multilevel regressions
cityBackground <- read.csv('CityBackground.csv', header = T)
data %>%  left_join(., y = cityBackground, by = 'location') -> data
data <- data %>% 
  mutate(image_disaster   = ifelse(topic == 'disaster', 1, 0),
         image_pollution  = ifelse(topic == 'pollution', 1, 0),
         image_cause      = ifelse(topic == 'cause', 1, 0),
         image_health     = ifelse(topic == 'health', 1, 0),
         image_adaptation = ifelse(topic == 'adaptation', 1, 0),
         image_mitigation = ifelse(topic == 'mitigation', 1, 0),
         image_scientific = ifelse(topic == 'scienific terms', 1, 0),
         image_weather    = ifelse(topic == 'weather', 1, 0),
         image_temperature = ifelse(topic == 'temperature', 1, 0))


# GLM
m_weather <- glm(image_weather ~ gender + age + education + perception + fam.income +
                   scale(pc_income) + scale(pm) + scale(avg_temp), 
                 family = binomial, data = data)

m_temperature <- glm(image_temperature ~ gender + age + education + perception + fam.income +
                       scale(pc_income) + scale(pm) + scale(avg_temp), 
                     family = binomial, data = data)

m_pollution <- glm(image_pollution ~ gender + age + education + perception + fam.income +
                     scale(pc_income) + scale(pm) + scale(avg_temp), 
                   family = binomial, data = data)

m_disaster <- glm(image_disaster ~ gender + age + education + perception + fam.income +
                    scale(pc_income) + scale(pm) + scale(avg_temp), 
                  family = binomial, data = data)

m_scientific <- glm(image_scientific ~ gender + age + education + perception + fam.income +
                      scale(pc_income) + scale(pm) + scale(avg_temp), 
                    family = binomial, data = data)

m_health <- glm(image_health ~ gender + age + education + perception + fam.income +
                  scale(pc_income) + scale(pm) + scale(avg_temp), 
                family = binomial, data = data)

m_adaptation <- glm(image_adaptation ~ gender + age + education + perception + fam.income +
                      scale(pc_income) + scale(pm) + scale(avg_temp), 
                    family = binomial, data = data)

m_mitigation <- glm(image_mitigation ~ gender + age + education + perception + fam.income +
                      scale(pc_income) + scale(pm) + scale(avg_temp), 
                    family = binomial, data = data)

m_cause <- glm(image_cause ~ gender + age + education + perception + fam.income +
                 scale(pc_income) + scale(pm) + scale(avg_temp), 
               family = binomial, data = data)


tab_model(m_weather, m_temperature,m_pollution,m_disaster,m_scientific,
          m_health,m_adaptation,m_mitigation,m_cause, 
          show.ci = FALSE, 
          show.se = TRUE, 
          auto.label = FALSE,
          string.est = '',
          string.se = "SE",
          show.icc = TRUE,
          show.aic = FALSE,
          dv.labels = c("weather", "temperature", "pollution", "disaster",
                        "scientific term", "health", "adaptation", "mitigation", "cause"))

