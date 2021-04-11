rm(list = ls())
library(tidyverse)
library(dplyr)
library(tidyr)
library(viridis)  
library(ggsci)

# To illustrate this typology of the covariates, 
# consider the case of repeated choice of destinations for vacations by families:

# (1) the length of the vacation, the season are choice situation specific variables,
# (2) income, family size are individual specific variables,
# (3) distance to destination, cost are alternative specific variables.

Image_data <- read.csv('C:/2020-doc/3_ClimateChange_Proj/Data_Analysis/03_Sub_Image/Image_data.csv',
                       header = T, stringsAsFactors = F)
str(Image_data)
varselect <- c('SubjID','T2.1','T3.1','T4.1','T.7','gender','age',
               'education','marriage','wor.env','gov.staff','fam.income',
               'Location','Image','sub.topic','topic')
Image_data <- Image_data[, varselect]
Image_data <- rename(Image_data, winterPer = T2.1, summerPer = T3.1,
                     disasterPer = T4.1, trust.gov = T.7)
# var types
ncol(Image_data)
varnumeric <- c('age','fam.income','education','winterPer','summerPer','disasterPer') # edu and income can be 
Image_data[varnumeric] <- lapply(Image_data[varnumeric],as.numeric)
Image_data %>% group_by(as.factor(fam.income)) %>%
  summarise(count = n())

# (1) Family Income
Image_data$fam.income <- ifelse(Image_data$fam.income <=3, 0,
                                ifelse(Image_data$fam.income >=4, 1, Image_data$fam.income))
Image_data %>% group_by(as.factor(education)) %>%
  summarise(count = n())
# (2) Education
Image_data$education <- ifelse(Image_data$education <=5, 0,
                                ifelse(Image_data$education >=6, 1, Image_data$education))
# (3) Climate Change Perception
Image_data <- mutate(Image_data,
                     winterPer = ifelse(winterPer == 1 | winterPer == 2, 1, 
                                        ifelse(winterPer ==3 |is.na(winterPer), 0, 0)),
                     summerPer = ifelse(summerPer == 1 | summerPer == 2, 1, 
                                        ifelse(summerPer ==3 |is.na(summerPer), 0, 0)),
                     disasterPer = ifelse(disasterPer == 1 | disasterPer == 2, 1, 
                                        ifelse(disasterPer ==3 |is.na(disasterPer), 0, 0)),
                     Perception = winterPer+summerPer+disasterPer) %>%
  select(-winterPer,-summerPer,-disasterPer)
# (4) Other Factors
varfactor  <- c('SubjID', 'trust.gov','gender','marriage',
                'wor.env','gov.staff','education','fam.income',
                'Image')
Image_data[varfactor] <- lapply(Image_data[varfactor],factor)

#-----------------------------------------------------------------
#-----------------------------------------------------------------
#-------------------- Sub.Image Analysis -------------------------
#-----------------------------------------------------------------
#-----------------------------------------------------------------

weather.image <- filter(Image_data, topic == 's') %>%
  group_by(sub.topic, Location) %>%
  summarize(weather.loc.num = n())


# data manipulation
Image_data_new <- Image_data %>% 
  select(sub.topic, topic, Location) %>%
  mutate(sub.topic = ifelse(as.character(sub.topic) == 'z', 't-3', 
                            ifelse(as.character(sub.topic) == 'x', 't-4',sub.topic)))
# create new variables for ordering the dataframe
sum_image <- Image_data_new %>%
  group_by(sub.topic, Location, topic) %>%
  summarise(loc.num = n())
sum_image
sum_image$Location[sum_image$Location == 'Longde'] <- 'Guyuan'
topic.sum <- sum_image %>%
  group_by(topic) %>%
  summarize(topic.sum = sum(loc.num)) 
subtopic.sum <- sum_image %>%
  group_by(sub.topic) %>%
  summarize(subtopic.sum = sum(loc.num)) 
sum_image <- left_join(sum_image, topic.sum, by = 'topic')
sum_image <- left_join(sum_image, subtopic.sum, by = 'sub.topic')
sum_image <- sum_image %>%
  arrange(desc(topic.sum), desc(subtopic.sum), desc(loc.num)) 
   
# id.count <- sum_image %>%
#   group_by(sub.topic) %>%
#   summarise(loc.count = n())


# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 2
nObsType <- nlevels(as.factor(sum_image$Location))
# to_add <- data.frame(matrix(NA, empty_bar*nlevels(as.factor(sum_image$topic))*nObsType, ncol(sum_image))) %>% as_tibble()
# colnames(to_add) <- colnames(sum_image)
# to_add$topic <- rep(levels(as.factor(sum_image$topic)), each=empty_bar*nObsType)
# join <- sum_image[,c('topic','topic.sum')]
# join <- unique(join)
# to_add <- to_add %>% 
#   select(-topic.sum) %>%
#   merge(join, by = 'topic')
# sum_image <- rbind(sum_image, to_add)
# sum_image <- sum_image %>%
#   arrange(desc(topic.sum), desc(subtopic.sum), desc(loc.num)) 

#write.csv(sum_image, 'C:/2020-doc/3_ClimateChange_Proj/Data_Analysis/03_Sub_Image/sumimage.csv')

# ===================PLOT==============
sum_image <- read.csv('C:/2020-doc/3_ClimateChange_Proj/Data_Analysis/03_Sub_Image/sumimage_id.csv')

# replace the code with new value
sum_image$sub.topic[sum_image$sub.topic == 'a-1'] <- 'flood'
sum_image$sub.topic[sum_image$sub.topic == 'a-2'] <- 'earthquake'
sum_image$sub.topic[sum_image$sub.topic == 'a-3'] <- 'typhoon/hurricane'
sum_image$sub.topic[sum_image$sub.topic == 'a-4'] <- 'melting iceberg'
sum_image$sub.topic[sum_image$sub.topic == 'a-5'] <- 'rising sea level'
sum_image$sub.topic[sum_image$sub.topic == 'a-6'] <- 'drought'
sum_image$sub.topic[sum_image$sub.topic == 'a-7'] <- 'wild fire'
sum_image$sub.topic[sum_image$sub.topic == 'a-8'] <- 'sand storm'
sum_image$sub.topic[sum_image$sub.topic == 'a-9'] <- 'hail'
sum_image$sub.topic[sum_image$sub.topic == 'a-10'] <- 'doomsday'
sum_image$sub.topic[sum_image$sub.topic == 'a-11'] <- 'others'

sum_image$sub.topic[sum_image$sub.topic == 't-1'] <- 'cold image'
sum_image$sub.topic[sum_image$sub.topic == 't-2'] <- 'warm image'
sum_image$sub.topic[sum_image$sub.topic == 't-3'] <- 'unpredictable temperature'
sum_image$sub.topic[sum_image$sub.topic == 't-4'] <- 'temperature anomaly'

sum_image$sub.topic[sum_image$sub.topic == 's-1'] <- 'severe weather'
sum_image$sub.topic[sum_image$sub.topic == 's-2'] <- 'nice weather'
sum_image$sub.topic[sum_image$sub.topic == 's-3'] <- 'weather/season'
sum_image$sub.topic[sum_image$sub.topic == 's-5'] <- 'proverb'

sum_image$sub.topic[sum_image$sub.topic == 'h-1'] <- 'global warming'
sum_image$sub.topic[sum_image$sub.topic == 'h-2'] <- 'green house effect'
sum_image$sub.topic[sum_image$sub.topic == 'h-3'] <- 'El Nino'
sum_image$sub.topic[sum_image$sub.topic == 'h-4'] <- 'ozone hole'
sum_image$sub.topic[sum_image$sub.topic == 'h-5'] <- 'others'

sum_image$sub.topic[sum_image$sub.topic == 'c-1'] <- 'air pollution'
sum_image$sub.topic[sum_image$sub.topic == 'c-2'] <- 'other pollution'
sum_image$sub.topic[sum_image$sub.topic == 'c-3'] <- 'environmental degradation'

sum_image$sub.topic[sum_image$sub.topic == 'd-1'] <- 'vehicle/industrial emission'
sum_image$sub.topic[sum_image$sub.topic == 'd-2'] <- 'GHG/CO2 emission'
sum_image$sub.topic[sum_image$sub.topic == 'd-3'] <- 'urbanization/human caused'

sum_image$sub.topic[sum_image$sub.topic == 'e-1'] <- 'physical discomfort'
sum_image$sub.topic[sum_image$sub.topic == 'e-2'] <- 'disease/sick/pandemic'
sum_image$sub.topic[sum_image$sub.topic == 'j'] <- 'unhapiness'

sum_image$sub.topic[sum_image$sub.topic == 'f-1'] <- 're. air pollution'
sum_image$sub.topic[sum_image$sub.topic == 'f-2'] <- 're. weather change'
sum_image$sub.topic[sum_image$sub.topic == 'f-3'] <- 're. temperature'
sum_image$sub.topic[sum_image$sub.topic == 'f-4'] <- 'others'

sum_image$sub.topic[sum_image$sub.topic == 'g-1'] <- 'reduce emission'
sum_image$sub.topic[sum_image$sub.topic == 'g-2'] <- 'protect environment'
sum_image$sub.topic[sum_image$sub.topic == 'g-3'] <- 'others'

sum_image$sub.topic[sum_image$sub.topic == 'i'] <- 'socio-economic impact'

sum_image$topic[sum_image$topic == 'a'] <- 'disaster'
sum_image$topic[sum_image$topic == 'c'] <- 'pollution'
sum_image$topic[sum_image$topic == 'd'] <- 'causes/emision'
sum_image$topic[sum_image$topic == 'e'] <- 'health'
sum_image$topic[sum_image$topic == 'f'] <- 'adaptation'
sum_image$topic[sum_image$topic == 'g'] <- 'mitigation'
sum_image$topic[sum_image$topic == 'h'] <- 'scienific terms'
sum_image$topic[sum_image$topic == 'i'] <- 'socioeconomic impact'
sum_image$topic[sum_image$topic == 's'] <- 'weather'
sum_image$topic[sum_image$topic == 't'] <- 'temperature'




# Get the name and the y position of each label
label_data <- sum_image %>% group_by(id, sub.topic) %>% summarize(tot=sum(loc.num))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- sum_image %>% 
  group_by(topic) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]


# Make the plot
p <- ggplot(sum_image) +      
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=loc.num, fill=Location), stat="identity", alpha=0.5) +
  scale_fill_viridis(discrete=TRUE) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
   geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
   geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
   geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
   geom_segment(data=grid_data, aes(x = end, y = 200, xend = start, yend = 200), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
   geom_segment(data=grid_data, aes(x = end, y = 400, xend = start, yend = 400), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
   # Add text showing the value of each 100/75/50/25 lines
   ggplot2::annotate("text", x = rep(max(sum_image$id),5), y = c(0, 50, 100, 200, 400), label = c("0", "50", "100", "200", "400") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) + 
   
   ylim(-300,max(label_data$tot, na.rm=T)) +
   theme_minimal() +
   theme(
     legend.position = "left",
     axis.text = element_blank(),
     axis.title = element_blank(),
     panel.grid = element_blank(),
     #plot.margin = unit(rep(-1,4), "cm") 
   ) +
   coord_polar() +
  
   # Add labels on top of each bar
   geom_text(data=label_data, aes(x=id, y=tot+10, label=sub.topic, hjust=hjust), color="black", fontface="bold",alpha=1, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
   
   # Add base line information
   geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  
   # geom_text(data=base_data, aes(x = title, y = -18, label=topic), hjust=c(1,1,1,1,1,1,1,1,1,1), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE) 
  # Save at png
  ggsave(p, file="output.png", width=12, height=7)

