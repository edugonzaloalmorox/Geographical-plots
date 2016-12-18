# Geographic plots 

library(rgdal)
library(foreign)
library(ggplot2)
library(choroplethr)
library(XML)
library(maptools)
library(dplyr)
library(tidyverse)
library(stringr)
library(rio)


gpclibPermit()

setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/geography_districts_england_dic15")

# 1. Get the geographical information of England - county level
#    - transform shape file into a data frame 
################################################################

sectors<- readOGR("/Users/Personas/Downloads/Great Britain Local Authority Districts Council Areas and Unitary Authorities", "Local_UnitaryAuthority")



summary(sectors)


sectors@data$id = rownames(sectors@data)


sectors.points = fortify(sectors, region = "id")

sectors.df = inner_join(sectors.points, sectors@data, by = "id")  #data frame that we use for drawing the map 

# clean England data 
sectors.df.england = sectors.df %>% filter(DESCRIPTIO != "Council Area (Scotland)" | CODE != "W0") %>% filter(str_detect(CODE, 'E0')) %>% arrange(NAME) 

# link

head(sectors.df.england)

sm = s1 %>% select(wave, oslaua, lpa.1, mean_price, entry_rate)

data_mapping = left_join(sectors.df.england, sm, by = c("CODE" = "oslaua" ))

data_mapping = unique(data_mapping)

data_1 = data_mapping %>% filter(wave == 1)

data_test = ggplot(data_1, aes(long, lat, group=group, fill = mean_price)) +
  scale_fill_brewer(type = "seq", palette = "Greens", name = "Entry rate (%)") + geom_polygon()  + geom_path(colour = "black", size =0.25) +
  labs(x = "Longitude", y = "Latitude", title = "Average entry rate - Local Authorities (March 11/February 16)")

ggsave("test_fill.png", data_test, scale = 0.5)





