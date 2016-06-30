# Plotting spatial data 
########################
# ----------------------------------------------------------
# Geographical data provided by the ONS
# Data on care homes provided by the CQC
# Analysis in three stages 
#     1. Convert shapefile into data frame
#     2. Merge geographical data with data to be analysed (ex. entry rates see: entry_exit_rates.csv for details)
#     3. Plot geographical data 
# ----------------------------------------------------------


library(rgdal)
library(foreign)
library(ggplot2)
library(choroplethr)
library(XML)
library(maptools)
library(dplyr)

gpclibPermit()

setwd("/Users/Personas/Google Drive/Research/PhD/data/boundaries/counties")

# 1. Get the geographical information of England - county level
#    - transform shape file into a data frame 
################################################################

sectors<- readOGR(dsn = ".", layer = "england_ct_2011")

summary(sectors)


sectors@data$id = rownames(sectors@data)


sectors.points = fortify(sectors, region = "id")

sectors.df = inner_join(sectors.points, sectors@data, by = "id") #data frame that we use for drawing the map 


# 2. Data for filling the map 
#############################

panel = read.csv("entry_exit_rates.csv", sep = "," , header = TRUE)


# ! Warning: the calculations have to be since march 2011 - I have to drop observations corresponding to jan 2011
# Calculations to proceed
# ........................

t_analysis =  panel %>% mutate(var =  paste(month, year, sep="_"))
t_analysis$var = as.factor(t_analysis$va)
t_analysis = t_analysis %>% filter(var != "1_2011")
panel = t_analysis %>% select(-var)
# .........................



library(dplyr)
panel = panel %>% select( LA, month, year, entry_rate)

library(data.table)
panel = data.table(panel)
setkey(panel, LA, year)

panel$entry_rate = with(panel, ifelse(is.na(entry_rate), 0, entry_rate))

test= panel[,yearly:= mean(entry_rate),by= LA]

test$year = with(test, as.factor(year))

test = test %>% select(LA, yearly)
test = unique(test)


# check the LA that are shared in the CQC and the ONS (geographical information)
geo_authorities = levels(sectors.df$name)
care_authorities = levels(test$name)

common = intersect(geo_authorities, care_authorities)
dif = setdiff(geo_authorities, care_authorities)

# "City of London" is the one that is not common

counties = sectors.df %>% filter(name != "City of London" & name != "Isles of Scilly")

# merge data frames 

library(plyr)

test =plyr:: rename(test,  c("LA" = "name"))

data_mapping=  merge(counties, test, by = "name") #data frame that we use for drawing the map 

# create a factor variable depending on the level of market entry to fill the map 

data_mapping$entry_cat = with(data_mapping, ordered(ifelse(yearly >= 0.5 & yearly <= 0.75, "0.5 - 0.75",
                                                           ifelse(yearly >0.75 & yearly <= 1, "0.75 - 1", 
                                                                  ifelse(yearly >1 & yearly <= 1.25, "1 - 1.25",
                                                                         ifelse(yearly > 1.25 & yearly <= 1.5, "1.25 - 1.5",
                                                                                ifelse(yearly > 1.5 & yearly <= 1.75, "1.5 - 1.75",
                                                                                       ifelse(yearly > 1.75 & yearly <= 2, "1.75 - 2",
                                                                                              ifelse(yearly >2, "> 2", "other")))))))))  

data_mapping$entry_cat = with (data_mapping, factor(entry_cat, levels = c("0.5 - 0.75", "0.75 - 1", "1 - 1.25", 
                                                                          "1.25 - 1.5", "1.5 - 1.75","1.75 - 2", "> 2" )))


# 3. Plot the map 
#################

library(ggplot2)
library(RColorBrewer)

ex_sector = ggplot(data_mapping, aes(long, lat, group=group, fill = entry_cat)) +
  scale_fill_brewer(type = "seq", palette = "Greens", name = "Entry rate (%)") + geom_polygon()  + geom_path(colour = "black", size =0.25) +
  labs(x = "Longitude", y = "Latitude", title = "Average entry rate - Local Authorities (March 11/February 16)")
ggsave("test_fill.png", ex_sector, scale = 0.5)

# test_fill.png is the file










