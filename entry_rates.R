# Create a panel for spatial economics 
# Aim: follow the local authorities and check which ones are the most likely to 
# attract care homes.
# Analysis from march 2011. I aggregate the entries before that. 
# ..............................................................................
# Steps:
#   1. Get the counts for the whole period
#   2. Sum all the observations before 2011
#   3. Get the entry/exit rates from 2011 onwards
# ...............................................................................


# 1 Get the counts 
###################
library(readr)


setwd("/Users/Personas/Google Drive/Research/PhD/data/projects/Active care homes")
data = read.csv("homes_prices.csv", sep = ",", header = TRUE)

data =  data[,-1]


str(data)

library(plyr)
library(dplyr)

# make a dataset easier to manipulate 

casas = data %>%
  filter(Local.Authority != "Unspecified") %>% select(`Local.Authority`, `Location.ID`, `Postal.Code`, `provider.start`:`provider.end`)

#(create a function here ... !)



casas$provider.start = with(casas, as.Date(provider.start, format = "%Y-%m-%d"))
casas$location.start = with(casas, as.Date(location.start, format = "%Y-%m-%d"))
casas$location.end = with(casas, as.Date(location.end, format = "%Y-%m-%d"))
casas$provider.end = with(casas, as.Date(provider.end, format = "%Y-%m-%d"))

# create data frames  with entry and exit data 
casas.entry <- transform(casas, month = format(location.start,"%m"), year = format(location.start, "%Y"))
casas.exit <- transform(casas, month = format(location.end,"%m"), year = format(location.end, "%Y"))



# get the counts 
counts.entry <- ddply(casas.entry,.(Local.Authority, month,year), nrow)
counts.exit <- ddply(casas.exit,.(Local.Authority, month,year), nrow)


counts.exit = counts.exit %>% filter(!is.na(month)) # eliminate those that are NA and mean active 

# rename the variables in the counts 
counts.entry = rename(counts.entry, entry = V1, LA = Local.Authority)
counts.exit = rename(counts.exit, exit = V1, LA = Local.Authority)

# dataset with the counts of exits and entries 
# ----------------------------------------------
counts = merge(counts.entry, counts.exit, by= c("LA", "month", "year"), all = TRUE)                   

counts =  with(counts, counts[order(LA, year, month),]) #order the observation depending on the entry variable

write.csv(counts, "counts.csv") #counts of the exits and the entries

# recode variables and get missing into 0


counts$entry = with(counts, ifelse(is.na(entry), 0, entry))
counts$exit = with(counts, ifelse(is.na(exit), 0, exit))

counts  = counts %>% mutate(total = entry - exit) #total gets the net number of care homes 

# 2. Get the total number of care homes until march 2011
##########################################################
meses = c("01", "02")

inicio = counts %>% filter(  month %in% meses & year== "2011" | year == "2010" )

ex = ddply(inicio, ~ LA, summarize, homes = sum(total))
ex$month = factor("02")
ex$year = factor("2011")

prueba = inner_join(counts, ex, by.x= c("LA","month"), all = TRUE) # data frame for calculating entry rates                  

# get rid of observations for those years that are not necessary for the analysis 
# recode jan 2011 to make it the beginning of the period 

prueba<- data.frame(prueba %>% 
                group_by(LA) %>% 
                summarise(homes.1 = sum(homes, na.rm = T)) %>%
                inner_join(.,prueba, by = 'LA') %>% 
                mutate(homes.1 = ifelse(month == 1 & year == 2011, homes.1, NA)))

prueba = prueba %>%
  select(LA, month, year, entry, exit, total, homes.1 )

anos = c("2012", "2013", "2014", "2015", "2016") # years that I want to select 
test.1 = prueba %>% filter(year %in% anos | month != "2" & year == "2011") 




# homes contains the initial number of homes and the entry or exits per month
test.1$homes = with(test.1, ifelse(is.na(homes.1), total, homes.1))



require(data.table)
require(zoo)

test.tb = data.table(test.1)

setkey(test.tb, LA)

test.tb = test.tb[,homes.1:= cumsum(homes),by= LA]

prueba = test.tb %>% select(LA, month, year, entry, exit, total, homes.1)
prueba = as.data.frame(prueba)

write.csv(prueba, "prueba.csv")

#3. Get the entry rates --> entry rate = entries / total
#########################################################

t = prueba %>% mutate(entry_rate = (entry/homes.1)*100, exit_rate = (exit/homes.1)*100)

library(plyr)

t = plyr:: rename(t, c("homes.1" = "homes"))

write.csv(t, "entry_exit_rates.csv")
# ! Warning: the calculations have to be since march 2011 - I have to drop observations corresponding to jan 2011
# Calculations to proceed
# ........................

# t_analysis =  t %>% mutate(var =  paste(month, year, sep="_"))
# t_analysis$var = as.factor(t_analysis$va)
# t_analysis = t_analysis %>% filter(var != "1_2011")
# t = t_analysis %>% select(-var)
# .........................

# Note: Isles of Sicily and Unspecified are levels of LA that are not o








