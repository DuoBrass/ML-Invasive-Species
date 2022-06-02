#Invasive Speices Tetrad
rm(list = ls())
library("xml2")
library("rvest")
library(caret)
library(rpart.plot)
library(rpart)
library(tidyverse)
library(lattice)
library(rpart)
library(caTools)
library(ranger)
library(MASS)
library(knn.covertree)
library(class)
library(e1071)
library(tidyverse)
library(sf)
library(raster)
library(dplyr)
library(mlr)
library(parallelMap)
library(tidyverse)

get_plant_tetrad_presence = function(tetrad="SP4298", minyear=2000, maxyear=tail(unlist(strsplit(date(), " ")),1)){
  

  
  # make the URL to list records from minyear to the current year
  url = paste0("https://database.bsbi.org/reports/sitetaxa.php?gridref=",
               tetrad, "&minfreq=0&minyear=", minyear, "&maxyear=",
               maxyear, "&vc=") 
  
  x = read_html(url)
  x2 = html_nodes(x, css=".obscurelink .siterep_previous")
  speciesName = html_text(x2, trim=TRUE)
  
  x4 = html_nodes(x, css="section")
  x4 = html_text(x4, trim=TRUE)
  for(s in speciesName) x4 = gsub(s, "", x4, fixed=TRUE)
  x4 = suppressWarnings(as.numeric(strsplit(x4, " ")[[1]])) # split by space and convert to number (text --> NA)
  x4 = x4[!is.na(x4)]
  numRecords = matrix(x4, ncol=2, byrow=TRUE)[,1]
  
  data.frame(speciesName, numRecords)
  
}

# get species list and their number of records for a tetrad
spp = get_plant_tetrad_presence(tetrad="SP4298", minyear=1990)

# urban area in the south
#spp = get_plant_tetrad_presence(tetrad="NO09P", minyear=1990) # remote highland area
head(spp)

# get list of all nonnative plant species in BSBI database
library(readxl)
#Native/non-native
all_spp = read_xlsx("ddbtaxon-list.xlsx") # downloaded from https://bsbi.org/taxon-lists
table(all_spp$`national status`, useNA="always")
nonnative_spp = all_spp[grep("alien", all_spp$`national status`),]

# add nonnative status to the species list
spp$nonnative = spp$speciesName %in% nonnative_spp$`taxon name`
table(spp$nonnative)

nrow(spp) # number of all species
sum(spp$numRecords) # number of records
sum(spp$nonnative) # number of non-native species recorded
mean(spp$nonnative) # proportion of all species that are nonnative
sum(spp$numRecords[spp$nonnative]) # number of records of non-native species
sum(spp$numRecords[spp$nonnative])/sum(spp$numRecords) # proportion of all records from non-native species
str(spp)
spp

spp <- spp[spp$nonnative == 'TRUE',]

spp$Dinty1 <- c("SP4298") 



spp

write.csv(spp,"SP4298.csv", row.names = FALSE) 

#Plotting the data for invasive species 100 datapoits over England####

df_Inv <- read.csv("Coords.csv")
df_Inv
#Invasive Species dataset

df_Inv %>% 
  drop_na()

ggplot() +
  geom_raster(data = df_Inv, aes(x = x, y = y, fill = InvTotal)) +
  scale_fill_viridis_c() +
  coord_quickmap() 

Map <- ggplot(df_Inv, aes(x, y, fill = InvTotal)) + geom_point() +
  coord_equal() + labs(x = "Easting (m)", y = "Northing (m)", fill = "Number Of Invasive Species.")

Map
