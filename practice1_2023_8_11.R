# load library
library(dplyr)
library(ggpubr)
library(ggplot2)
library(RColorBrewer) 
library(reactable)
library(reactablefmtr)
library(htmltools)
library(fontawesome)
library(DT)

# read data
starwars <- read.csv("starwars.csv")
head(starwars)
dplyr::glimpse(starwars)
