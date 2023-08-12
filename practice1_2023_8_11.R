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
table(starwars$species)

dplyr::glimpse(starwars)
summary(starwars)
colnames(starwars)
unique(starwars$species)
table(starwars$species)

# select data
starwars[c(1:4), c('name', 'homeworld')]
starwars[c(1:10), c('name', 'homeworld')]

starwars[c(1:4), c(1, 9)]
starwars[c(1:4), c(1:9)]

starwars %>% slice(1:4) %>% dplyr::select(name, homeworld)
starwars %>% slice(1:4) %>% dplyr::select(1, 9)

starwars %>% slice(1:4) %>% dplyr::select(-name)
starwars %>% slice(1:4) %>% dplyr::select(-1)

starwars %>% slice(1:4) %>% dplyr::select(contains("_"))
starwars %>% slice(1:4) %>% dplyr::select(starts_with("s"))

# filter data
starwars %>% filter(mass > mean(mass, na.rm = TRUE)) %>% dplyr::select(name,mass, homeworld)
filter(starwars, species == "Human") %>% dplyr::select(name, species, homeworld)
filter(starwars, hair_color == "none" & eye_color == "black") %>% dplyr::select(name, hair_color,homeworld, eye_color )
starwars %>% filter(mass >1000 & height > 150) %>% dplyr::select(name, hair_color,homeworld, eye_color )
starwars %>% group_by(gender) %>% filter(mass > mean(mass, na.rm = TRUE)) %>% dplyr::select(gender, mass, name, hair_color, eye_color )
starwars %>% filter(mass > mean(mass, na.rm = TRUE)) %>% dplyr::select(gender, mass, name, hair_color, eye_color )
starwars %>% group_by(gender)

# create new column
starwars %>%
  select(name, mass, species) %>%
  mutate(mass_norm = mass / mean(mass, na.rm = TRUE))

starwars %>%
  select(name, height, mass, homeworld, species) %>%
  group_by(species) %>%
  mutate(mass_norm = mass / mean(mass, na.rm = TRUE))

starwars %>%
  select(name, height, mass, homeworld) %>%
  mutate(
    mass2 = mass * 2,
    mass2_squared = mass2 * mass2
  )

starwars %>%
  select(name, height, mass, homeworld) %>%
  mutate(
    mass = NULL,
    height = height * 0.0328084 # convert to feet
  )

starwars %>%
  select(name, homeworld, species) %>%
  mutate(across(!name, as.factor))
