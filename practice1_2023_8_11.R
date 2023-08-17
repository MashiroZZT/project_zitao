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

# create new column (variable)
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
    height = height * 0.0328084
  )

starwars %>%
  select(name, homeworld, species) %>%
  mutate(across(!name, as.factor))

starwars %>%
  select(name, homeworld, species)

# scatter plot
p <- ggplot(data = mpg, aes(x = displ, y = hwy, colour = class))  + 
  geom_point(size = 2)
p

p <- ggplot(data = mpg, aes(x = displ, y = hwy, colour = class))  + 
  geom_point(size = 2) +
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.3) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  labs(x = "Display", y = "Highway", fill = "Class", color = "Class")
p

p <- ggplot(mpg, aes(displ, hwy, colour = class))  + 
  geom_point(size = 3) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  facet_grid(vars(cyl), scales = "free")
p

# bar plot
p <- ggplot(data = mpg, aes(x = manufacturer,fill = as.character(cyl)))   + 
  geom_bar(position = "stack") + 
  scale_fill_brewer(palette = "BuPu")
p

p <- ggplot(data = mpg, aes(x = manufacturer,fill = as.character(cyl)))  + 
  geom_bar(position = "fill") 
p

# Load data
data("mtcars")
dfm <- mtcars
# Convert the cyl variable to a factor
dfm$cyl <- as.factor(dfm$cyl)
# Add the name colums
dfm$name <- rownames(dfm)

ggbarplot(dfm, x = "name", y = "mpg",
          fill = "cyl",               # change fill color by cyl
          color = "white",            # Set bar border colors to white
          palette = "jco",            # jco journal color palett. see ?ggpar
          sort.val = "desc",          # Sort the value in dscending order
          sort.by.groups = FALSE,     # Don't sort inside each group
          x.text.angle = 90           # Rotate vertically x axis texts
)

ggbarplot(dfm, x = "name", y = "mpg",
          fill = "cyl",               # change fill color by cyl
          color = "white",            # Set bar border colors to white
          palette = "jco",            # jco journal color palett. see ?ggpar
          sort.val = "asc",           # Sort the value in dscending order
          sort.by.groups = TRUE,      # Sort inside each group
          x.text.angle = 90           # Rotate vertically x axis texts
)

# density plot
# Create some data format
set.seed(1234)
wdata = data.frame(
  sex = factor(rep(c("F", "M"), each=200)),
  weight = c(rnorm(200, 55), rnorm(200, 58)))
head(wdata, 4)

ggdensity(data = wdata, x = "weight",
          add = "mean", rug = TRUE,
          color = "sex", fill = "sex",
          palette = c("#00AFBB", "#E7B800"))

# histogram plot
gghistogram(data = wdata, x = "weight",
            add = "mean", rug = TRUE,
            color = "sex", fill = "sex",
            palette = c("#00AFBB", "#E7B800"),
            bins = 30)

# box plot
# Load data
data("ToothGrowth")
df <- ToothGrowth
head(df, 4)

p <- ggboxplot(df, x = "dose", y = "len",
               color = "dose", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
               add = "jitter", shape = "dose")
p

my_comparisons <- list( c("0.5", "1"), c("1", "2"), c("0.5", "2") )
p + stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 50) 

# violin plot
ggviolin(df, x = "dose", y = "len", fill = "dose",
         palette = c("#00AFBB", "#E7B800", "#FC4E07"),
         add = "boxplot", add.params = list(fill = "white"))+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+ # Add significance levels
  stat_compare_means(label.y = 50)  

# deviation graph
# Calculate the z-score of the mpg data
dfm$mpg_z <- (dfm$mpg -mean(dfm$mpg))/sd(dfm$mpg)
dfm$mpg_grp <- factor(ifelse(dfm$mpg_z < 0, "low", "high"), 
                      levels = c("low", "high"))
# Inspect the data
head(dfm[, c("name", "wt", "mpg", "mpg_z", "mpg_grp", "cyl")])

ggbarplot(dfm, x = "name", y = "mpg_z",
          fill = "mpg_grp",           # change fill color by mpg_level
          color = "white",            # Set bar border colors to white
          palette = "jco",            # jco journal color palett. see ?ggpar
          sort.val = "asc",           # Sort the value in ascending order
          sort.by.groups = FALSE,     # Don't sort inside each group
          x.text.angle = 90,          # Rotate vertically x axis texts
          ylab = "MPG z-score",
          xlab = FALSE,
          legend.title = "MPG Group"
)

ggbarplot(dfm, x = "name", y = "mpg_z",
          fill = "mpg_grp",           # change fill color by mpg_level
          color = "white",            # Set bar border colors to white
          palette = "jco",            # jco journal color palett. see ?ggpar
          sort.val = "desc",          # Sort the value in descending order
          sort.by.groups = FALSE,     # Don't sort inside each group
          x.text.angle = 90,          # Rotate vertically x axis texts
          ylab = "MPG z-score",
          legend.title = "MPG Group",
          rotate = TRUE,
          ggtheme = theme_minimal()
)

# dot chart
ggdotchart(dfm, x = "name", y = "mpg",
           color = "cyl",                                # Color by groups
           palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
           sorting = "ascending",                        # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           ggtheme = theme_pubr()                        # ggplot2 theme
)

ggdotchart(dfm, x = "name", y = "mpg",
           color = "cyl",                                # Color by groups
           palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           rotate = TRUE,                                # Rotate vertically
           group = "cyl",                                # Order by groups
           dot.size = 6,                                 # Large dot size
           label = round(dfm$mpg),                        # Add mpg values as dot labels
           font.label = list(color = "white", size = 9, 
                             vjust = 0.5),               # Adjust label parameters
           ggtheme = theme_pubr()                        # ggplot2 theme
)

ggdotchart(dfm, x = "name", y = "mpg_z",
           color = "cyl",                                # Color by groups
           palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           add.params = list(color = "lightgray", size = 2), # Change segment color and size
           group = "cyl",                                # Order by groups
           dot.size = 6,                                 # Large dot size
           label = round(dfm$mpg_z,1),                        # Add mpg values as dot labels
           font.label = list(color = "white", size = 9, 
                             vjust = 0.5),               # Adjust label parameters
           ggtheme = theme_pubr()                        # ggplot2 theme
)+
  geom_hline(yintercept = 0, linetype = 2, color = "lightgray")

ggdotchart(dfm, x = "name", y = "mpg",
           color = "cyl",                                # Color by groups
           palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           rotate = TRUE,                                # Rotate vertically
           dot.size = 2,                                 # Large dot size
           y.text.col = TRUE,                            # Color y text by groups
           ggtheme = theme_pubr()                        # ggplot2 theme
)+
  theme_cleveland()        # Add dashed grids

# RColorBrewer
display.brewer.all(colorblindFriendly = TRUE)  
