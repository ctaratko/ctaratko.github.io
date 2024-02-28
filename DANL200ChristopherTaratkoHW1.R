library(tidyverse)
library(skimr)

# SUNY Geneseo, Fall 2023
# DANL 200: Introduction to Data Analytics
# Homework Assignment 1

# Instructor: Byeong-Hak Choe
# Student Name: Christopher Taratko
# Student ID:
# List of students whom you work with:
# 
#
# ... 



# Question 2. Working with starwars data.frame -----------------------------


# Q2a --------------------------------------------------------------

# Download the CSV file, starwars.csv, 
# from the Brightspace. 

# Read the data file, starwars.csv, 
# as the data.frame object with the name, 
# starwars, using (1) the read_csv() function and 
# (2) the absolute path name of the file starwars.csv
# from your local hard disk drive in your laptop.

# Answer for Q2a

starwars <- read.csv(
  "/Users/christophertaratko/Downloads/starwars.csv"
)




# Variable Description for starwars ---------------------------------------

# The following describes the variables 
# in the `starwars` data.frame.

# films: List of films the character appeared in
# name: Name of the character
# species: Name of species
# height: Height (cm)
# mass: Weight (kg)
# hair_color, skin_color, eye_color: Hair, skin, and eye colors
# birth_year: Year born (BBY = Before Battle of Yavin)
# sex: The biological sex of the character, 
# namely male, female, hermaphroditic, or none (as in the case for Droids).
# gender: The gender role or gender identity of the character 
# as determined by their personality or 
# the way they were programmed (as in the case for Droids).
# homeworld: Name of homeworld


# Q2b --------------------------------------------------------------
# Provide the R code to find how many characters are 
# `Droid` `species`.

# Answer for Q2b

droid <- starwars %>% filter(species == "Droid")
View(droid)


# Q2c --------------------------------------------------------------
# Provide the R code to find which `Droid` character did appear 
# most in the Star Wars `films`.

# Answer for Q2c

table(droid$name)
prop.table(table(droid$name))

starwars %>% count(films, sort = TRUE)
starwars %>% count(species, sort = TRUE)
starwars %>% count(films, .drop = FALSE)


# Q2d --------------------------------------------------------------
# Provide the R code to find the minimum, first quartile, 
# median, third quartile, and the maximum of `height`.

# [Statistics] Explain why the median value is different from 
# the mean value.
# When is the median value greater than the mean value?
# When is the median value less than the mean value?

# Answer for Q2d

summary(starwars$height)


# Q2e --------------------------------------------------------------
# Provide the R code to find the number of unique/distinct 
# `homeworld` values.

# Answer for Q2e

unique_homeworlds <- unique(starwars$homeworld)
num_unique_homeworlds <- length(unique_homeworlds)
cat("Number of unique homeworlds:", num_unique_homeworlds, "\n")



# Question 3. Data Visualization with gapminder ----------------------------


# Q3a ---------------------------------------------------------------------
# Provide the R code to install the R package, `gapminder`.

# Answer for Q3a

install.packages("gapminder")

# Q3b ---------------------------------------------------------------------
# Load the `gapminder` data.frame provided by the R package, 
# `gapminder`.

# Answer for Q3b

library(gapminder)
data(gapminder)
head(gapminder)


# Variable Description for gapminder data.frame ------------------------------

# The following describe the variables in the `gapminder` data.frame.

# `country`
# factor with 142 levels

# `continent`
# factor with 5 levels

# `year`
# ranges from 1952 to 2007 in increments of 5 years

# `lifeExp`
# life expectancy at birth, in years

# `pop`
# population

# `gdpPercap`
# GDP per capita (US$, inflation-adjusted)




# Q3c ---------------------------------------------------------------------

# Provide ggplot code and comments to describe 
# the relationship between `gdpPercap` and `lifeExp`.

# Answer for Q3c

library(ggplot2)
ggplot(data = gapminder) +
  geom_point(mapping = aes(x = gdpPercap, y = lifeExp)

# for pce. & pop. there's exponential growth (positive)
# and for the others, there's static growth/decay 
# for the most part except for the end
             

# Q3d ---------------------------------------------------------------------

# Provide ggplot code and comments to describe 
# how the relationship between `gdpPercap` and `lifeExp` 
# varies by `continent`.

# Answer for Q3d

library(gapminder)
library(ggplot2)
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, color = continent, alpha = .2))




# -----------------------------------------------------------------------
# This section is left intentionally blank.
