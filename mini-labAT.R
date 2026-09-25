# Lab 2

# We'll start by working with the actual crashes and persons fully 
library(tidyverse)

crashes <- read_csv("crashes.csv")
persons <- read_csv("person.csv")

female_pedestrians  <- persons |>
  filter(PERSON_SEX == "F")
female_Only_pedestrians <- persons |>
  filter(PERSON_SEX == "F", PERSON_TYPE == "Pedestrian")
# 1: Make a table of person records for female pedestrians 

persons_female <- crashes |>
  semi_join(female_Only_pedestrians, join_by(COLLISION_ID)) |>
  select(BOROUGH,COLLISION_ID, starts_with("VEHICLE TYPE CODE"))


## what does one row represent?
###collisions with women pedestrians, and where they where

# 2: Keep only the crashes that involved at least one female pedestrian. 
# Keep COLLISION_ID, BOROUGH, and the five vehicle type columns. 
# how can we select every variable that starts with "VEHICLE TYPE CODE"?

# does one row still represent one crash? check it. 
nrow(persons_female)

# why a filtering join instead of a mutating join? 

# 3: Right now the vehicle types are columns. We want one row per vehicle. 
# before writing your code, how many rows should we have? 
##4940


# how many missing values are in the new dataframe? 
91316-4940
##863376 values excluded from previous data frame

# why do we think that slots 3, 4, and 5 have so many more missing values? 

## 3,4,5 vehicles in a crash become less common

# does every crash have a first vehicle recorded?

## only 12% does not

# are we safe to drop missing values?

## missing values just mean a car was not present in that slot, so yea

# 5: what kinds of vehicles are involved in crashes with a female pedestrian? 

persons_female |> 
  count(`VEHICLE TYPE CODE 1`,`VEHICLE TYPE CODE 2`, sort = TRUE) |> 
  print(n = 40)

table(persons_female$BOROUGH) 
table(persons_female$`VEHICLE TYPE CODE 1`)
##Mainly Sedans
