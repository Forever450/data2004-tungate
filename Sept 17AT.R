library(tidyverse)
install.packages("tidyverse")

crashes <- read_csv("data/raw/crashes.csv")
persons <- read_csv("data/raw/person.csv")

glimpse(crashes)
glimpse(persons)

# What does one row represent in each table?

## One row represents a different collision

# What variable appears in both? Does it do the same job in both?

## collision_ID

crashes_core <- crashes |> 
  select(
    COLLISION_ID,
    `CRASH DATE`,
    BOROUGH,
    `NUMBER OF PERSONS INJURED`,
    `NUMBER OF PERSONS KILLED`
  )

persons_core <- persons |> 
  select(
    UNIQUE_ID,
    COLLISION_ID,
    PERSON_TYPE,
    PERSON_INJURY,
    PERSON_AGE,
    PERSON_SEX
  )

# You work for the NYC Department of Transportation and you have been given a media request: 
# How do person-level injury outcomes compare across boroughs? 

# Can you answer this with one dataframe alone? Probably not, or else we wouldn't do this on 
# our join day :)

# So we have to put these together. To do this, we have to work with keys. 

# A PRIMARY KEY uniquely identifies a row in its own table.
# A FOREIGN KEY points at another table's primary key.

# How should we figure out which one COLLISION_ID is?

## One way to do that is to count() the primary keys and look
## for entries where n is greater than one.

crashes_core |> 
  count(COLLISION_ID) |>
  filter(n>1)

persons_core |>
  count(COLLISION_ID) |>
  filter(n>1)

# Why does COLLISION_ID repeat in the person table?
## idk :(
## the amount of people in each collision !! there can be more than
## one person to a collision :)

# Could we try to learn something about that wreck with 52 people in it? Let's use 
## the persons df

##BUS

# Do we have any missing keys?
crashes_core |>
  anti_join(persons_core, by = "COLLISION_ID") |>
  count("COLLISION_ID", sort = TRUE)
##No, because if we did, this tibble would have <NA> values and it doesn't.

# Cardinality

# Cardinality is how many rows on each side can share a key value. 
## one-to-one: each key appears once in both
## one-to-many: unique on the left, repeats on the right
## many-to-many: repeats on both sides

# Which one do we have? What does that tell you about what a join will
# do to our rows?

##one-to-many

nrow(crashes_core)
nrow(persons_core)
n_distinct(persons_core$COLLISION_ID)

# Two of those numbers are close but not equal. What does that difference
# tell you before we join anything?

##the number of people in a crash vs the number of crashes are not equal

# Let's do our join. What should we join by? What's your guess for the number of rows? 

# We'll start by doing a join that keeps observations where both cases exist. 
# Can anyone remember which join this is? 

# Use the console if you don't remember. 

## inner - joins matches across both - drops the ones that dont match
## left - joins matches, but does not drop the lack of matches

crashes_inner <- crashes_core |>
  inner_join(persons_core, join_by(COLLISION_ID)) ## Rows: 317,941

glimpse(crashes_inner)
## for crashes inner, what is our grain?
# one row represents one crash record with person-level crash information.

##wouldnt be incorrect to do this either, but it is different

persons_inner <-  persons_core |> 
  inner_join(crashes_core, join_by(COLLISION_ID))


# one row represents one person record with crash-level information


# What does one row represent now? Is that the same as before? ^^

# Now the other one. inner_join keeps rows that matched in BOTH tables.
# Which one keeps every row of the LEFT table whether it matched or not.

crashes_left <- crashes_core |>
  left_join(persons_core, join_by(COLLISION_ID))

glimpse(crashes_left)

nrow(crashes_inner) #317,941
nrow(crashes_left) #318,319, left kept all of the rows, even the unmatched rows
## We know that there are some records that do not have person records involved.
## we might have wrecks not involving people, such as self-driving vehicles or
## vehicles being left unattended.


# Where did the difference come from? ^^

# What is one row in crash_people_inner? What is one row in
# crash_people_left? Are they the same question?

nrow(crashes_left)
sum(!is.na(crashes_left$UNIQUE_ID))

# If someone asked how many people were involved in crashes, which of
# those two numbers would you hand them?

## left join

# When would you want inner_join? When would you want left_join?

## It depends on the comparison. 
### Which depends on the question you are asking.

# We said this was one-to-many. We can say that in the code and make R
# check it for us.

crashes_core |> 
  inner_join(persons_core, join_by(COLLISION_ID),
             relationship = "one-to-many")

crashes_core |> 
  inner_join(persons_core, join_by(COLLISION_ID),
             relationship = "one-to-one")

# What happened on the second one? Why would you want that?

# COVERAGE is which rows on each side found a match. The table got
# bigger, so nothing was lost, right?

crashes_core |> 
  anti_join(persons_core, join_by(COLLISION_ID)) |> 
  nrow()
### crashes without people

persons_core |> 
  anti_join(crashes_core, join_by(COLLISION_ID)) |> 
  nrow()
## people without crashes (NONE)

# anti_join keeps left rows with NO match and adds no columns. Does the
# order matter here? Are those two lines asking the same question?

# What kind of crash has no person records?

# We can make R shout about this one too.

persons_core |> 
  left_join(crashes_core, join_by(COLLISION_ID), unmatched = "error")

# What does that argument buy you?

# The four mutating joins:
# inner_join = rows that matched in both
# left_join = every row in the LEFT table, matched or not
# right_join = every row in the right table
# full_join = everything from both

# Given all that, which one do you want for person-level outcomes with
# borough attached? Which table goes on the left?

##### Your turn #####

# Build a person-level table that includes borough.



# Before you write anything:
## What should one row represent when you're done?
## Which table goes on the left?

# Then:
## Join them.
## Declare the cardinality with relationship = and see if R agrees.
## Use anti_join() to look at whatever didn't match.
## Count records by BOROUGH, PERSON_TYPE, and PERSON_INJURY.

# Some of those rows will have no borough. Before you filter them out:
# how many are there, and are they all missing for the same reason?

person_crashes <- persons_core |> 
  left_join(crashes_core, join_by(COLLISION_ID))

nrow(persons_core)

nrow(person_crashes)

persons_core |> 
  anti_join(crashes_core, join_by(COLLISION_ID))

## A tibble: 0 × 6
# ℹ 6 variables: UNIQUE_ID <dbl>, COLLISION_ID <dbl>, PERSON_TYPE <chr>,
#   PERSON_INJURY <chr>, PERSON_AGE <dbl>, PERSON_SEX <chr>