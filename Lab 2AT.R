# Lab 2: is this file ready to analyze?

##Forever Tungate

library(tidyverse)


# you're a junior analyst at a state library association. a colleague
# downloaded the 2024 Public Libraries Survey and wants to compare
# library systems on visits, circulation, and programs.

# your supervisor wants an audit before anyone analyzes anything.

# nothing today is new. you've done every piece of this already.


# grain and key

libraries <- read_csv(
  "PLS_FY24_AE_pud24i.csv",
  locale = locale(encoding = "latin1"),
  show_col_types = FALSE
)

glimpse(libraries)

# one row is one administrative entity/library system


# the download also has an "outlet" file. it has more rows.
# why would it? (the user's guide will tell you.)

# which column should identify a row?

## the column FSCKEY should identify a row(primary key).

# what did those tell you?

# what would it have meant if the count() came back with rows?

libraries |>
  count(FSCSKEY, sort = TRUE) |>
  filter(n>1)

##if count came back with rows it would have duplicates, but since it  did not
##it passed a key check

# let's focus on VISITS for now. what does this column mean?

##visits is total annual library visits

# VISITS counts visits to the library in a year.
# before you run anything: what values would be impossible?

## negative visits would be impossible as you cannot have a negative integer 
## right?

libraries |>
  summarise(
    min_visits = min(VISITS, na.rm = TRUE),
    max_visits = max(VISITS, na.rm = TRUE)
  )

libraries |>
  filter(VISITS < 0) |>
  count(VISITS)

# how many different negative values? how many rows of each?

## there are two different negative values, -3 and -1. -3 visits in 5 rows and
## -1 visits in 69 rows

# is a negative number here bad data, missing data, or a code?
# can you tell from the data alone?

## I believe it is a code, but you cant look at the data alone. In the
## documentation, it states: 

##data for the temporarily closed records are set to a value of -3
##-1 Missing data

# does R think any of these are missing?
## no, False for 9249 so none are missing.

is.na(libraries$VISITS) |>
  table()

# on tuesday, NA told us THAT something was missing but not WHY.
# what's different here?

##there are no NA therefore it cannot tell us something is missing.
## we had to find the why elsewhere.

# go to the user's guide. for each negative value:
# what does it mean, in the guide's words? where did you find it?

## I found it in the "IMLS 2024 PUBLIC LIBRARIES SURVEY DATA FILES -- 
## PUBLIC-USE FILES INSTRUCTIONS" on canvas and also in the 
## "Public Libraries Survey Fiscal Year 2024: Data File Documentation"
## under variable names and description  

# do the two codes mean the same thing?

##the codes mean, under descriptions:
##Total annual library visits
##-1–Missing
##-3–Temporarily closed administrative entity


# every numeric column has a flag column. find the one for VISITS.

## F_Visits

# what does the flag tell you? does it tell the two codes apart?

## the flag tells us information about the data, and we can tell the two codes
## apart.

##U_24 Not imputed (i.e., outlying area or temporarily closed)
##R_24 The data were reported and not imputed


# make a clean version. VISITS stays exactly as it is.

libraries <- libraries |>
  mutate(visits_clean = if_else(VISITS %in% c(-1, -3), NA_real_, VISITS))

# why list the codes instead of writing VISITS < 0?

##in case there are other errors we cannot see

# both codes just turned into NA. what did we lose?
# where can we still find it?

##we lose the reason why it was missing


# did it do what we meant? three questions:
##yes it turned the negative integers into missing integers
# same number of library systems?
## there is not something I am missing by taking care of -1 and -3
nrow(libraries)

# did every code become NA?

libraries %>%
  count(visits_clean, sort = TRUE)

# did anything else become NA?

##no

# does any of this matter?

## yea

# why is the raw mean lower? what's in each denominator?

##its lower because the negative values are pulling them down

# your turn :)
# get in your group project groups

# do that process for each of the following:

# TOTATTEN - program attendance

##Total attendance at synchronous programs
##-1–Missing
##-3–Temporarily closed administrative entity

libraries |>
  summarise(
    min_totatten = min(TOTATTEN, na.rm = TRUE),
    max_totatten = max(TOTATTEN, na.rm = TRUE)
  )

libraries |>
  filter(TOTATTEN < 0) |>
  count(TOTATTEN)

## -3 has five rows
## -1 has 401 rows

is.na(libraries$TOTATTEN) |>
  table()

libraries <- libraries |>
  mutate(TOTATTEN_clean = if_else(TOTATTEN %in% c(-1, -3), NA_real_, TOTATTEN))

libraries %>%
  count(TOTATTEN_clean, sort = TRUE)

## 406 NA
## 144 rows of zero


# TOTPRO - number of programs

##Total number of synchronous program sessions
##-1–Missing
##-3–Temporarily closed administrative entity

libraries |>
  summarise(
    min_totpro = min(TOTPRO, na.rm = TRUE),
    max_totpro = max(TOTPRO, na.rm = TRUE)
  )

libraries |>
  filter(TOTPRO < 0) |>
  count(TOTPRO)

## -3 has five rows
## -1 has 351 rows

is.na(libraries$TOTPRO) |>
  table()

libraries <- libraries |>
  mutate(TOTPRO_clean = if_else(TOTPRO %in% c(-1, -3), NA_real_, TOTPRO))

libraries %>%
  count(TOTPRO_clean, sort = TRUE)


# TOTCIR - total circulation

# you're not solving a new problem. same steps, different variable.
# everything you need is in the VISITS section.

# what should it measure? what would be impossible?

##TOTATTEN : should measure total attendance, negative values should be
## impossible if it isn't a code

##TOTPRO: Same as the others, negative values should be impossible

# range. any odd values? how many of each?

##TOTATTEN

## odd values are -1 and -3 again, Missing and Temporarily closed 
## administrative entity respectively
## -3 has five rows
## -1 has 401 rows

##TOTPRO

##Total number of synchronous program sessions
##-1–Missing
##-3–Temporarily closed administrative entity

##-3     5
##-1   354



# what does the user's guide say they mean?
# ("we couldn't find it" is an answer. "we assumed" isn't.)


# what's the flag column? (the names get shortened. look for it.)

## F_TOTATT

##F_TOTPRO


# clean version. keep the raw column.

# check it: same rows? every code became NA? nothing else did?

##just the negative rows

# mean before and after. big change or small?

mean(libraries$TOTATTEN) ##11387.35
mean(libraries$TOTATTEN_clean, na.rm = TRUE) ##11910.21


mean(libraries$TOTPRO) ##549.7965
mean(libraries$TOTPRO_clean, na.rm = TRUE) ##572.0402


##large difference, the clean version having a larger mean of attendance

# where are they? 

# recoding to NA fixes the number. does it finish the job? 
# are the coded rows spread out, or do they bunch up?
# if someone compares circulation across states, what goes wrong?


# this is for you to answer
# is this file ready to analyze as is? 3-4 sentences.

## as is, depends on what you are trying to do with it, but I believe you can
## there is information on everything in the file, and there are no missing
## confusing values. 
