# Strings and categorical representation

library(tidyverse)


pa <- read_delim(
  "fec_pa_2024_strings.psv",
  delim = "|",
  quote = "",
  na = character(),
  col_types = cols(.default = col_character())
)

# only contributions > $200 are included
# only three of the original columsn are there. record_id is mine, not the FEC's
# helps to protect anonymity. 
##GRAIN: one row is a contribution record that is greater than 200 in 2024


glimpse(pa)

nrow(pa)
n_distinct(pa$record_id)

# What does one row represent? Is one row one contributor?
##one row is a contribution record that is >$200 in 2024

# Which variable are we interested in today?
##Empoyer

# Our task: who employs the people giving money to campaigns?

pa |> 
  count(employer, sort = TRUE) |> 
  print(n = 25)

# Same question as the vehicles. Is this counting employers?

# Look for ways someone could say they are self employed. 
# What about unemployed? 

##SELF-Employed
##SELF
##Self Employed
## there is a lot of variation between these items but they have the same
## meanings.
##    Normalization
## 1. All Caps
## 2. Replace dashes or spaces or both (underscore)
## 3. Drop extra Spaces -> inclusive of trailing and leading spaces

n_distinct(pa$employer)

# Does that mean there are 6,950 different employers?

# Normalizing representation
examp_text <- "Hello, this  is an example. " ##Double spaces

str_to_upper(examp_text) ## make it all caps (or lower w str_to_lower)
str_trim(examp_text) ## drops trailing space
str_squish(examp_text) ## double space anywhere

# let's create employer_normalized, where we make every observation in all caps
# and we remove traiing and leading white spaces. 

pa_clean <- pa |> 
  mutate(
    employer_normalized = employer |>
      str_to_upper() |>
      str_squish()
    )

n_distinct(pa$employer)
n_distinct(pa_clean$employer_normalized)
# That barely changed. Why not?
# What does that tell you about how this data was recorded?


# What's actually inconsistent?
pa |> 
  filter(
    employer %in% c(
      "NOT EMPLOYED", "NOT-EMPLOYED",
      "SELF EMPLOYED", "SELF-EMPLOYED", "SELF",
      "ELECTRICIANS LOCAL 98", "ELECTRICIANS LOCAL98",
      "HIGHMARK INC", "HIGHMARK, INC.", "HIGHMARK HEALTH"
    )
  ) |> 
  count(employer, sort = TRUE)

# Which of these differences are just representation?
# Which might actually mean different things?
# Which would you change without outside information?

# Punctuation 
## let's get rid of those hyphenations now. 
# we can use str_replace_all(fixed()) to do this. 
# should we use str_squish again?
pa_clean <- pa_clean |>
  mutate(
    employer_normalized_punct = employer_normalized |>
      str_replace_all(fixed("-"), " ") |>
      str_squish()
  )

pa_clean <- pa_clean |> 
  mutate(
    employer_no_hyphen = employer_normalized |> 
      str_replace_all(fixed("-"), " ") |> 
      str_squish()
  )

pa_clean |> 
  filter(
    employer %in% c(
      "NOT EMPLOYED", "NOT-EMPLOYED",
      "SELF EMPLOYED", "SELF-EMPLOYED", "SELF"
    )
  ) |> 
  count(employer_normalized_punct, sort = TRUE)

# What collapsed? What didn't?
##everything but self into self employed

# SELF looks like it belongs with SELF EMPLOYED. Does it?
# What if Self is a company? An abbreviation for something else?

# Removing a hyphen changed how two words were written.
# Merging SELF would be a claim about who these people are.


# Exact replacement
# When you know exactly which value you want to change, change that
# value and nothing else.

# Here's where we can use case_when()

pa_clean <- pa_clean |> 
  mutate(
    employer_normalized_punct = case_when(
      employer_normalized == "NOT-EMPLOYED" ~ "NOT EMPLOYED",
      employer_normalized == "SELF-EMPLOYED" ~ "SELF EMPLOYED",
      employer_normalized == "ELECTRICIANS LOCAL98" ~ "ELECTRICIANS LOCAL 98",
      employer_normalized == "HIGHMARK, INC." ~ "HIGHMARK INC",
      TRUE ~ employer_normalized_punct
    )
  )

# Notice this matches on employer_normalized, not employer_no_hyphen.
# If you had already stripped punctuation, would
# "HIGHMARK, INC." still be there for case_when to find?
## Yes because we didnt clean commas, just hyphens

# Why keep raw column?
pa_clean |> 
  filter(employer != employer_normalized_punct) |> 
  count(employer, employer_normalized_punct, sort = TRUE) |> 
  print(n = 61)

# I wrote four replacement rules. Why did more rows than that change?

# Find ORRICK HERRINGTON  SUTCLIFFE LLP. Why is there a double space
# in the middle of a law firm's name? What did str_squish() do to it?

pa_clean |> 
  select(employer, employer_normalized, employer_normalized_punct) |> 
  distinct() |> 
  arrange(employer_normalized_punct) |> 
  print(n = 30)

# What would we lose if we overwrote employer?


# Weekly 2

coffee <- read_csv("data/coffee_ratings.csv")

coffee |> 
  count(producer, sort = TRUE)

n_distinct(coffee$producer)

# Tuesday: Regular expression 

# Today I told you which values to look at. What if I hadn't?
# Would you read all 6,950?

# Tuesday: how do you find suspicious or related values without
# reading every one?