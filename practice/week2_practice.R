library(tidyverse)
library(tidycensus)

#I'm going to load some data
pa_income <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "PA",
  year =2023,
  survey = "acs5"
)

pa_income

dim(pa_income)
glimpse(pa_income)
head(pa_income,10)

pa_income$GEOID
as.numeric("01001")

filter(pa_income, estimate > 60000)

select(pa_income, NAME, estimate, moe)
