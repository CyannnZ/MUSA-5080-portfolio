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
head(pa_income)
#Pennsylvania has 67 counties, so it has 67 rows and 5 columns.

pa_income$GEOID

as.numeric("01001")
#The "0" is missing. It's been changed into a number rather then a text.

filter(pa_income, estimate > 60000)

# Counties where the margin of error is bigger than 3000
filter(pa_income, moe > 3000)

# Counties where the estimate is under 50000
filter(pa_income, estimate < 50000)

select(pa_income, NAME, estimate, moe)
# Rows stayed the same, columns dropped.

# R: filter → Rows
# C: select → Columns

# Show only GEOID and estimate
select(pa_income, GEOID, estimate)

mutate(pa_income, moe_pct = moe / estimate * 100)
# rows stay same; columns +1, on the far end

# keep the new column
pa_income <- mutate(pa_income, moe_pct = moe / estimate * 100)
pa_income
# moe_pct is the margin of error as a percentage of the estimate.
# A larger moe_pct means the ACS estimate is relatively less precise.

arrange(pa_income,moe_pct)
arrange(pa_income,desc(moe_pct))
# 42023 Cameron County, Pennsylvania  B19013_001    47681  4326    9.07
# Same rectangle, different order. Nothing was added or removed

step1 <- filter(pa_income, moe_pct > 5)
step2 <- arrange(step1, desc(moe_pct))
step3 <- select(step2, NAME, estimate, moe, moe_pct)
step3

pa_income |>
  filter(moe_pct > 5) |>
  arrange(desc(moe_pct)) |>
  select(NAME, estimate, moe, moe_pct)
# take pa_income, and then keep the unreliable ones, and then sort worst first, and then show me these four columns.
# pipe goes at the end of a line, never the start of the next one.
# Shortcut: Ctrl+Shift+M (Cmd+Shift+M on Mac)

pa_income <- mutate(pa_income, reliable = moe_pct <5)

pa_income |>
  group_by(reliable) |>
  summarize(n=n(),
            avg_income = mean(estimate))
# 67 rows went in. Two came out — one per group.

pa_income <- pa_income |>
  mutate(reliability = case_when(
    moe_pct < 3 ~ "high confidence",
    moe_pct < 6 ~ "Moderate",
    TRUE        ~ "Low confidence"
  ))
count(pa_income,reliability)
# 1 Low confidence      7
# 2 Moderate           34
# 3 high confidence    26

pa_two <- get_acs(
  geography = "county",
  variables = c("B19013_001","B01003_001"),
  state = "PA", year = 2023, survey = "acs5"
)
pa_two
#  income and population are in different rows.

pa_wide <- get_acs(
  geography = "county",
  variables = c(income     = "B19013_001",
                population = "B01003_001"),
  state = "PA", year = 2023, survey = "acs5", output = "wide"
)
pa_wide
# In wide-format ACS data, E denotes the estimate and M denotes the margin of error (MOE) for each variable.

pa_wide |>
  mutate(moe_pct = incomeM / incomeE *100) |>
  arrange(desc(moe_pct)) |>
  select(NAME, populationE, incomeE, moe_pct) |>
  head(10)
# Counties with the highest relative margins of error for median household income tend to have smaller populations. This likely reflects smaller ACS sample sizes in less-populous counties, which produce less precise estimates.






