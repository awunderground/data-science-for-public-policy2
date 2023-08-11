library(tidyverse)
library(haven)
library(here)

# this cleaning is based on Stata scripts from the original Authors' code
combined <- read_stata(here("data-raw", "seedanalysis_011204_080404.dta"))

combined <- combined  |>
  # turn dollars into hundreds of dollars
  mutate(
    totbal = totbal / 100,
    newtotbal = newtotbal / 100
  ) |>
  # fix population variable
  mutate(pop = as.numeric(str_replace(pop, ",", ""))) |>
  # create bank penetration variable|>
  mutate(brgy_penetration = no_clients / pop)

combined <- combined |>
  # calculate the mean and sd for each bank
  # the Stata code is a little cryptic
  group_by(brgy_penetration) |>
  mutate(sd_totbal = sd(totbal),
         mean_totbal = mean(totbal)) |>
  ungroup()

combined <- combined |>
  # create control dummy
  mutate(control = as.numeric(group == "C")) |>
  # create distance to bank variable
  mutate(dist_GB = 
           case_when(
             butuan == 1 ~ dbutuan,
             ampayon == 1 ~ dampayon
           )
  )

# create a set with the 1777 observations that are in the SEED, 
# marketing treatment, or control group
treated <- combined |>
  filter(!is.na(treatment))

treated_subset <- treated |>
  mutate(
    group3 = case_when(
      treatment == 0 & marketing == 0 ~ "control",
      treatment == 0 & marketing == 1 ~ "marketing",
      treatment == 1 & marketing == 0 ~ "treatment"
    )
  ) |>
  mutate(group2 = if_else(group3 == "treatment", "treatment", "control")) |>
  select(group2, group3, totbal, newtotbal, balchange)

write_csv(treated_subset, here("data", "seed.csv"))
