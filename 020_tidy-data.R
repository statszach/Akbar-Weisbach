rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("002_libraries.R"))
source(here::here("005_folder-paths.R"))

# load data 

NHDS <- readRDS(here::here(rds_path, "010_NHDS.rds"))

## select for just last 5 years

tidy01 <- NHDS %>%
  dplyr::mutate(YEAR = first_2_digits_of_year*100 + year) %>% 
  dplyr::filter(between(YEAR, 2006, 2010))

## create DX variables

# Parkinson's = 332.0
# DVT = 453.40, 453.41, 453.42
# Pulmonary embolism = 415.19
# Dementia = 290.4 & 331.0

# Adverse event from anticoagulants = E934.2
# Colonic Obstruction 560, 560.0-560.9

tidy02 <- tidy01 %>% 
  dplyr::mutate(PD_DX = dplyr::if_else(dx1 == "3320-" | dx2 == "3320-" | dx3 ==  "3320-" |
                                       dx4 ==  "3320-" | dx5 ==  "3320-" | dx6 ==  "3320-" |
                                       dx7 ==  "3320-", 1, 0, missing = 0),
                Dementia_DX = dplyr::if_else(dx1 == "3904-" | dx1 == "3310-" |
                                             dx2 == "3904-" | dx2 == "3310-" |
                                             dx3 == "3904-" | dx3 == "3310-" |
                                             dx4 == "3904-" | dx4 == "3310-" |
                                             dx5 == "3904-" | dx5 == "3310-" |
                                             dx6 == "3904-" | dx6 == "3310-" |
                                             dx7 == "3904-" | dx7 == "3310-", 1, 0, missing = 0),
                DVT_DX = dplyr::if_else(dx1 == "45340" | dx1 == "45341" | dx1 == "45342" |
                                        dx2 == "45340" | dx2 == "45341" | dx2 == "45342" |
                                        dx3 == "45340" | dx3 == "45341" | dx3 == "45342" |
                                        dx4 == "45340" | dx4 == "45341" | dx4 == "45342" |
                                        dx5 == "45340" | dx5 == "45341" | dx5 == "45342" |
                                        dx6 == "45340" | dx6 == "45341" | dx6 == "45342" |
                                        dx7 == "45340" | dx7 == "45341" | dx7 == "45342", 1, 0, missing = 0),
                PE_DX = dplyr::if_else(dx1 == "41519" | dx2 == "41519" | dx3 ==  "41519" |
                                       dx4 == "41519" | dx5 == "41519" | dx6 ==  "41519" |
                                       dx7 == "41519", 1, 0, missing = 0),
                AEA = dplyr::if_else(dx1 == "E9342" | dx2 == "E9342" | dx3 ==  "E9342" |
                                       dx4 == "E9342" | dx5 == "E9342" | dx6 ==  "E9342" |
                                       dx7 == "E9342", 1, 0, missing = 0),
                CO_DX = dplyr::if_else(dx1 == "560--" | dx1 == "5600-" | dx1 == "5601-" | dx1 == "5602-" | dx1 == "5603-" | dx1 == "56030" | dx1 == "56031" | dx1 == "56032" | dx1 == "56039" | dx1 == "5608-" | dx1 == "56081" | dx1 == "56089" | dx1 == "5609-" |
                                       dx2 == "560--" | dx2 == "5600-" | dx2 == "5601-" | dx2 == "5602-" | dx2 == "5603-" | dx2 == "56030" | dx2 == "56031" | dx2 == "56032" | dx2 == "56039" | dx2 == "5608-" | dx2 == "56081" | dx2 == "56089" | dx2 == "5609-" |
                                       dx3 == "560--" | dx3 == "5600-" | dx3 == "5601-" | dx3 == "5602-" | dx3 == "5603-" | dx3 == "56030" | dx3 == "56031" | dx3 == "56032" | dx3 == "56039" | dx3 == "5608-" | dx3 == "56081" | dx3 == "56089" | dx3 == "5609-" |
                                       dx4 == "560--" | dx4 == "5600-" | dx4 == "5601-" | dx4 == "5602-" | dx4 == "5603-" | dx4 == "56030" | dx4 == "56031" | dx4 == "56032" | dx4 == "56039" | dx4 == "5608-" | dx4 == "56081" | dx4 == "56089" | dx4 == "5609-" |
                                       dx5 == "560--" | dx5 == "5600-" | dx5 == "5601-" | dx5 == "5602-" | dx5 == "5603-" | dx5 == "56030" | dx5 == "56031" | dx5 == "56032" | dx5 == "56039" | dx5 == "5608-" | dx5 == "56081" | dx5 == "56089" | dx5 == "5609-" |
                                       dx6 == "560--" | dx6 == "5600-" | dx6 == "5601-" | dx6 == "5602-" | dx6 == "5603-" | dx6 == "56030" | dx6 == "56031" | dx6 == "56032" | dx6 == "56039" | dx6 == "5608-" | dx6 == "56081" | dx6 == "56089" | dx6 == "5609-" |
                                       dx7 == "560--" | dx7 == "5600-" | dx7 == "5601-" | dx7 == "5602-" | dx7 == "5603-" | dx7 == "56030" | dx7 == "56031" | dx7 == "56032" | dx7 == "56039" | dx7 == "5608-" | dx7 == "56081" | dx7 == "56089" | dx7 == "5609-", 1, 0, missing = 0),
                FI_DX = dplyr::if_else( dx1 == "56032" |
                                        dx2 == "56032" |
                                        dx3 == "56032" |
                                        dx4 == "56032" |
                                        dx5 == "56032" |
                                        dx6 == "56032" |
                                        dx7 == "56032", 1, 0),
                II_DX = dplyr::if_else(dx1 == "5603-" | dx1 == "56030" |
                                       dx2 == "5603-" | dx2 == "56030" |
                                       dx3 == "5603-" | dx3 == "56030" |
                                       dx4 == "5603-" | dx4 == "56030" |
                                       dx5 == "5603-" | dx5 == "56030" |
                                       dx6 == "5603-" | dx6 == "56030" |
                                       dx7 == "5603-" | dx7 == "56030", 1, 0)
  )

# create demographics variables

tidy03 <- tidy02 %>% 
  dplyr::mutate(race_4cat = dplyr::case_when(race == 1 ~ 1,
                                             race == 2 ~ 2,
                                             race == 4 ~ 3,
                                             race == 5 ~ 5,
                                             TRUE ~ 4),
                age_4cat = dplyr::case_when(age < 60 ~ 1,
                                            between(age, 60, 69) ~ 2,
                                            between(age, 70, 79) ~ 3,
                                            age >= 80 ~ 4,
                                            TRUE ~ 0),
                age_2cat = dplyr::if_else(age < 75, 0, 1))

# Note: race_4cat 1 = white, 2 = black/aa, 3 = asian, 4 = other, 5 = not stated

# Filter out persons w dementia and just for persons with PD

tidyPD01 <- tidy03 %>% 
  dplyr::filter(Dementia_DX == 0)

# select for variables of interest, then aggregate

tidyPD02 <- tidyPD01 %>% 
  select(PD_DX, DVT_DX, PE_DX, AEA, race_4cat, age_2cat, YEAR, sex, analysis_weight, CO_DX, FI_DX, II_DX)

tidyPD_aggregate <- tidyPD02 %>% 
  group_by(PD_DX, DVT_DX, PE_DX, AEA, race_4cat, age_2cat, YEAR, sex, CO_DX, FI_DX, II_DX) %>% 
  summarise(meanweight = mean(analysis_weight)) %>% 
  ungroup()

tidyfull <- tidy03 %>% 
  select(PD_DX, DVT_DX, PE_DX, AEA, race_4cat, age_2cat, YEAR, sex, analysis_weight, CO_DX, FI_DX, II_DX) %>% 
  group_by(PD_DX, DVT_DX, PE_DX, AEA, race_4cat, age_2cat, YEAR, sex, CO_DX, FI_DX, II_DX) %>% 
  summarise(meanweight = mean(analysis_weight)) %>% 
  ungroup()

# Add labels for analysis/tables

tidyPD_aggregate$sex <- factor(tidyPD_aggregate$sex, levels = c(1, 2), labels = c("Male", "Female"))
tidyPD_aggregate$race_4cat <- factor(tidyPD_aggregate$race_4cat, levels = c(1, 2, 3, 4, 5),
                                  labels = c("White", "Black/African American",
                                             "Asian", "Other",  "Not Stated"))
tidyPD_aggregate$age_2cat <- factor(tidyPD_aggregate$age_2cat, levels = c(0, 1),
                               labels = c("Under 75", "75 or Older"))

tidyfull$sex <- factor(tidyfull$sex, levels = c(1, 2), labels = c("Male", "Female"))
tidyfull$race_4cat <- factor(tidyfull$race_4cat, levels = c(1, 2, 3, 4, 5),
                                     labels = c("White", "Black/African American",
                                                "Asian", "Other",  "Not Stated"))
tidyfull$age_2cat <- factor(tidyfull$age_2cat, levels = c(0, 1),
                                    labels = c("Under 75", "75 or Older"))

# create survey design

PD_cohort <- survey::svydesign(ids = ~1, fpc = NULL, strata = NULL, weights = ~meanweight,
                                  data = tidyPD_aggregate)

full_cohort <- survey::svydesign(ids = ~1, fpc = NULL, strata = NULL, weights = ~meanweight,
                               data = tidyfull)

## save surveydesign

saveRDS(PD_cohort, here::here(rds_path, "020_PD_cohort.rds"))
saveRDS(full_cohort, here::here(rds_path, "020_full_cohort.rds"))