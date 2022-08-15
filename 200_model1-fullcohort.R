rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("002_libraries.R"))
source(here::here("005_folder-paths.R"))

# load data 

full_cohort <- readRDS(here::here(rds_path, "020_full_cohort.rds"))

# table 1

table1_full <- full_cohort %>% 
  gtsummary::tbl_svysummary(by = PD_DX) %>% 
  gtsummary::add_overall() %>% 
  gtsummary::add_p()

# model 1 - PD and DVT

m1 <- svyglm(DVT_DX ~ PD_DX + age_2cat + as.factor(race_4cat) + sex + as.numeric(YEAR), 
             design = full_cohort,
             family = quasibinomial(link = "logit"))

summary(m1)

model1table_full <- m1 %>% 
  gtsummary::tbl_regression()

# model 2 - PD and PE

m2 <- svyglm(PE_DX ~ PD_DX + age_2cat + as.factor(race_4cat) + sex + as.numeric(YEAR), 
             design = full_cohort,
             family = quasibinomial(link = "logit"))

model2table_full <- m2 %>% 
  gtsummary::tbl_regression()  

# model 3 - AEA and PD

m3 <- svyglm(AEA ~ PD_DX + age_2cat + as.factor(race_4cat) + sex + as.numeric(YEAR), 
             design = full_cohort,
             family = quasibinomial(link = "logit"))

summary(m3)

model3table_full <- m3 %>% 
  gtsummary::tbl_regression()

## save tables

saveRDS(table1_full, here::here(rds_path, "200_table1_full.rds"))
saveRDS(model1table_full, here::here(rds_path, "200_model1table_full.rds"))
saveRDS(model2table_full, here::here(rds_path, "200_model2table_full.rds"))
saveRDS(model3table_full, here::here(rds_path, "200_model3table_full.rds"))
