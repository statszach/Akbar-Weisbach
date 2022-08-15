rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("002_libraries.R"))
source(here::here("005_folder-paths.R"))

# load data 

PD_cohort <- readRDS(here::here(rds_path, "020_PD_cohort.rds"))

# table 1

table1_PD <- PD_cohort %>% 
  gtsummary::tbl_svysummary(by = PD_DX) %>% 
  gtsummary::add_overall() %>% 
  gtsummary::add_p()

# model 1 - PD and DVT

m1 <- svyglm(DVT_DX ~ PD_DX + age_2cat + as.factor(race_4cat) + sex + as.numeric(YEAR), 
             design = PD_cohort,
             family = quasibinomial(link = "logit"))

summary(m1)

model1table_PD <- m1 %>% 
  gtsummary::tbl_regression()

# model 2 - PD and PE

m2 <- svyglm(PE_DX ~ PD_DX + age_2cat + as.factor(race_4cat) + sex + as.numeric(YEAR), 
             design = PD_cohort,
             family = quasibinomial(link = "logit"))

model2table_PD <- m2 %>% 
  gtsummary::tbl_regression()  

# model 3 - AEA and PD

m3 <- svyglm(AEA ~ PD_DX + age_2cat + as.factor(race_4cat) + sex + as.numeric(YEAR), 
             design = PD_cohort,
             family = quasibinomial(link = "logit"))

summary(m3)

model3table_PD <- m3 %>% 
  gtsummary::tbl_regression()

## save tables

saveRDS(table1_PD, here::here(rds_path, "100_table1_PD.rds"))
saveRDS(model1table_PD, here::here(rds_path, "100_model1table_PD.rds"))
saveRDS(model2table_PD, here::here(rds_path, "100_model2table_PD.rds"))
saveRDS(model3table_PD, here::here(rds_path, "100_model3table_PD.rds"))

