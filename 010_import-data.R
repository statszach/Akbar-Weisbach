NHDS <- haven::read_dta(fs::path(data_path, "NHDS.dta"))

saveRDS(NHDS, here::here(rds_path, "010_NHDS.rds"))