fs::dir_create(here::here("R", "RDS"))
fs::dir_create(here::here("R", "IMAGES"))

fs::dir_create(here::here("DATA", "SOURCE"))

data_path <- fs::path(here::here("DATA", "SOURCE"))
rds_path <- fs::path(here::here("R", "RDS"))
images_path <- fs::path(here::here("R", "IMAGES"))