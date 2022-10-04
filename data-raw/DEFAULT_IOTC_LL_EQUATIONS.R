## code to prepare `DEFAULT_IOTC_LL_EQUATIONS` dataset goes here

DEFAULT_IOTC_LL_EQUATIONS = update_equation(as.data.table(read.xlsx("./IOTC_DETERMINISTIC_EQUATIONS.xlsx", sheet = "L_L", rowNames = FALSE)))

usethis::use_data(DEFAULT_IOTC_LL_EQUATIONS, overwrite = TRUE)
