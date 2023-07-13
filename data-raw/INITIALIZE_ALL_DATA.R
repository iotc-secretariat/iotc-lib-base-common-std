library(iotc.base.common.data)

update_equation = function(equations_table) {
  equations_table[, EQ_ID := paste0("EQ_", EQ_ID)]

  return(equations_table)
}

source("./SIZE_BIN_SPLITTERS.R")
source("./DEFAULT_IOTC_WL_ND_KEYS.R")
source("./DEFAULT_IOTC_WL_EQUATIONS.R")
source("./DEFAULT_IOTC_LW_EQUATIONS.R")
source("./DEFAULT_IOTC_LL_EQUATIONS.R")
