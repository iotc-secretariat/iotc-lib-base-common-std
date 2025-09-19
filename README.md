# README

## What is this repository for?

This repository hosts the scripts and files for generating a R library that provides functions for morphometric conversions for the IOTC species and pelagic sharks caught in tuna and tuna-like fisheries.

## Contents

-  The source parameters for the conversions are included in the XLSX spreadsheet [IOTC_DETERMINISTIC_EQUATIONS.xlsx](./data-raw/IOTC_DETERMINISTIC_EQUATIONS.xlsx)

- The following R scripts generate the rda files available in the **data** folder from the tables included in the XLSX spreadsheet 
      + [DEFAULT_IOTC_LL_EQUATIONS.R](./data-raw/DEFAULT_IOTC_LL_EQUATIONS.R)
      + [DEFAULT_IOTC_LW_EQUATIONS.R](./data-raw/DEFAULT_IOTC_LW_EQUATIONS.R)
      + [DEFAULT_IOTC_WL_EQUATIONS.R](./data-raw/DEFAULT_IOTC_WL_EQUATIONS.R)
      + [DEFAULT_IOTC_WL_ND_KEYS.R](./data-raw/DEFAULT_IOTC_WL_ND_KEYS.R)

- The script [SIZE_BIN_SPLITTERS.R](./data-raw/SIZE_BIN_SPLITTERS.R) creates a table of bin sizes and bin increments for bins sized from 1 to 50, regardless of the unit of measure. These data will used to split size bins larger than 1 in multiple size bins with width = 1, each of which is assigned a fraction (PROP) of the original fish count, depending on its width. The script produces the file `sysdata.rda` that is stored in the **R** folder

- The core equations of the deterministic and non-deterministic keys are defined as R functions in [iotc_base_common_std_sf_core_equations.R](./R/iotc_base_common_std_sf_core_equations.R)

- The core functions for standardising the length and weight data are in the scripts [iotc_base_common_std_sf_core_functions.R](./R/iotc_base_common_std_sf_core_functions.R) and [iotc_base_common_std_sf_core_weight_conversions.R](.R/iotc_base_common_std_sf_core_weight_conversions.R)

- The main standardisation functions are in the script [iotc_base_common_std_sf_standardization.R](./R/iotc_base_common_std_sf_standardization.R])

## How to build the artifacts

- Update the XLSX spreadsheet

- Run the `INITIALIZE_ALL_DATA.R` script that will source the R scripts and produce the .rda files.

- Clone the repository and build the library locally to access the conversion functions:

```
# Install devtools if not already installed
install.packages("devtools")

# Install the package from GitHub
devtools::install_github("iotc-lib-base-common-std")

# Load the library
library(iotc.base.common.std)
```

## Examples of conversions




