#' @title DEFAULT_IOTC_LL_EQUATIONS
#' @description The length-length conversion equations for the most common IOTC species
#' @format A data table
#' \describe{
#'   \item{\code{SPECIES}}{character The IOTC species code}
#'   \item{\code{FROM}}{character An IOTC length measure code}
#'   \item{\code{TO}}{character The IOTC length measure code for the species' standard length (one among: FL, EFL)}
#'   \item{\code{C}}{double the multiplicative constant for the conversion equation}
#'   \item{\code{A}}{double the 'A' costant for the conversion equation}
#'   \item{\code{B}}{double the 'B' costant for the conversion equation}
#'   \item{\code{EQ_ID}}{character the identifier for the conversion equation to use (one among: IDENTITY, SQUARED, PROP, INVPROP, INVPROP_ALT, POW, INVPOW)}
#'   \item{\code{NOTES}}{character Additional notes}
#' }
#' @source \url{https://iotc.org/WPTT/24/Data/13-Equations/}
"DEFAULT_IOTC_LL_EQUATIONS"

#' @title DEFAULT_IOTC_LW_EQUATIONS
#' @description The length-weight conversion equations for the most common IOTC species
#' @format A data table
#' \describe{
#'   \item{\code{SPECIES}}{character The IOTC species code}
#'   \item{\code{FROM}}{character The IOTC length measure code for the species' standard length (one among: FL, EFL)}
#'   \item{\code{TO}}{character The IOTC weight measure code for the species' standard weight (one among: RND)}
#'   \item{\code{GEAR_TYPE}}{character The category of gears to which the equation applies (one among: PSPLGI, LLOT)}
#'   \item{\code{C}}{double the multiplicative constant for the conversion equation}
#'   \item{\code{A}}{double the 'A' costant for the conversion equation}
#'   \item{\code{B}}{double the 'B' costant for the conversion equation}
#'   \item{\code{EQ_ID}}{character the identifier for the conversion equation to use (one among: IDENTITY, SQUARED, PROP, INVPROP, INVPROP_ALT, POW, INVPOW)}
#'   \item{\code{NOTES}}{character Additional notes}
#' }
#' @source \url{https://iotc.org/WPTT/24/Data/13-Equations/}
#' @source \url{https://iotc.org/WPNT/12/Data/11-Equations/}
"DEFAULT_IOTC_LW_EQUATIONS"

#' @title DEFAULT_IOTC_WL_EQUATIONS
#' @description The weight-length conversion equations for some common IOTC species
#' @format A data table
#' \describe{
#'   \item{\code{SPECIES}}{character The IOTC species code}
#'   \item{\code{FROM}}{character An IOTC weight measure code (one among: RND, GGT)}
#'   \item{\code{TO}}{character The IOTC length measure code for the species' standard length (one among: FL, EFL)}
#'   \item{\code{C}}{double the multiplicative constant for the conversion equation}
#'   \item{\code{A}}{double the 'A' costant for the conversion equation}
#'   \item{\code{B}}{double the 'B' costant for the conversion equation}
#'   \item{\code{EQ_ID}}{character the identifier for the conversion equation to use (one among: IDENTITY, SQUARED, PROP, INVPROP, INVPROP_ALT, POW, INVPOW)}
#'   \item{\code{NOTES}}{character Additional notes}
#' }
#' @source \url{https://iotc.org/WPTT/24/Data/13-Equations/}
"DEFAULT_IOTC_WL_EQUATIONS"

#' @title DEFAULT_IOTC_WL_ND_KEYS
#' @description Keys to implement a non-deterministic weight-length conversion equations for BET and YFT
#' @format A data frame
#'  \describe{
#'   \item{\code{SPECIES}}{character The IOTC species code}
#'   \item{\code{SOURCE_MEASURE_TYPE_CODE}}{character The IOTC weight measure code for the species' standard weight (one among: RND, GGT)}
#'   \item{\code{SOURCE_CLASS_LOW}}{integer the lower bound of the source weight class (in kg)}
#'   \item{\code{TARGET_MEASURE_TYPE_CODE}}{character The IOTC length measure code for the species' standard weight (one among: FL)}
#'   \item{\code{TARGET_CLASS_LOW}}{integer the lower bound of the target length class (in cm)}
#'   \item{\code{PROPORTION}}{double The proportion of fish allocated to the target weight class (converted from a source length class)}
#' }
#' @source \url{https://iotc.org/WPTT/24/Data/13-Equations/}
"DEFAULT_IOTC_WL_ND_KEYS"
