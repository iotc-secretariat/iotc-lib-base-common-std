#' String constant for purse-seine, pole-and-line and gillnet gears
#' @export
GEAR_TYPE_PSPLGI = "PSPLGI"

#' String constant for longline and all other gears
#' @export
GEAR_TYPE_LLOT   = "LLOT"

#' Standardizes 'raw' SF data (in _long_ format)
#'
#' Input records undergo a number of processing steps that yield a SF dataset with all
#' records converted to a standard length measurement (species-specific), assigned to a size bin and complemented by
#' a calculated round weight (if proper L-W equations exist).
#'
#' The conversion process consists in:
#'
#' 1) Removing all records with a non-standard (i.e., larger than \code{max_bin_size}) bin size
#'
#' 2) Converting all length measurements to the species-specific standard lengths (through \code{ll_equations})
#'
#' 3) Converting (deterministically) all weight measurements to species-specific standard lengths (through \code{wl_equations})
#'
#' 4) Converting (non-deterministically) all weight measurements to species-specific standard lengths (through \code{wl_keys})
#'
#' 5) Calculating the round weight for all records (through \code{lw_equations})
#'
#' 6) Assigning all records to a size bin of \code{bin_size} cm, collapsing all records with a length less than or equal to \code{first_class_low} cm to the first bin, and capping larger size bins to \code{last_size_bin}
#'
#' @param raw_data A single-species subset of the available SF data
#' @param max_bin_size The maximum allowed bin size (in cm) for the original data
#' @param first_class_low The maximum length (in cm) aggregated under the first size bin
#' @param last_size_bin The last size bin in the output
#' @param bin_size The size (in cm) of the output size bins
#' @param ll_equations The deterministic L-L conversion equations. Defaults to \code{\link{DEFAULT_IOTC_LL_EQUATIONS}}
#' @param lw_equations The deterministic L-W conversion equations. Defaults to \code{\link{DEFAULT_IOTC_LW_EQUATIONS}}
#' @param wl_equations The deterministic W-L conversion equations. Defaults to \code{\link{DEFAULT_IOTC_WL_EQUATIONS}}
#' @param wl_keys      The non-deterministic W-L keys.             Defaults to \code{\link{DEFAULT_IOTC_WL_ND_KEYS}}
#' @return the resulting standardized dataset (in _long_ format)
#' @export
standardize_size_frequencies = function(raw_data, max_bin_size = 5,
                                        first_class_low = 10, last_size_bin = 150, bin_size = 2,
                                        ll_equations = DEFAULT_IOTC_LL_EQUATIONS,
                                        lw_equations = DEFAULT_IOTC_LW_EQUATIONS,
                                        wl_equations = DEFAULT_IOTC_WL_EQUATIONS,
                                        wl_keys      = DEFAULT_IOTC_WL_ND_KEYS) {

  processed_data =
    add_size_bin(
      preprocess_data(raw_data,
                      max_bin_size = max_bin_size,
                      ll_equations = ll_equations,
                      lw_equations = lw_equations,
                      wl_equations = wl_equations,
                      wl_keys      = wl_keys),
      first_class_low = first_class_low,
      last_size_bin = last_size_bin,
      bin_size = bin_size
    )

  processed_data =
    processed_data[, .(FISH_COUNT = sum(FISH_COUNT, na.rm = TRUE), WEIGHT = sum(WEIGHT, na.rm = TRUE)),
                       keyby = .(FLEET_CODE, YEAR, MONTH_START, MONTH_END, FISHING_GROUND_CODE, GEAR_CODE,
                                 FISHERY_TYPE_CODE, FISHERY_GROUP_CODE, FISHERY_CODE, SCHOOL_TYPE_CODE, SPECIES_CODE,
                                 SEX_CODE, RAISE_CODE, MEASURE_TYPE_CODE, SIZE_BIN)]

  processed_data[, CLASS_LOW  := first_class_low + ( as.numeric(str_sub(SIZE_BIN, -3)) - 1 ) * bin_size]
  processed_data[, CLASS_HIGH := CLASS_LOW + bin_size]

  processed_data[SIZE_BIN == "C001", `:=`(CLASS_LOW = 0, CLASS_HIGH = first_class_low)]
  processed_data[SIZE_BIN == last_size_bin, CLASS_HIGH := NA]

  processed_data[MONTH_START %in% 1:3,   QUARTER := "Q1"]
  processed_data[MONTH_START %in% 4:6,   QUARTER := "Q2"]
  processed_data[MONTH_START %in% 7:9,   QUARTER := "Q3"]
  processed_data[MONTH_START %in% 10:12, QUARTER := "Q4"]

  return(processed_data[, .(YEAR, QUARTER, MONTH_START, MONTH_END,
                            FISHING_GROUND_CODE, FLEET_CODE,
                            GEAR_CODE,
                            FISHERY_TYPE_CODE, FISHERY_GROUP_CODE, FISHERY_CODE,
                            SCHOOL_TYPE_CODE,
                            SPECIES_CODE, SEX_CODE,
                            RAISE_CODE, MEASURE_TYPE_CODE,
                            SIZE_BIN, CLASS_LOW, CLASS_HIGH,
                            WEIGHT,
                            FISH_COUNT)])
}


#' Standardizes 'raw' SF data (in _wide_ format)
#'
#' Input records undergo a number of processing steps that yield a SF dataset with all
#' records converted to a standard length measurement (species-specific), assigned to a size bin and complemented by
#' a calculated round weight (if proper L-W equations exist).
#'
#' The conversion process consists in:
#'
#' 1) Removing all records with a non-standard (i.e., larger than \code{max_bin_size}) bin size
#'
#' 2) Converting all length measurements to the species-specific standard lengths (through \code{ll_equations})
#'
#' 3) Converting (deterministically) all weight measurements to species-specific standard lengths (through \code{wl_equations})
#'
#' 4) Converting (non-deterministically) all weight measurements to species-specific standard lengths (through \code{wl_keys})
#'
#' 5) Calculating the round weight for all records (through \code{lw_equations})
#'
#' 6) Assigning all records to a size bin of \code{bin_size} cm, collapsing all records with a length less than or equal to \code{first_class_low} cm to the first bin, and capping larger size bins to \code{last_size_bin}
#'
#' 7) Pivoting the results by size bin (from \code{T001} to \code{T<last_size_bin>})
#'
#' @param raw_data A single-species subset of the available SF data
#' @param max_bin_size The maximum allowed bin size (in cm) for the original data
#' @param first_class_low The maximum length (in cm) aggregated under the first size bin
#' @param last_size_bin The last size bin in the output
#' @param bin_size The size (in cm) of the output size bins
#' @param keep_sex_and_raise_code Whether to stratify or not the output by sex and raise code (defaults to \code{FALSE})
#' @param ll_equations The deterministic L-L conversion equations. Defaults to \code{\link{DEFAULT_IOTC_LL_EQUATIONS}}
#' @param lw_equations The deterministic L-W conversion equations. Defaults to \code{\link{DEFAULT_IOTC_LW_EQUATIONS}}
#' @param wl_equations The deterministic W-L conversion equations. Defaults to \code{\link{DEFAULT_IOTC_WL_EQUATIONS}}
#' @param wl_keys      The non-deterministic W-L keys.             Defaults to \code{\link{DEFAULT_IOTC_WL_ND_KEYS}}
#' @return the resulting standardized dataset (in _wide_ format)
#' @export
standardize_and_pivot_size_frequencies = function(raw_data, max_bin_size = 5,
                                                  first_class_low = 10, last_size_bin = 150, bin_size = 2,
                                                  keep_sex_and_raise_code = FALSE,
                                                  ll_equations = DEFAULT_IOTC_LL_EQUATIONS,
                                                  lw_equations = DEFAULT_IOTC_LW_EQUATIONS,
                                                  wl_equations = DEFAULT_IOTC_WL_EQUATIONS,
                                                  wl_keys      = DEFAULT_IOTC_WL_ND_KEYS) {

  return(
    pivot_data(
      group_by_size_bin(
        add_size_bin(
          preprocess_data(raw_data,
                          max_bin_size = max_bin_size,
                          ll_equations = ll_equations,
                          lw_equations = lw_equations,
                          wl_equations = wl_equations,
                          wl_keys      = wl_keys),
          first_class_low = first_class_low,
          last_size_bin = last_size_bin,
          bin_size = bin_size
        ),
        keep_sex_and_raise_code = keep_sex_and_raise_code
      ),
      first_class_low = first_class_low,
      bin_size = bin_size,
      keep_sex_and_raise_code = keep_sex_and_raise_code
    )
  )
}

#' Alias for \code{\link{standardize_size_frequencies}}
#' @export
standardize.SF = standardize_size_frequencies

#' Alias for \code{\link{standardize_and_pivot_size_frequencies}}
#' @export
standardize_pivot.SF = standardize_and_pivot_size_frequencies

#' Converts between length measurements for a given species (assuming an equation is known)
#' @export
convert_lengths = function(species_code, length_from_code, length_to_code = "FL", measurement, ll_equations = DEFAULT_IOTC_LL_EQUATIONS) {
  equation = ll_equations[SPECIES == species_code &
                          FROM    == length_from_code &
                          TO      == length_to_code]

  if(nrow(equation) == 0)
    stop(paste0("Unable to find a L-L conversion equation for ", species_code, " (", length_from_code, " -> ", length_to_code, ")"))

  if(nrow(equation) > 1)
    stop(paste0("Multiple (", nrow(equation), ") L-L conversion equations for ", species_code, " (", length_from_code, " -> ", length_to_code, ")"))

  equation = equation[1]

  return(
    get(equation$EQ_ID)(equation$C, equation$A, equation$B, measurement)
  )
}

#' Converts between length and weight measurements for a given species (assuming an equation is known)
#' @export
length_to_weight = function(species_code, gear_type = GEAR_TYPE_PSPLGI, length_from_code = "FL", weight_to_code = "RND", measurement, lw_equations = DEFAULT_IOTC_LW_EQUATIONS) {
  equation = lw_equations[SPECIES   == species_code &
                          FROM      == length_from_code &
                          TO        == weight_to_code &
                          GEAR_TYPE == gear_type]

  if(nrow(equation) == 0)
    stop(paste0("Unable to find a L-W conversion equation for ", species_code, " / ", gear_type, " (", length_from_code, " -> ", weight_to_code, ")"))

  if(nrow(equation) > 1)
    stop(paste0("Multiple (", nrow(equation), ") L-W conversion equations for ", species_code, " / ", gear_type, " (", length_from_code, " -> ", weight_to_code, ")"))

  equation = equation[1]

  return(
    get(equation$EQ_ID)(equation$C, equation$A, equation$B, measurement)
  )
}

#' Converts between weight and length measurements for a given species (assuming an equation is known)
#' @export
weight_to_length = function(species_code, gear_type = GEAR_TYPE_PSPLGI, weight_from_code = "RND", length_to_code = "FL", measurement, wl_equations = DEFAULT_IOTC_WL_EQUATIONS) {
  equation = lw_equations[SPECIES == species_code &
                          FROM    == weight_from_code &
                          TO      == length_to_code] #gear type is not (yet?) considered when converting from W to L

  if(nrow(equation) == 0)
    stop(paste0("Unable to find a W-L conversion equation for ", species_code, " / ", gear_type, " (", weight_from_code, " -> ", length_to_code, ")"))

  if(nrow(equation) > 1)
    stop(paste0("Multiple (", nrow(equation), ") W-L conversion equations for ", species_code, " / ", gear_type, " (", weight_from_code, " -> ", length_to_code, ")"))

  equation = equation[1]

  return(
    get(equation$EQ_ID)(equation$C, equation$A, equation$B, measurement)
  )
}

#' Converts between weight and length measurements for a given species (using non-deterministic conversion keys, assuming these are known)
#' @export
weight_to_length_non_deterministic = function(species_code, weight_from_code = "RND", length_to_code = "FL", measurement, wl_keys = DEFAULT_IOTC_WL_ND_KEYS) {
  keys = wl_keys[SPECIES_CODE == species_code &
                 SOURCE_MEASURE_TYPE_CODE == weight_from_code &
                 TARGET_MEASURE_TYPE_CODE == length_to_code &
                 SOURCE_CLASS_LOW == round(measurement)]

  if(nrow(keys) == 0)
    stop(paste0("Unable to find non-deterministic W-L keys for ", species_code, " (", weight_from_code, " -> ", length_to_code, ")"))

  return(
    keys[, .(WEIGHT = SOURCE_CLASS_LOW, LENGTH = TARGET_CLASS_LOW, PROPORTION)]
  )
}

#' Alias for \code{\link{convert_lengths}}
#' @export
convert.LL = convert_lengths

#' Alias for \code{\link{length_to_weight}}
#' @export
convert.LW = length_to_weight

#' Alias for \code{\link{weight_to_length}}
#' @export
convert.WL = weight_to_length

#' Alias for \code{\link{weight_to_length_non_deterministic}}
#' @export
convert.WL.ND = weight_to_length_non_deterministic
