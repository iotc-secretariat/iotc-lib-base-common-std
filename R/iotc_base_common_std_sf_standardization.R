#' DESCRIPTION TO BE ADDED
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
#' @export
standardize_size_frequencies = function(raw_data, max_bin_size = 5,
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
