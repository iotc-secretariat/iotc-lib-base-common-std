# Assigns one of the two categories of fisheries used to identify the L-W equation for some species.
# Doesn't make much sense to have different eqs. by fishery, IMHO. Nevertheless, here we are: applies
# to BET and YFT only.
assign_length_gear_type = function(raw_data) {
  raw_data[ FISHERY_GROUP_CODE %in% c("PS", "BB", "GN"), LENGTH_FISHERY_CODE := "PSPLGI"]
  raw_data[!FISHERY_GROUP_CODE %in% c("PS", "BB", "GN"), LENGTH_FISHERY_CODE := "LLOT"]

  return(raw_data)
}

group_by_class_low = function(raw_data) {
  return (
    raw_data[, .(FISH_COUNT = sum(FISH_COUNT, na.rm = TRUE)),
                 keyby = .(FLEET_CODE, YEAR, MONTH_START, MONTH_END, FISHING_GROUND_CODE, GEAR_CODE,
                           FISHERY_TYPE_CODE, FISHERY_GROUP_CODE, FISHERY_CODE, SCHOOL_TYPE_CODE, SPECIES_CODE,
                           MEASURE_TYPE_CODE, MEASURE_UNIT_CODE, SEX_CODE, RAISE_CODE, CLASS_LOW, CLASS_HIGH)]
  )
}

assign_bin_size = function(raw_data) {
  raw_data[, BIN_SIZE := CLASS_HIGH - CLASS_LOW ]

  return(raw_data)
}

split_bins = function(raw_data) {
  raw_data = merge(raw_data, SIZE_BIN_SPLITTERS,
                   by = "BIN_SIZE",
                   all.x = TRUE, all.y = TRUE,
                   allow.cartesian = TRUE)[!is.na(CLASS_LOW)]

  raw_data[ , `:=`(CLASS_LOW  = CLASS_LOW + BIN_INC,
                   CLASS_HIGH = CLASS_LOW + BIN_INC + 1, FISH_COUNT = FISH_COUNT * PROP * 1.0)]
  return(
    group_by_class_low(
      raw_data
    )
  )
}

filter_data = function(raw_data, max_bin_size) {
  l_info(paste0("Filtering original data with size bin <= ", max_bin_size, " cm..."))

  FC_BEFORE = sum(raw_data$FISH_COUNT, na.rm = TRUE)

  filtered =
    raw_data[FISH_COUNT > 0 &
         CLASS_LOW <= 500 &        # This limit applies to all type of measurement... So far, only 4 measures in the IOTDB exist with CLASS_LOW > 500 and these are all due to mistakes
         BIN_SIZE <= max_bin_size] # In theory this should only apply to length measurements... Anyways, GGT / RND are always provided with 1 as bin size...

  FC_AFTER = sum(filtered$FISH_COUNT, na.rm = TRUE)

  l_info("Finished filtering original data.")
  l_info(paste0("Initial samples: ", FC_BEFORE, " - Filtered samples: ", FC_AFTER, " - Diff: ", ( FC_AFTER - FC_BEFORE )))

  return(filtered)
}

convert_length_synonyms = function(raw_data, species_code, species_specific_equations, standard_length, alternative_lengths) {
  for(l in alternative_lengths) {
    if(nrow(species_specific_equations[FROM == l]) == 0) { # No equation identified: perform 1:1 conversion to standard length
      to_update = raw_data[SPECIES_CODE == species_code & MEASURE_TYPE_CODE == l]

      to_update_num = sum(to_update$FISH_COUNT, na.rm = TRUE)

      if(to_update_num > 0) {
        l_info(paste0(" ! 'identity' conversion of ", nrow(to_update), " records from ", l, " to ", standard_length, " for a total of ", sum(to_update$FISH_COUNT, na.rm = TRUE), " fish..."))

        raw_data[SPECIES_CODE == species_code & MEASURE_TYPE_CODE == l, MEASURE_TYPE_CODE := standard_length]
      }
    }
  }
}

apply_length_length_deterministic_equations = function(raw_data, ll_equations) {
  l_info("= [ L-L ] ===================================")

  l_info("Applying L-L deterministic equations...")

  if(!is_available(ll_equations) | is.null(nrow(ll_equations))) {
    l_warn("No L-L equations provided: returning original data...")

    l_info("! [ L-L ] !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")

    return(raw_data)
  }

  FC_BEFORE = sum(raw_data$FISH_COUNT, na.rm = TRUE);

  for(s in unique(raw_data$SPECIES_CODE)) {
    l_info(paste0("L-L : processing species ", s, "..."))

    s_ll_equations = ll_equations[SPECIES == s]

    convert_length_synonyms(raw_data, s, s_ll_equations, standard_length = "FL",  alternative_lengths = c("FLB", "FLC", "FLCT", "FLUT"))
    convert_length_synonyms(raw_data, s, s_ll_equations, standard_length = "EFL", alternative_lengths = c("EFUT"))
    convert_length_synonyms(raw_data, s, s_ll_equations, standard_length = "CKL", alternative_lengths = c("CKUT"))
    convert_length_synonyms(raw_data, s, s_ll_equations, standard_length = "PAL", alternative_lengths = c("PALT"))
    convert_length_synonyms(raw_data, s, s_ll_equations, standard_length = "PCL", alternative_lengths = c("PCLT"))
    convert_length_synonyms(raw_data, s, s_ll_equations, standard_length = "LDF", alternative_lengths = c("LDFT"))

    if(nrow(s_ll_equations) == 0 | is.null(nrow(s_ll_equations))) {
      l_warn(paste0("No L-L equations available for ", s))
    } else {
      for(e in 1:nrow(s_ll_equations)) {
        eq = s_ll_equations[e]

        MF = eq$FROM[[1]]
        MT = eq$TO[[1]]
        FN = get(eq$EQ[[1]])
        C  = eq$C[[1]]
        A  = eq$A[[1]]
        B  = eq$B[[1]]

        l_info(paste0(" * Converting ", MF, " to ", MT))

        raw_data[SPECIES_CODE == s & MEASURE_TYPE_CODE == MF,
                 `:=`(MEASURE_TYPE_CODE = MT,
                        CLASS_LOW  = round(FN(C, A, B, CLASS_LOW), 0),
                        CLASS_HIGH = round(FN(C, A, B, CLASS_HIGH), 0))]

        raw_data[SPECIES_CODE == s & MEASURE_TYPE_CODE == MT & CLASS_LOW == CLASS_HIGH,
                 CLASS_HIGH := CLASS_LOW + 1]
      }
    }
  }

  FC_AFTER = sum(raw_data$FISH_COUNT, na.rm = TRUE);

  l_info("Finished applying L-L deterministic conversions!")
  l_info(paste0("Fish count before: ", FC_BEFORE, " - after: ", FC_AFTER, " - Diff: ", ( FC_AFTER - FC_BEFORE )))

  l_info("# [ L-L ] ###################################")

  return(raw_data)
}

apply_weight_length_deterministic_equations = function(raw_data, wl_equations) {
  l_info("= [ W-L ] ===================================")

  l_info("Applying W-L deterministic equations...")

  if(!is_available(wl_equations) | is.null(nrow(wl_equations))) {
    l_warn("No W-L equations provided: returning original data...")

    l_info("! [ W-L ] !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")

    return(raw_data)
  }

  FC_BEFORE = sum(raw_data$FISH_COUNT, na.rm = TRUE);

  for(s in unique(raw_data$SPECIES_CODE)) {
    l_info(paste0("W-L : processing species ", s, "..."))

    s_wl_equations = wl_equations[SPECIES == s]

    if(nrow(s_wl_equations) == 0 | is.null(nrow(s_wl_equations))) {
      l_warn(paste0("No W-L equations available for ", s))
    } else {
      for(e in 1:nrow(s_wl_equations)) {
        eq = s_wl_equations[e]

        MF = eq$FROM[[1]]
        MT = eq$TO[[1]]
        FN = get(eq$EQ[[1]])
        C  = eq$C[[1]]
        A  = eq$A[[1]]
        B  = eq$B[[1]]

        l_info(paste0("Converting ", MF, " to ", MT))

        raw_data[SPECIES_CODE == s & MEASURE_TYPE_CODE == MF,
                 `:=`(MEASURE_TYPE_CODE = MT,
                      CLASS_LOW  = round(FN(C, A, B, CLASS_LOW), 0),
                      CLASS_HIGH = round(FN(C, A, B, CLASS_HIGH), 0))]

        raw_data[SPECIES_CODE == s & MEASURE_TYPE_CODE == MT & CLASS_LOW == CLASS_HIGH,
                 CLASS_HIGH := CLASS_LOW + 1]
      }
    }
  }

  FC_AFTER = sum(raw_data$FISH_COUNT, na.rm = TRUE);

  l_info("Finished applying W-L deterministic conversions!")
  l_info(paste0("Fish count before: ", FC_BEFORE, " - after: ", FC_AFTER, " - Diff: ", ( FC_AFTER - FC_BEFORE )))

  l_info("# [ W-L ] ###################################")

  return(raw_data)
}

apply_weight_length_nondeterministic_keys = function(raw_data, wl_keys) {
  l_info("= [ W-L ND ] ================================")

  l_info("Applying W-L non-deterministic keys...")

  if(!is_available(wl_keys) | is.null(nrow(wl_keys))) {
    l_warn("No W-L keys provided: returning original data...")

    l_info("! [ W-L ND ] !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")

    return(raw_data)
  }

  FC_BEFORE = sum(raw_data$FISH_COUNT, na.rm = TRUE);

  merged = merge(raw_data, wl_keys,
                 by.x = c("SPECIES_CODE", "MEASURE_TYPE_CODE", "CLASS_LOW"),
                 by.y = c("SPECIES_CODE", "SOURCE_MEASURE_TYPE_CODE", "SOURCE_CLASS_LOW"),
                 all.x = TRUE, all.y = FALSE, allow.cartesian = TRUE)

  merged[!is.na(PROPORTION), `:=`(MEASURE_TYPE_CODE = TARGET_MEASURE_TYPE_CODE, # Should always be FL...
                                  MEASURE_UNIT_CODE = "CM",
                                  CLASS_LOW = TARGET_CLASS_LOW,
                                  CLASS_HIGH = TARGET_CLASS_LOW + 1,
                                  FISH_COUNT = FISH_COUNT * PROPORTION)]


  FC_AFTER = sum(merged$FISH_COUNT, na.rm = TRUE);

  l_info("Finished applying W-L non deterministic conversions!")
  l_info(paste0("Fish count before: ", FC_BEFORE, " - after: ", FC_AFTER, " - Diff: ", ( FC_AFTER - FC_BEFORE )))

  l_info("# [ W-L ND ] ################################")

  return(
    group_by_class_low(
      merged
    )
  )
}

assign_round_weight_from_standard_length = function(raw_data, lw_equations) {
  l_info("= [ L-W ] ===================================")

  l_info("Converting standard length to round weight...")

  if(!is_available(lw_equations) | is.null(nrow(lw_equations))) {
    l_warn("No -LW equations provided: returning original data...")

    l_info("! [ L-W ] !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")

    raw_data$WEIGHT = numeric()

    return(raw_data)
  }

  raw_data = assign_length_gear_type(raw_data)
  raw_data$WEIGHT = numeric()

  FC_BEFORE = sum(raw_data$FISH_COUNT, na.rm = TRUE);

  for(s in unique(raw_data$SPECIES_CODE)) {
    s_lw_equations = lw_equations[SPECIES == s]

    if(nrow(s_lw_equations) == 0 | is.null(nrow(s_lw_equations))) {
      stop(paste0("No L-W equations available for ", s))
    } else {
      for(e in 1:nrow(s_lw_equations)) {
        eq = s_lw_equations[e]

        LF = eq$GEAR_TYPE[[1]]
        MT = eq$FROM[[1]]
        FN = get(eq$EQ[[1]])
        C  = eq$C[[1]]
        A  = eq$A[[1]]
        B  = eq$B[[1]]

        raw_data[SPECIES_CODE == s & LENGTH_FISHERY_CODE == LF & MEASURE_TYPE_CODE == MT,
                 WEIGHT := FISH_COUNT * FN(C, A, B, CLASS_LOW + ( CLASS_HIGH - CLASS_LOW ) / 2)]
      }
    }
  }

  NO_WEIGHT = raw_data[is.na(WEIGHT)]
  NO_WEIGHT$SPECIES_CODE = as.factor(NO_WEIGHT$SPECIES_CODE)
  NO_WEIGHT$MEASURE_TYPE_CODE = as.factor(NO_WEIGHT$MEASURE_TYPE_CODE)
  NO_WEIGHT$LENGTH_FISHERY_CODE = as.factor(NO_WEIGHT$LENGTH_FISHERY_CODE)

  if(nrow(NO_WEIGHT) > 0) {
    l_warn(paste0(nrow(NO_WEIGHT), " records out of ", nrow(raw_data), " could not be assigned a weight, for a total of ",
                  sum(NO_WEIGHT$FISH_COUNT, na.rm = TRUE), " individual fish..."))

    #print(summary(NO_WEIGHT))

    print(
      NO_WEIGHT[, .(FISH_COUNT = sum(FISH_COUNT, na.rm = TRUE)), keyby = .(FLEET_CODE, YEAR, MONTH_START, MONTH_END, FISHING_GROUND_CODE, GEAR_CODE, MEASURE_TYPE_CODE, CLASS_LOW)]
    )
  }

  FC_AFTER = sum(raw_data$FISH_COUNT, na.rm = TRUE);

  l_info("Standard length - round weight conversion completed!")
  l_info(paste0("Fish count before: ", FC_BEFORE, " - after: ", FC_AFTER, " - Diff: ", ( FC_AFTER - FC_BEFORE )))

  l_info("# [ L-W ] ###################################")

  return(raw_data[!is.na(WEIGHT)])
}

preprocess_data = function(raw_data, ll_equations, wl_equations, lw_equations, wl_keys, max_bin_size = 5) {
  raw_data$FISH_COUNT = as.numeric(raw_data$FISH_COUNT)

  NUM_FISH_ORIG = round(sum(raw_data$FISH_COUNT))

  raw_data =
    assign_round_weight_from_standard_length(
      split_bins(
        assign_bin_size(
          apply_weight_length_deterministic_equations(
            apply_length_length_deterministic_equations(
              apply_weight_length_nondeterministic_keys(
                split_bins(
                  filter_data(
                    assign_bin_size(
                      assign_length_gear_type(
                        raw_data
                      )
                    ),
                    max_bin_size
                  )
                ), wl_keys
              ), ll_equations
            ), wl_equations
          )
        )
      ), lw_equations
    )[!is.na(CLASS_LOW) & FISH_COUNT > 0]

  NUM_FISH_PREPROC = round(sum(raw_data$FISH_COUNT), 0)

  l_info(paste0("Original num. fish           : ", NUM_FISH_ORIG))
  l_info(paste0("Num. fish after preprocessing: ", NUM_FISH_PREPROC))
  l_info(paste0("Difference                   : ", NUM_FISH_PREPROC - NUM_FISH_ORIG))

  return(raw_data)
}

add_size_bin = function(raw_data, first_class_low = 10, last_size_bin = 150, bin_size = 2) {
  # Extremely inefficient, yet easy to write...
  raw_data[CLASS_LOW <= first_class_low, SB := 1]
  raw_data[CLASS_LOW >  first_class_low, SB := 1 + floor((CLASS_LOW - first_class_low) / bin_size)]
  raw_data[SB > last_size_bin, SB  := last_size_bin]

  raw_data[, SIZE_BIN := paste0("C", str_sub(paste0("000", SB), -3))]
  raw_data$SB = NULL

  ALL_SIZE_BINS = paste0("C", str_sub(paste0("000", 1:last_size_bin), -3))

  raw_data$SIZE_BIN = factor(
    raw_data$SIZE_BIN,
    labels = ALL_SIZE_BINS,
    levels = ALL_SIZE_BINS,
    ordered = TRUE
  )

  return(raw_data)
}

group_by_size_bin = function(raw_data, keep_sex_and_raise_code = FALSE) {
  if(keep_sex_and_raise_code) {
    return(
      raw_data[, .(FISH_COUNT = sum(FISH_COUNT, na.rm = TRUE), WEIGHT = sum(WEIGHT, na.rm = TRUE)),
                   keyby = .(FLEET_CODE, YEAR, MONTH_START, MONTH_END, FISHING_GROUND_CODE, GEAR_CODE,
                             FISHERY_TYPE_CODE, FISHERY_GROUP_CODE, FISHERY_CODE, SCHOOL_TYPE_CODE, SPECIES_CODE,
                             SEX_CODE, RAISE_CODE, MEASURE_TYPE_CODE, SIZE_BIN)]
    )
  } else {
    return(
      raw_data[, .(FISH_COUNT = sum(FISH_COUNT, na.rm = TRUE), WEIGHT = sum(WEIGHT, na.rm = TRUE)),
                   keyby = .(FLEET_CODE, YEAR, MONTH_START, MONTH_END, FISHING_GROUND_CODE, GEAR_CODE,
                             FISHERY_TYPE_CODE, FISHERY_GROUP_CODE, FISHERY_CODE, SCHOOL_TYPE_CODE, SPECIES_CODE,
                             MEASURE_TYPE_CODE, SIZE_BIN)]
    )
  }
}

pivot_data = function(raw_data, first_class_low = 10, bin_size = 2, keep_sex_and_raise_code = FALSE) {
  raw_data$FIRST_CLASS_LOW = first_class_low
  raw_data$SIZE_INTERVAL = bin_size

  if(keep_sex_and_raise_code) {
    raw_data[, `:=`(NO_FISH = sum(FISH_COUNT, na.rm = TRUE),
                    KG_FISH = sum(WEIGHT, na.rm = TRUE)),
               by = .(FLEET_CODE, YEAR, MONTH_START, MONTH_END, FISHING_GROUND_CODE, GEAR_CODE,
                      #FISHERY_TYPE_CODE, FISHERY_GROUP_CODE, FISHERY_CODE,
                      SPECIES_CODE, SCHOOL_TYPE_CODE,
                      SEX_CODE, RAISE_CODE, MEASURE_TYPE_CODE, FIRST_CLASS_LOW, SIZE_INTERVAL)]

    raw_data[, AVG_WEIGHT := fifelse(NO_FISH == 0, 0, KG_FISH / NO_FISH)]

    return(
      dcast.data.table(
        raw_data,
        formula = FLEET_CODE + YEAR + MONTH_START + MONTH_END + FISHING_GROUND_CODE + GEAR_CODE +
          #FISHERY_TYPE_CODE + FISHERY_GROUP_CODE + FISHERY_CODE +
          SPECIES_CODE + SCHOOL_TYPE_CODE +
          SEX_CODE + RAISE_CODE + MEASURE_TYPE_CODE + FIRST_CLASS_LOW + SIZE_INTERVAL + NO_FISH + KG_FISH + AVG_WEIGHT ~ SIZE_BIN,
        fun.aggregate = sum,
        fill = 0,
        value.var = c("FISH_COUNT"),
        drop = c(TRUE, FALSE)
      )
    )
  } else {
    raw_data[, `:=`(NO_FISH = sum(FISH_COUNT, na.rm = TRUE),
                    KG_FISH = sum(WEIGHT, na.rm = TRUE)),
               by = .(FLEET_CODE, YEAR, MONTH_START, MONTH_END, FISHING_GROUND_CODE, GEAR_CODE,
                      #FISHERY_TYPE_CODE, FISHERY_GROUP_CODE, FISHERY_CODE,
                      SPECIES_CODE, SCHOOL_TYPE_CODE,
                      MEASURE_TYPE_CODE, FIRST_CLASS_LOW, SIZE_INTERVAL)]

    raw_data[, AVG_WEIGHT := fifelse(NO_FISH == 0, 0, KG_FISH / NO_FISH)]

    return(
      dcast.data.table(
        raw_data,
        formula = FLEET_CODE + YEAR + MONTH_START + MONTH_END + FISHING_GROUND_CODE + GEAR_CODE +
          #FISHERY_TYPE_CODE + FISHERY_GROUP_CODE + FISHERY_CODE +
          SPECIES_CODE + SCHOOL_TYPE_CODE +
          MEASURE_TYPE_CODE + FIRST_CLASS_LOW + SIZE_INTERVAL + NO_FISH + KG_FISH + AVG_WEIGHT ~ SIZE_BIN,
        fun.aggregate = sum,
        fill = 0,
        value.var = c("FISH_COUNT"),
        drop = c(TRUE, FALSE)
      )
    )
  }
}
