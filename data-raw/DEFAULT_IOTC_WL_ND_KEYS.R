## code to prepare `DEFAULT_IOTC_WL_ND_KEYS` dataset goes here

#Reads the non-deterministic keys to convert from a GGT / RND value to multiple FL values (each with a different probability)
# Applies only to BET and YFT, and can / should be replaced by the deterministic (inverse) equation available for each species or
# by new (i.e., yet to be configured) gear-specific inverse equations...
# As it is now, it ensures that the density probability of each FL for a given GGT / RND sums up to 1. Same can't be said of the
# currently configured LENGTH-WEIGHT keys in the SFAddNewToIOTDB.accdb used to convert the S-F to standard values.
DEFAULT_IOTC_WL_ND_KEYS =
  as.data.table(
    query(connection = DB_IOTCSTATISTICS(),
          query = "
            SELECT
            	S.CODE AS SPECIES_CODE,
            	CASE
            	  WHEN US.CODE = 'GG' THEN 'GGT'
            	  ELSE 'RND'
            	END AS SOURCE_MEASURE_TYPE_CODE,
            	DC.SOURCE_CLASS AS SOURCE_CLASS_LOW,
              UT.CODE AS TARGET_MEASURE_TYPE_CODE,
              DC.TARGET_CLASS AS TARGET_CLASS_LOW,
              DC.PROPORTION
            FROM
              cd_proc_size_nondeterministic_conversions DC
            INNER JOIN
              cl_species S
            ON
              DC.CL_SPECIES_ID = S.ID
            INNER JOIN
              cl_units US
            ON
              DC.CL_UNIT_ID_SOURCE = US.ID
            INNER JOIN
              cl_units UT
            ON
              DC.CL_UNIT_ID_TARGET = UT.ID
            ORDER BY 1, 2, 3, 4, 5"
    )
  )

usethis::use_data(DEFAULT_IOTC_WL_ND_KEYS, overwrite = TRUE)
