## code to prepare `SIZE_BIN_SPLITTERS` dataset goes here

# Creates the bin sizes and the bin increments for bins sized from 1 to 50 (regardless of the unit of measure: could be length or weight, doesn't matter...)
BIN_SIZES = c()
BIN_INCS  = c()

for(i in 1:50) {
  BIN_SIZES = append(BIN_SIZES, rep(i, i))
  BIN_INCS  = append(BIN_INCS, seq(0, i - 1))
}

# These 'splitters' are Used to split size bins larger than 1 in multiple size bins with width = 1,
# each of which is assigned a fraction (PROP) of the original fish count, depending on its width...
SIZE_BIN_SPLITTERS =
  data.table(
    BIN_SIZE = BIN_SIZES,
    BIN_INC  = BIN_INCS,
    PROP = 1 / BIN_SIZES
  )

usethis::use_data(SIZE_BIN_SPLITTERS, overwrite = TRUE, internal = TRUE)
