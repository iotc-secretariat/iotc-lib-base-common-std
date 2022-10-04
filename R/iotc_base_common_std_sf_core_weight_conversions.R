GGT_TO_RND = function(data) {
  data[MEASURE_TYPE_CODE == "GGT", `:=`(MEASURE_TYPE_CODE = "RND",
                                        CLASS_LOW  = round(CLASS_LOW  * 1.13, 0),
                                        CLASS_HIGH = round(CLASS_HIGH * 1.13, 0))]

  return (data)
}

RND_TO_GGT = function(data) {
  data[MEASURE_TYPE_CODE == "RND", `:=`(MEASURE_TYPE_CODE = "GGT",
                                        CLASS_LOW  = round(CLASS_LOW  / 1.13, 0),
                                        CLASS_HIGH = round(CLASS_HIGH / 1.13, 0))]

  return (data)
}
