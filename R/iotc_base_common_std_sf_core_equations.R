# The various deterministic equations used to convert L-L, L-W and W-L

EQ_IDENTITY = function(C, A, B, M) {
  return(
    M
  )
}

EQ_SQUARED = function(C, A, B, M) {
  return(
    C * ( M + A ) ^ 2 / ( B ^ 2 )
  )
}

EQ_PROP = function(C, A, B, M) {
  return(
    C * ( ( A * M ) + B )
  )
}

EQ_INVPROP = function(C, A, B, M) {
  return(
    C * ( M + B ) / A
  )
}#

EQ_INVPROP_ALT = function(C, A, B, M) {
  return(
    C * ( M - B ) / A
  )
}

EQ_POW = function(C, A, B, M) {
  return(
    C * A * ( M ^ B )
  )
}

EQ_INVPOW = function(C, A, B, M) {
  return(
    C * ( M / A ) ^ ( 1 / B )
  )
}
