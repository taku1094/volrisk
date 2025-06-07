.onLoad <- function(libname, pkgname) {
  utils::globalVariables(c(
    "unique_id", "client_id", "mortality", "lapse",
    "px", "pw", "tpx", "tpw"
  ))
}
utils::globalVariables(c("DURATION", "sim_n", "PREM", "CLAIM", "BAL"))
