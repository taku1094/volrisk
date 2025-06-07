#' @title Create Insurance Portfolio for Simulation
#'
#' @description
#' This function standardizes and validates raw insurance portfolio data
#' to prepare it for simulation. It ensures all necessary fields are correctly
#' mapped and conform to required formats. If no column mapping is provided,
#' the user will be prompted interactively.
#'
#' @param data A data.frame containing raw insurance portfolio data.
#' @param cols A named list with column mappings. The list should include:
#'   \code{unique_id}, \code{client_id}, \code{duration},
#'   \code{mortality}, \code{lapse}, \code{nar}, \code{rate}.
#'
#' @return A cleaned data.frame with standardized column names:
#'   \code{unique_id}, \code{client_id}, \code{duration},
#'   \code{mortality}, \code{lapse}, \code{nar}, \code{rate}.
#'
#' @examples
#' make_portfolio(example_portfolio, cols = list(
#'   unique_id = "POL_ID",
#'   client_id = "CLIENT_ID",
#'   duration = "DURATION",
#'   mortality = "MORTALITY",
#'   lapse = "LAPSE",
#'   nar = "NAR",
#'   rate = "RATE"
#' ))
#'
#' @export
make_portfolio <- function(data, cols = NULL) {
  if (!is.data.frame(data)) stop("Input must be a data.frame.")
  col_names <- names(data)
  required_keys <- c("unique_id", "client_id", "duration", "mortality", "lapse", "nar", "rate")

  key_descriptions <- c(
    unique_id = "policy identifier (unique per contract)",
    client_id = "insured person identifier (can repeat)",
    duration = "policy duration (positive integer)",
    mortality = "mortality rate (0 to 1)",
    lapse = "lapse/withdrawal rate (0 to 1)",
    nar = "net amount at risk (non-negative)",
    rate = "reinsurance premium rate (0 to 1)"
  )

  ask_colname <- function(key) {
    cat("\nAvailable columns:\n")
    cat(paste0("  - ", col_names), sep = "\n")
    desc <- key_descriptions[[key]]
    readline(prompt = paste0("\nEnter column name for ", key, " [", desc, "] (type 'back' to go back): "))
  }

  if (is.null(cols)) cols <- list()

  i <- 1
  while (i <= length(required_keys)) {
    key <- required_keys[i]

    if (is.null(cols[[key]])) {
      input <- ask_colname(key)

      if (tolower(input) == "back") {
        if (i == 1) {
          cat("Cannot go back from the first item.\n")
          next
        } else {
          cols[[required_keys[i - 1]]] <- NULL
          i <- i - 1
          next
        }
      }

      if (!(input %in% col_names)) {
        cat("Invalid input. Please enter one of the listed columns.\n")
      } else {
        cols[[key]] <- input
        i <- i + 1
      }
    } else {
      i <- i + 1
    }
  }

  df <- data[, unlist(cols)]
  names(df) <- required_keys

  # Validation
  if (anyNA(df)) stop("Input contains missing (NA) values.")
  if (any(!is.numeric(df$duration)) || any(df$duration %% 1 != 0 | df$duration <= 0)) {
    stop("Duration must be a positive integer.")
  }
  if (any(!is.numeric(df$mortality)) || any(df$mortality < 0 | df$mortality > 1)) {
    stop("Mortality must be numeric between 0 and 1.")
  }
  if (any(!is.numeric(df$lapse)) || any(df$lapse < 0 | df$lapse > 1)) {
    stop("Lapse must be numeric between 0 and 1.")
  }
  if (any(!is.numeric(df$nar)) || any(df$nar < 0)) {
    stop("NAR must be numeric and non-negative.")
  }
  if (any(!is.numeric(df$rate)) || any(df$rate < 0 | df$rate > 1)) {
    stop("Rate must be numeric between 0 and 1.")
  }

  # Check unique_id → client_id mapping
  pair_df <- unique(df[, c("unique_id", "client_id")])
  if (nrow(pair_df) != length(unique(df$unique_id))) {
    stop("Each unique_id must be associated with only one client_id.")
  }

  # Check uniform duration count per unique_id
  if (length(unique(table(df$unique_id))) != 1) {
    stop("Each unique_id must have the same number of duration rows.")
  }

  return(df)
}
