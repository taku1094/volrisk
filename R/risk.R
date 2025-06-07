#' @title Compute Risk Measures
#' @description
#' Computes Value-at-Risk (VaR) and Tail Value-at-Risk (TVaR) based on simulated insurance cash flows.
#' The function supports discounting for multi-year horizons and can evaluate loss ratio (LR) or balance (BAL).
#'
#' @param cf A data.frame containing simulated cash flow results. Must include columns: `split`, `sim_n`, `PREM`, `CLAIM`, `DURATION`.
#' @param time_horizon A numeric vector specifying time horizons (e.g., c(1, 5, 10)).
#' @param level A numeric vector of confidence levels for risk quantification (e.g., c(0.01, 0.1, 0.99)).
#' @param discount Annual discount rate to convert future cash flows to present value. Default is 0 (no discount).
#' @param output The metric to be analyzed: `"LR"` for loss ratio or `"BAL"` for balance. Default is `"LR"`.
#'
#' @return A data.frame summarizing VaR and TVaR values across the specified horizons and confidence levels.
#' @export
#'
#' @importFrom dplyr bind_rows filter group_by summarise mutate
#'
#' @examples
#' # Using example_simulation dataset (assumes data is loaded)
#' result <- risk(example_simulation,
#'                time_horizon = c(1, 10),
#'                level = c(0.01, 0.99),
#'                discount = 0.02,
#'                output = "BAL")
#' print(result)
risk <- function(cf, time_horizon = c(1), level = c(0.005, 0.01, 0.1, 0.2, 0.5, 0.8, 0.9, 0.99, 0.995), discount = 0, output = "LR") {
  if (!is.data.frame(cf)) {
    stop("'cf' must be a data.frame.")
  }

  required_cols <- c("split", "sim_n", "PREM", "CLAIM", "DURATION")
  missing_cols <- setdiff(required_cols, names(cf))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in 'cf': ", paste(missing_cols, collapse = ", "))
  }

  if (!"BAL" %in% names(cf)) {
    cf$BAL <- cf$PREM - cf$CLAIM
    message("'BAL' was automatically calculated from PREM - CLAIM.")
  }

  if (discount != 0) {
    cf$PREM <- cf$PREM * (1 + discount)^(-cf$DURATION + 1)
    cf$CLAIM <- cf$CLAIM * (1 + discount)^(-cf$DURATION + 1)
    cf$BAL <- cf$BAL * (1 + discount)^(-cf$DURATION + 1)
  }

  VaR_calc <- function(data, alpha) {
    total <- nrow(data)
    sorted <- data[order(data[[output]], decreasing = (output == "BAL")), ]
    pos <- ceiling(total * alpha)
    return(sorted[[output]][pos])
  }

  TVaR_calc <- function(data, alpha) {
    total <- nrow(data)
    sorted <- data[order(data[[output]], decreasing = (output == "BAL")), ]
    pos <- ceiling(total * alpha)
    subset <- sorted[pos:total, ]
    if (output == "LR") {
      return(sum(subset$CLAIM) / sum(subset$PREM))
    } else {
      return(mean(subset[[output]]))
    }
  }

  results <- dplyr::bind_rows(lapply(time_horizon, function(h) {
    df_h <- dplyr::filter(cf, DURATION <= h) |>
      dplyr::group_by(split, sim_n) |>
      dplyr::summarise(
        PREM = sum(PREM),
        CLAIM = sum(CLAIM),
        BAL = sum(BAL),
        .groups = "drop"
      ) |>
      dplyr::mutate(LR = CLAIM / PREM)

    data.frame(
      alpha = level,
      horizon = h,
      VaR = sapply(level, function(a) VaR_calc(df_h, a)),
      TVaR = sapply(level, function(a) TVaR_calc(df_h, a))
    )
  }))

  return(results)
}
