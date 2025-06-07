#' @title Calculate Profit Commission for Reinsurance
#' @description Calculates profit commission payouts for a single set of reinsurance cash flows.
#'
#' This function supports standard profit commission rules using input cash flows and applies key parameters
#' such as the profit commission rate, administrative expense rate, and loss carryforward logic.
#'
#' @param PREM Numeric vector of premiums.
#' @param CLAIM Numeric vector of claims.
#' @param pc_rate Profit commission rate (typically between 0 and 1).
#' @param me Administrative expense rate deducted from premium (typically between 0 and 1).
#' @param loss_carry Whether to apply loss carryforward across periods. Must be "Y" (yes) or "N" (no).
#' @param duration Numeric vector of durations. Must match length of PREM and CLAIM and be consecutive integers.
#'
#' @importFrom dplyr group_by mutate ungroup
#' @importFrom magrittr %>%
#' @return A numeric vector of calculated profit commissions (same length as duration).
#'
#' @examples
#' # Simple numeric vectors
#' PREM <- c(1000, 1000, 1000, 1000, 1000, 1000)
#' CLAIM <- c(600, 1800, 600, 600, 600, 600)
#' duration <- 1:6
#' calc_pc(PREM, CLAIM, pc_rate = 0.9, me = 0.05, loss_carry = "Y", duration = duration)
#'
#' # Using example_simulation dataset
#' example_simulation_with_PC <- example_simulation %>%
#'   dplyr::group_by(split, sim_n) %>%
#'   dplyr::mutate(
#'   PC = calc_pc(PREM,
#'                CLAIM,
#'                pc_rate = 0.9,
#'                me = 0.05,
#'                loss_carry = "N",
#'                duration = DURATION)) %>%
#'   dplyr::ungroup()
#' @export
calc_pc <- function(PREM, CLAIM, pc_rate, me, loss_carry, duration) {
  if (!loss_carry %in% c("Y", "N")) {
    stop("Invalid value for 'loss_carry'. Expected 'Y' or 'N'.")
  }
  if (!is.numeric(PREM)) {
    stop("PREM must be a numeric vector.")
  }
  if (!is.numeric(CLAIM)) {
    stop("CLAIM must be a numeric vector.")
  }
  if (!is.numeric(duration) || !all(diff(sort(duration)) == 1)) {
    stop("duration must be a numeric vector of consecutive integers.")
  }
  if (length(PREM) != length(CLAIM) || length(PREM) != length(duration)) {
    stop("PREM, CLAIM, and duration must be of equal length.")
  }

  max_dur <- max(duration, na.rm = TRUE) + 1
  PC <- numeric(max_dur)
  bal <- numeric(max_dur)
  lc <- numeric(max_dur)

  for (j in seq_len(max_dur - 1)) {
    bal[j] <- PREM[j] - CLAIM[j] - PREM[j] * me - lc[j]
    if (loss_carry == "N") {
      lc[j + 1] <- 0
    } else {
      lc[j + 1] <- ifelse(bal[j] < 0, -bal[j], 0)
    }
    PC[j] <- ifelse(bal[j] > 0, bal[j] * pc_rate, 0)
  }

  return(PC[seq_len(max_dur - 1)])
}
