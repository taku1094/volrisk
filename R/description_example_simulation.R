#' Example Simulation Results
#'
#' @description
#' A synthetic dataset representing simulated insurance cash flows over time.
#' This dataset is provided for demonstration and validation of key package functions,
#' including risk evaluation (`risk()`) and profit commission calculation (`calc_pc()`).
#' It includes premium and claim flows per simulation trial and policy duration,
#' enabling users to test volatility risk metrics and profit commission calculations.
#'
#' @format A tibble with 10 rows and 5 variables:
#' \describe{
#'   \item{split}{Repetition number of the simulation}
#'   \item{sim_n}{Simulation trial number}
#'   \item{DURATION}{Duration year of the policy}
#'   \item{PREM}{Premium}
#'   \item{CLAIM}{Claim}
#' }
#'
#' @usage data(example_simulation)
#' @keywords datasets
"example_simulation"
