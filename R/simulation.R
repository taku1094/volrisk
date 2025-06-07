#' Run Simulation of Death and Lapse Scenarios
#'
#' @description
#' This function simulates death and lapse events over the insurance period
#' using the portfolio created by `make_portfolio()`. The total number of simulations
#' is `n_sim * split`, and results are saved in CSV or Parquet format in the specified path.

#'
#' @param df A data.frame returned by `make_portfolio()`
#' @param n_sim Number of simulations per split
#' @param split Number of splits (simulation repetitions)
#' @param seed Random seed (optional)
#' @param output_format Output file format: "csv" or "parquet"
#' @param output_path Path to save the simulation results and logs
#'
#' @return No return value. Files are saved to `output_path`.
#' @importFrom dplyr group_by mutate ungroup select distinct n filter
#' @importFrom arrow write_parquet
#' @importFrom rstudioapi isAvailable selectDirectory
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach foreach %dopar%
#' @importFrom progress progress_bar
#' @importFrom data.table fwrite
#' @importFrom stringr str_c
#' @importFrom stats runif rmultinom
#'
#' @export
#' @examples
#' # Prepare portfolio
#' port <- make_portfolio(example_portfolio, cols = list(
#'   unique_id = "POL_ID",
#'   client_id = "CLIENT_ID",
#'   duration = "DURATION",
#'   mortality = "MORTALITY",
#'   lapse = "LAPSE",
#'   nar = "NAR",
#'   rate = "RATE"
#' ))
#'
#' # Run simulation (output_path = tempdir() for demonstration)
#' simulation(port,
#'            n_sim = 10,
#'            split = 100,
#'            seed = 12345,
#'            output_format = "csv",
#'            output_path = tempdir()
#'            )
simulation <- function(df, n_sim = NULL, split = NULL, seed = NULL, output_format = "csv", output_path = NULL) {
  `%dopar%` <- foreach::`%dopar%`

  required_cols <- c("unique_id", "client_id", "duration", "mortality", "lapse", "nar", "rate")
  if (!all(required_cols %in% names(df))) {
    stop("Input data.frame must contain the columns: ", paste(required_cols, collapse = ", "))
  }

  if (is.null(n_sim)) {
    n_sim <- as.integer(readline(prompt = "Enter number of simulations per split: "))
  }
  if (is.null(split)) {
    split <- as.integer(readline(prompt = "Enter number of splits: "))
  }

  if (is.null(output_path)) {
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      output_path <- rstudioapi::selectDirectory(caption = "Select folder to save output files")
    } else {
      cat("Please enter the full path to the directory where you want to save the output files:\n")
      output_path <- readline(prompt = "Path: ")
    }
    if (!dir.exists(output_path)) {
      stop("The specified directory does not exist: ", output_path)
    }
  }

  result_path <- file.path(output_path, "result")
  if (!dir.exists(result_path)) {
    dir.create(result_path)
  }

  if (is.null(seed)) {
    seed <- as.integer(as.numeric(Sys.time())) %% .Machine$integer.max
    message("Seed not provided. Using generated seed: ", seed)
  }

  if (!(output_format %in% c("csv", "parquet"))) {
    stop("Output format must be either 'csv' or 'parquet'.")
  }
  if (!requireNamespace("arrow", quietly = TRUE) && output_format == "parquet") {
    stop("Package 'arrow' is required for Parquet output.")
  }

  MAX_DURATION <- max(df$duration)
  Number_of_POL <- nrow(dplyr::distinct(df, unique_id))
  Number_of_CLIENT <- nrow(dplyr::distinct(df, client_id))

  df_2 <- df |> dplyr::group_by(unique_id) |> dplyr::mutate(
    px = 1 - mortality,
    pw = 1 - lapse,
    tpx = dplyr::lag(cumprod(px), default = 1),
    tpw = dplyr::lag(cumprod(pw), default = 1),
    d = mortality * tpx,
    w = lapse * tpw
  ) |> dplyr::ungroup()

  DTH <- list()
  LAP <- list()
  NAR <- list()
  RATE <- list()
  POL_CLIENT_MAPPING <- list()

  for(i in 1:Number_of_POL){
    df_filtered <- df_2 |> dplyr::filter(unique_id == i)
    DTH[[i]] <- c(df_filtered$d, 1 - sum(df_filtered$d))
    LAP[[i]] <- c(df_filtered$w, 1 - sum(df_filtered$w))
    NAR[[i]] <- c(df_filtered$nar)
    RATE[[i]] <- c(df_filtered$rate)
    POL_CLIENT_MAPPING[[i]] <- as.integer(dplyr::distinct(df_filtered, client_id))
  }

  UPPER_TRI <- matrix(0, MAX_DURATION + 1, MAX_DURATION + 1)
  UPPER_TRI[upper.tri(UPPER_TRI, diag = TRUE)] <- 1

  set.seed(seed)
  seed_for_lapse <- matrix(as.integer(runif(Number_of_POL * split, min = 1, max = .Machine$integer.max)), nrow = split, byrow = TRUE)
  seed_for_death <- matrix(as.integer(runif(Number_of_CLIENT * split, min = 1, max = .Machine$integer.max)), nrow = split, byrow = TRUE)

  start_time <- Sys.time()
  is_check <- identical(Sys.getenv("_R_CHECK_LIMIT_CORES_"), "TRUE")
  cores <- if (is_check) 2 else parallel::detectCores()
  cl <- parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cl)
  parallel::clusterExport(cl, c("MAX_DURATION", "n_sim", "Number_of_POL",
                                "seed_for_death", "seed_for_lapse", "DTH", "LAP",
                                "POL_CLIENT_MAPPING", "UPPER_TRI", "NAR", "RATE",
                                "result_path", "output_format"), envir = environment())

  pb <- progress::progress_bar$new(
    format = "Calculating... [:bar] :elapsed | eta: :eta",
    total = split,
    width = 60
  )
  progress <- function(n){ pb$tick(tokens = list()) }
  opts <- list(progress = progress)

  result <- foreach::foreach(
    i = 1:split,
    .options.snow = opts,
    .packages = c("tidyverse","data.table", "readxl", "arrow", "stringr"),
    .inorder = FALSE
  ) %dopar% {

    TOTAL_PREM <- matrix(0, MAX_DURATION, n_sim)
    TOTAL_CLAIM <- matrix(0, MAX_DURATION, n_sim)

    for(number in 1:Number_of_POL){
      set.seed(seed_for_death[i, POL_CLIENT_MAPPING[[number]]])
      D <- rmultinom(n_sim, 1, DTH[[number]])
      set.seed(seed_for_lapse[i, number])
      W <- rmultinom(n_sim, 1, LAP[[number]])
      R <- (UPPER_TRI %*% D) * (UPPER_TRI %*% W)
      D <- D[-(MAX_DURATION + 1),]
      W <- W[-(MAX_DURATION + 1),]
      R <- R[-(MAX_DURATION + 1),]
      PREM <- R * NAR[[number]] * RATE[[number]]
      CLAIM <- R * D * (1 - W / 2) * NAR[[number]]
      TOTAL_PREM <- TOTAL_PREM + PREM
      TOTAL_CLAIM <- TOTAL_CLAIM + CLAIM
    }

    cf <- data.frame(
      split = i,
      sim_n = rep(1:n_sim, each = MAX_DURATION),
      DURATION = rep(1:MAX_DURATION, times = n_sim),
      PREM = as.vector(TOTAL_PREM),
      CLAIM = as.vector(TOTAL_CLAIM)
    )

    if(output_format == "csv"){
      data.table::fwrite(cf, stringr::str_c(result_path, "/", i, ".csv"))
    } else {
      arrow::write_parquet(cf, stringr::str_c(result_path, "/", i, ".parquet"))
    }
  }

  parallel::stopCluster(cl)
  end_time <- Sys.time()
  elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Log output
  log_text <- paste0(
    "Simulation starts at: ", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n",
    "Simulation run completed at: ", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n",
    "Seed used: ", seed, "\n",
    "Elapsed time (seconds): ", elapsed_time, "\n",
    "Number of simulation count: ", n_sim * split, "\n"
  )
  writeLines(log_text, con = file.path(output_path, "simulation_log.txt"))
  data.table::fwrite(df, file.path(output_path, "used_portfolio.csv"))
  message("Simulation completed successfully.")
}
