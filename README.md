
# volrisk

The **volrisk** package provides simulation tools for modeling
stochastic cash flows in life reinsurance contracts with **profit
commission** provisions.

## Features

- Create insurance portfolios with validation
- Simulate death and lapse events under stochastic assumptions
- Calculate profit commissions from simulated cash flows
- Quantify risk using Value-at-Risk (VaR) and Tail Value-at-Risk (TVaR)

## Installation

``` r
# Install from GitHub
devtools::install_github("taku1094/volrisk")
library(volrisk)
```

## Example Usage

### 1. Generate Portfolio

``` r
port <- make_portfolio(example_portfolio, cols = list(
  unique_id = "POL_ID",
  client_id = "CLIENT_ID",
  duration = "DURATION",
  mortality = "MORTALITY",
  lapse = "LAPSE",
  nar = "NAR",
  rate = "RATE"
))
```

### 2. Run Simulation

``` r
simulation(
  port, 
  n_sim = 10, 
  split = 100,
  seed = 12345,
  output_format = "csv", 
  output_path = tempdir()
)
```

### 3. Calculate Profit Commission

``` r
data(example_simulation)

example_simulation_with_PC <- example_simulation %>%
  dplyr::group_by(split, sim_n) %>%
  dplyr::mutate(
    PC = calc_pc(PREM, CLAIM,
                 pc_rate = 0.9,
                 me = 0.05,
                 loss_carry = "N",
                 duration = DURATION)
  ) %>%
  dplyr::ungroup()
```

### 4. Evaluate Risk

``` r
result <- risk(
  example_simulation,
  time_horizon = c(1, 10),
  level = c(0.01, 0.99),
  discount = 0.02,
  output = "BAL"
)
```
