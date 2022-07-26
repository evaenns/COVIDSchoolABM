rm(list=ls())

set.seed(20220901)

source("R/model-setup.R")

erhs_net <- create_school_net(
  n_students = 1954,
  n_lunches = 3,
  n_classes = 79,
  n_hours = 6
)

source("R/model-run.R")

params <- list(
  d_latent = 4, 
  d_incubation = 6,
  d_immunity = 365, 
  p_asymp = 0.405, 
  n_days = 150,
  start_day = 3,
  rate_inf = 0.0012,
  community_p_inf = "MN Washington County",
  
  I_0 = 1,
  
  get_d_contag = function(n) {rgamma(n, shape = 4, rate = 0.75)}
)

interv <- list(
  p_mask = 0.2,
  eff_mask = 0.66,
  
  p_vax = 0.6,
  eff_vax = 0.8,
  
  d_quarantine = 10,
  trigger_masks = F,
  testing_plan = "none"
)

results <- tibble(
  rates = c(0.0000, 0.0010, 0.0011, 0.0012, 0.0013, 0.0014),
  MSE = rep(0,6),
  total_cases = rep(0,6)
)
load("data/sowashco-dashboard.RData")
  
erhs_observed <- sowashco_dashboard$high[1:10] * 1954 / (1954 + 1972 + 1975)
  
for (k in 1:6) {
  params$rate_inf <- results$rates[k]
  
  sims <- run_sims(erhs_net, 50, params, interv, print_msgs = T)
  save(sims, file = paste0("calibration/erhs-rate-", params$rate_inf, ".RData"))
  
  avg <- rowMeans(sapply(sims, function(x){x$daily_cases}))
  
  erhs_simulated <- c(
    sum(avg[1:17]),
    sum(avg[18:23]),
    sum(avg[24:30]),
    sum(avg[31:37]),
    sum(avg[38:44]),
    sum(avg[45:51]),
    sum(avg[52:58]),
    sum(avg[59:65]),
    sum(avg[66:72]),
    sum(avg[73:79])
  )
  
  SE <- (erhs_simulated - erhs_observed)^2
  WSE <- (SE * c(17,6,7,7,7,7,7,7,7,7))/79
  results$WMSE[k] <- sum(WSE)
  results$total_cases[k] <- sum(avg)
  
  
}