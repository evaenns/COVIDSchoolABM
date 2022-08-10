source("R/model-setup.R")

erhs_net <- create_school_net(
  n_students = 1954,
  n_lunches = 3,
  n_classes = 78,
  n_hours = 6,
  seed = 1659994788
)

source("R/model-run.R")

params <- list(
  d_latent = 4, 
  d_incubation = 6,
  d_immunity = 365,
  get_d_contag = function(n) {rgamma(n, shape = 4, rate = 0.75)},
  p_asymp = 0.405, 
  
  n_days = 79,
  start_day = 3,
  
  rate_inf = 0.0012,
  community_p_inf = "MN Washington County",
  I_0 = 1,
  
  eff_mask_S = 0.5,
  eff_mask_I = 0.75,
  eff_vax = 0.8
)

interv <- list(
  p_mask = 0.25,
  p_vax = 0.5,
  d_quarantine = 10,
  quarantine_contacts = F,
  test_period = -1 #never
)

print("start")
for (rate in 0:15 * 0.0001) {
  
  params$rate_inf <- rate
  
  sims <- run_sims(
    erhs_net, 
    500, 
    params, 
    interv, 
    seed = 1659994788,
    parallel = T,
    n_cores = 100
  )
  
  filename = paste0(
    "calibration/msi-calibration-20220808/2021T1-rate-",
    substr(format(rate, scientific = F, nsmall = 4), 3, 6),
    ".rds"
  )
  saveRDS(sims, file = filename)
}
print("finished")
