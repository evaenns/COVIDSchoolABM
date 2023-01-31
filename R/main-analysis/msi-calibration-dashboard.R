source("R/src-model-setup.R")

erhs_net <- create_school_net(
  n_students = 1954,
  n_lunches = 3,
  n_classes = 78,
  n_hours = 6,
  seed = 1659994788
)

source("R/src-model-sim.R")

params <- list(
  d_latent = 4, 
  d_incubation = 6,
  d_immunity = 365,
  get_d_contag = function(n) {
    sample(1:15, n, prob = c(0, 0, 0.0620, 0.1887, 0.4066, 0.1242, 0.0316, 0.0923, 0, 0.0317, 0, 0.0629, 0, 0, 0), replace = T)
  },
  p_asymp = 0.405, 
  
  n_days = 79,
  start_day = 3,
  
  rate_inf = 0.0012,
  community_p_inf = "MN Washington County",
  I_0 = 1,
  
  eff_mask_S = 0.5,
  eff_mask_I = 0.75,
  eff_vax = 0.8,
  
  test_sens = 0.63,
  test_spec = 0.998
)

interv <- list(
  p_mask = 0.25,
  p_vax = 0.5,
  d_quarantine = 10,
  d_mask_after = 0,
  quarantine_contacts = F,
  test_to_stay = F,
  test_period = -1
)

# Finished setting up everything to model fall 2021 trimester w/ Delta variant

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
    "output/20220821/2021T1-rate-",
    substr(format(rate, scientific = F, nsmall = 4), 3, 6),
    ".rds"
  )
  saveRDS(sims, file = filename)
}
print("finished")
