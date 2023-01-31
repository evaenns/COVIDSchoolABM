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
  d_latent = 1, 
  d_incubation = 3,
  d_immunity = 365,
  get_d_contag = function(n) {
    sample(1:15, n, prob = c(0, 0.0300, 0.1184, 0.1461, 0.1764, 0.0290, 0.1480, 0.0871, 0, 0.0889, 0.1179, 0.0290, 0, 0, 0.0292), replace = T)
  },
  p_asymp = 0.405, 
  
  n_days = 77,
  start_day = 3,
  
  rate_inf = 0.0007 * 4, 
  community_p_inf = 0.001,
  I_0 = 1,
  
  eff_mask_S = 0.5,
  eff_mask_I = 0.75,
  eff_vax = 0.4,
  
  test_sens = 0.63,
  test_spec = 0.998
)

interv <- list(
  p_mask = 0.25,
  p_vax = 0.25,
  d_quarantine = 5,
  d_mask_after = 0,
  quarantine_contacts = F,
  test_to_stay = F,
  test_period = -1
)

# sep 27 scenario runs

for (m in c(0.1, 1)) {
  interv$p_mask <- m
  
  for (v in c(0.25, 0.5)) {
    interv$p_vax <- v
    
    for (q_length in 1:3) {
      interv$d_quarantine <- c(5, 5, 10)[q_length]
      interv$d_mask_after <- c(0, 5, 0)[q_length]
      
      for (strategy in 1:4) {
        interv$test_period <- c(-1, 7, -1, -1)[strategy]
        interv$test_to_stay <- c(F, F, T, F)[strategy]
        interv$quarantine_contacts <- c(F, F, F, T)[strategy]
        
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
          "output/20221211/mask",
          100 * m,
          "-boost",
          100 * v,
          c("-5day", "-5plus5", "-10day")[q_length],
          c("-none", "-weekly", "-tts", "-quarantine")[strategy],
          ".rds"
        )
        
        saveRDS(sims, file = filename)
      }
    }
  }
}

