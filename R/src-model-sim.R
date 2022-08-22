library(dplyr)
library(purrr)
library(doRNG)
library(doParallel)

# This runs n_sims trials of the model and returns a list of time series 
#
# school_net: the contact network created from create_school_net in R/src-model-setup.R
#
# params is the list of parameters:
#   d_latent:        Time from exposure to infectiousness (in days) 
#   d_incubation:    Time from exposure to symptoms (in days)
#   d_immunity:      Length of immunity period after infection (in days)
#   get_d_contag:    Length of infection after symptoms (in days)
#                    A function that takes in one size argument
#   p_asymp:         The proportion of COVID-19 infections that are asymptomatic (0-1)
#   
#   n_days:          The number of days to run the model
#   start_day:       The day of the week to start (Sunday = 0)
#
#   rate_inf:        The hourly rate of infection between one susceptible and one 
#                    infectious student.
#   community_p_inf: The daily case rate per capita in the community (0-1)
#   I_0:             The number of initially infected students
#
#   eff_mask_S:      Mask wearer protection effectiveness against transmission (0-1)
#   eff_mask_I:      Mask source control effectiveness against transmission (0-1)
#   eff_vax:         Vaccine effectiveness against transmission (0-1)
#
#   test_sens:       Test sensitivity (0-1)
#   test_spec:       Test specificity (0-1)
#
# interv is the list of interventions:
#   p_mask:              The proportion of students that are masked
#   p_vax:               The proportion of students that are vaccinated
#   d_quarantine:        The length of isolation/quarantine (in days)
#   d_mask_after:        The length of mask wearing after quarantine (in days)
#   quarantine_contacts: Whether or not to quarantine close contacts (boolean)
#   test_to_stay:        Whether unvaccinated close contacts will test (boolean)
#   test_period:         Time between tests (e.g. 7 for weekly)

run_sims <- function(school_net, n_sims, params, interv, seed = Sys.time(), parallel = F, n_cores = 1) {
  if (params$d_incubation < params$d_latent) stop("Incubation period must be longer than latent period.")
  if (sum(interv$quarantine_contacts, interv$test_to_stay, interv$test_period != -1) > 1) warning(
    "Are you sure you want more than one of the following interventions?\n - Quarantining close contacts\n - Test to stay\n - Periodic testing"
  )
  
  set.seed(seed) 
  
  nodes <- school_net$nodes
  
  # randomly assign masks and vaccines, which will stay fixed for these trials
  nodes$mask <- F
  nodes$mask[sample(1:nrow(nodes), interv$p_mask * nrow(nodes))] <- T
  
  nodes$vax <- F
  nodes$vax[sample(1:nrow(nodes), interv$p_vax * nrow(nodes))] <- T
  
  edges <- school_net$edges

  if (parallel) {
    # parallel implementation of trial runs for speed
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    print(cl)
    
    sim_results <- foreach(
      sim = 1:n_sims, 
      .export = c(
        "sim_agents",
        "washington_data",
        "get_close_contacts",
        "do_tests"
      ), 
      .packages = "dplyr"
    ) %dorng% {
      sim_agents(nodes, edges, params, interv)
    }
    
    stopCluster(cl)
  } else {
    # running n_sims trials without parallel computation
    sim_results <- list()
    
    for (sim in 1:n_sims) {
      sim_name <- paste0("simulation_", sim)
      sim_results[[sim_name]] <- sim_agents(nodes, edges, params, interv)
    }
  }
  
  return(sim_results)
}

# Runs a single trial and returns a dataframe containing time series by day of 
# Susceptible, Exposed, Infected, Recovered, COVID cases (detected), COVID 
# infections (that happened in school), and days of in-person school lost.

sim_agents <- function(nodes, edges, params, interv) {

  out <- tibble(
    S = rep(0, params$n_days),
    E = rep(0, params$n_days),
    Ip = rep(0, params$n_days),
    Is = rep(0, params$n_days),
    Ia = rep(0, params$n_days),
    R = rep(0, params$n_days),
    daily_cases = rep(0, params$n_days),
    daily_infs = rep(0, params$n_days),
    in_school_Is = rep(0, params$n_days),
    out_of_school = rep(0, params$n_days),
    tests_used = rep(0, params$n_days)
  )
  
  # initialize each student to be healthy, non quarantined, etc.
  nodes <- nodes %>% mutate(
    compartment = "S",
    quarantined = F,
    confirmed = F,
    day_exposed = -Inf,
    symptoms_start = -Inf,
    tts_start = -Inf,
    q_start = -Inf,
    d_contag = 0
  )
  
  # community infection setup
  if (params$community_p_inf == "MN Washington County") {
    community_pr <- washington_data("9/1/21", params)
  } else {
    community_pr <- rep(params$community_p_inf, params$n_days)
  }
  
  for (d in 1:params$n_days) {
    
    # move a number of students into Exposed to start
    if (d == 1) {
      inf_student <- sample(which(nodes$compartment == "S"), params$I_0)
      nodes$compartment[inf_student] <- "E"
      nodes$day_exposed[inf_student] <- d
      nodes$d_contag[inf_student] <- params$get_d_contag(params$I_0)
    }
    
    # All timed stage transitions below ----------------------------------------
    
    # E -> Ip: Exposed students become infectious after latent period
    E_to_Ip <- nodes$compartment == "E" & d >= nodes$day_exposed + params$d_latent
    nodes$compartment[E_to_Ip] <- "Ip"
    
    # Ip -> Is/Ia: Some people start to develop symptoms after incubation period
    Ip_to_Isa <- 
      nodes$compartment == "Ip" & d >= nodes$day_exposed + params$d_incubation
    nodes$compartment[Ip_to_Isa] <- if_else(
      runif(sum(Ip_to_Isa)) < params$p_asymp,
      "Ia",
      "Is"
    )
    nodes$symptoms_start[Ip_to_Isa & nodes$compartment == "Is"] <- d
    
    # Is/Ia -> R: Making students recover from COVID-19
    Isa_to_R <- nodes$compartment %in% c("Ip", "Is", "Ia") & d >= nodes$day_exposed + params$d_latent + nodes$d_contag
    nodes$compartment[Isa_to_R] <- "R"
    
    # R -> S: Becoming susceptible again after recovery
    R_to_S <- nodes$compartment == "R" & d >= nodes$day_exposed + params$d_latent + nodes$d_contag + params$d_immunity
    nodes$compartment[R_to_S] <- "S"
    
    # All testing and quarantining interventions -------------------------------
    
    # People testing after they have symptoms (2 spaced 48 hrs apart, FDA)
    nodes$q_start[d == nodes$symptoms_start] <- d
    home_tests <- which(
      nodes$symptoms_start %in% c(d, d - 2)
      & !nodes$confirmed
    )
    home_cases <- do_tests(nodes, params, home_tests)
    nodes$confirmed[home_cases] <- T
  
    # School mandated tests - TTS or periodic testing
    school_tests <- integer(0)
    if (d %% interv$test_period == 1) {
      school_tests <- 1:nrow(nodes)
    }
    if (interv$test_to_stay) {
      school_tests <- which(nodes$tts_start %in% c(d, d - 2, d - 4))
    }
    school_tests <- 
      school_tests[!nodes$quarantined[school_tests] & d != nodes$symptoms_start[school_tests]]
    school_cases <- do_tests(nodes, params, school_tests)
    nodes$confirmed[school_cases] <- T
    nodes$q_start[school_cases] <- d 
    
    # Handling stuff for all positive cases (both @home and @school)
    cases <- union(home_cases, school_cases)
    contacts <- get_close_contacts(nodes, edges, cases)
    if (interv$quarantine_contacts) {
      nodes$q_start[contacts] <- d + 1
    }
    if (interv$test_to_stay) {
      nodes$tts_start[contacts] <- d + 1
    }
    
    # Entering and exiting quarantine
    nodes$quarantined[d == nodes$q_start] <- TRUE
    nodes$quarantined[d == nodes$q_start + interv$d_quarantine] <- FALSE
    nodes$confirmed[d == nodes$q_start + interv$d_quarantine] <- F
    
    # Logging
    out$daily_cases[d] <- length(cases)
    out$tests_used[d] <- length(union(home_tests, school_tests))
    
    # S -> E this part is the actual transmission ------------------------------
    if (d %% 7 %in% ((2:6 - params$start_day) %% 7) ) {
      
      # In-school transmission if it's a weekday ===============================
      edges_inf <- edges %>%
        filter(
          # Ignore students in quarantine
          !nodes$quarantined[to] & !nodes$quarantined[from]
        ) %>%
        filter(
          # Choose only edges from I -> S or S <- I
          nodes$compartment[to] == "S" & nodes$compartment[from] %in% c("Ip", "Is", "Ia")
          | nodes$compartment[to] %in% c("Ip", "Is", "Ia") & nodes$compartment[from] == "S"
        ) %>% 
        mutate(
          # Find out who's on giving/receiving end this time
          who_S = if_else(
            nodes$compartment[to] == "S",
            to,
            from
          ),
          who_I = if_else(
            nodes$compartment[to] == "S",
            from,
            to
          ),
          # Add modifiers
          true_rate_inf = params$rate_inf * (
            1 - params$eff_vax * nodes$vax[who_S]
          ),

          true_rate_inf = true_rate_inf * (
            1 - params$eff_mask_S * (type != "lunch")
            * (nodes$mask[who_S] | d < nodes$q_start[who_S] + interv$d_quarantine + interv$d_mask_after)
          ),
          true_rate_inf = true_rate_inf * (
            1 - params$eff_mask_I * (type != "lunch")
            * (nodes$mask[who_I] | d < nodes$q_start[who_I] + interv$d_quarantine + interv$d_mask_after)
          ),
          # Convert rate to probability
          p_inf = 1 - exp(
            -true_rate_inf * case_when(
              type == "class" ~ 1, type == "lunch" ~ 1/3
            ) 
          )
        )
      
      # Flip the coins, eliminate those who didn't get infected
      edges_inf <- edges_inf %>% filter(runif(nrow(edges_inf)) < p_inf)
      
      # Move new infections to the Exposed compartment
      new_infs <- unique(edges_inf$who_S)
      nodes$compartment[new_infs] <- "E"
      nodes$day_exposed[new_infs] <- d
      nodes$d_contag[new_infs] <- params$get_d_contag(length(new_infs))
      out$daily_infs[d] <- length(new_infs)
      
      out$in_school_Is[d] <- sum(!nodes$quarantined & nodes$compartment == "Is")
      out$out_of_school[d] <- sum(nodes$quarantined)
    } else {
      # Introduce outside infections if it's a weekend =========================
      susceptibles <- which(nodes$compartment == "S")
      outside_infs <- sample(susceptibles, round(length(susceptibles) * community_pr[d]))

      nodes$compartment[outside_infs] <- "E"
      nodes$day_exposed[outside_infs] <- d
      nodes$d_contag[outside_infs] <- params$get_d_contag(length(outside_infs))
      
      out$in_school_Is[d] <- 0
      out$out_of_school[d] <- 0
    }
    
    # Recording SEIR time series -----------------------------------------------
    for (comp in c("S", "E", "Ip", "Is", "Ia", "R")) {
      out[d, comp] <- sum(nodes$compartment == comp)
    }
    
    
    # End of the day -----------------------------------------------------------
  }
  
  return(out)
}

# This does the data crunching for the fall 2021 scenario (calibration)

washington_data <- function(start_date, params) {
  case_data <- readRDS("data/time_series_mn_washington.rds")
  
  a <- which(names(case_data) == start_date) + params$d_incubation
  b <- a + params$n_days - 1
  
  # 7 day avg daily cases
  # 600 is september 1st
  case_data <- case_data[(a + 3):(b + 3)] - case_data[(a - 4):(b - 4)]
  case_data <- case_data / 7
  
  # proportion of washington population
  # 2020 census
  case_data <- case_data / 267568
  
  # inflate due to underreporting
  case_data <- case_data * 4
  
  return(as.numeric(case_data))
}

# this will be used to determine close contacts for quarantining purposes

get_close_contacts <- function(nodes, edges, ids) {
  edges_q <- edges %>% 
    filter(
      type == "class"
      & xor(to %in% ids, from %in% ids)
    ) %>% 
    mutate(
      contacts = if_else(
        to %in% ids,
        from,
        to
      )
    ) %>%
    filter(!nodes$quarantined[contacts])
  return(unique(edges_q$contacts))
}

do_tests <- function(nodes, params, ids) {
  tp <- 
    nodes$compartment[ids] %in% c("Ip", "Is", "Ia") & 
    runif(length(ids)) < params$test_sens
  fp <- 
    nodes$compartment[ids] %in% c("S", "E", "R") & 
    runif(length(ids)) < 1 - params$test_spec
  
  return(ids[tp | fp])
}