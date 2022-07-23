library(dplyr)
library(purrr)

# params is the list of interventions, plus number of days and infection probability
# 

run_sims <- function(school_net, n_sims, params, interv, print_msgs = F) {
  # masks and vaccination
  nodes <- school_net$nodes
  
  vec_mask <- rep(interv$eff_mask, nrow(nodes) * interv$p_mask)
  nodes$eff_mask <- c(vec_mask, rep(0, nrow(nodes) - length(vec_mask)))
  
  vec_vax <- rep(interv$eff_vax, nrow(nodes) * interv$p_vax)
  nodes$eff_vax <- c(vec_vax, rep(0, nrow(nodes) - length(vec_vax)))

  edges <- school_net$edges

  sim_results <- list()
  
  for (sim in 1:n_sims) {
    sim_name <- paste0("simulation_", sim)
    sim_results[[sim_name]] <- sim_agents(nodes, edges, params, interv)
    
    if (print_msgs) print(paste0("Simulation #", sim))
  }
  
  return(sim_results)
}

# a single trial of the simulation
sim_agents <- function(nodes, edges, params, interv) {
  
  out <- tibble(
    S = rep(0, params$n_days),
    E = rep(0, params$n_days),
    I = rep(0, params$n_days),
    R = rep(0, params$n_days),
    daily_cases = rep(0, params$n_days), # (test positive)
    daily_infs = rep(0, params$n_days)
  )
  mask_mandate <- F
  
  # initialize each student to be healthy and non quarantined
  nodes <- nodes %>% mutate(
    compartment = "S",
    quarantined = F,
    day_exposed = Inf,
    day_start_q = Inf,
    d_contag = 0
  )
  
  # community infection setup
  if (params$community_p_inf == "MN Washington County") {
    community_pr <- washington_data()
  } else {
    community_pr <- rep(params$community_p_inf, params$n_days)
  }
  
  # the days
  for (d in 1:params$n_days) {
    
    # introduce 1 new case (can change to one every week, etc)
    # can insert the community prevalence formula here
    if (d == 1) {
      inf_student <- sample(which(nodes$compartment == "S"), params$I_0)
      nodes$compartment[inf_student] <- "E"
      nodes$day_exposed[inf_student] <- d
    }
    
    # mask mandate
    if (interv$trigger_masks) {
      if (interv$trigger_var == 1) {
        check_p <- rollsum(out$daily_cases, 7, align = "right", fill = 0)[d]/nrow(nodes)
      }
      if (interv$trigger_var == 2) {
        check_p <- sum(nodes$quarantined)/nrow(nodes)
      }
      # print(check_p)
      mask_mandate <- check_p > interv$trigger_p
    }
    
    ## stage transitions HERE
    
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
    
    # Is/Ia -> R: Making students recover from COVID-19
    Isa_to_R <- nodes$compartment != "S" & d >= nodes$day_exposed + params$d_incubation + nodes$d_contag
    nodes$compartment[Isa_to_R] <- "R"
    
    # R -> S: Becoming susceptible again after recovery
    R_to_S <- nodes$compartment == "R" & d >= nodes$day_exposed + params$d_incubation + nodes$d_contag + params$d_immunity
    nodes$day_start_q[R_to_S] <- Inf
    nodes$compartment[R_to_S] <- "S"
    
    # regular testing
    # ASSUMPTION: every symptomatic person quarantines
    # tested_ids <- sample(which(!students$quarantined), 0 * sum(!students$quarantined))
    # students$quarantined[tested_ids] <- 
    #   substr(students$compartment[tested_ids], 1, 1) == "I" & 
    #   runif(length(tested_ids)) < 0.99 #sensitivity
    
    # entering/exiting quarantine
    # here we instantly enter quarantine and count a case
    # Here we assume that tests work perfectly
    # Here we use strictly greater than, based on CDC guidelines
    feeling_sick <- !nodes$quarantined & nodes$compartment == "Is" & d < nodes$day_start_q
    nodes$day_start_q[feeling_sick] <- d
    
    if (interv$testing_plan == "Weekly antigen" && d %% 7 == 1) {
      tested_cases <- 
        !nodes$quarantined & 
        nodes$compartment %in% c("Ip", "Is", "Ia") & 
        runif(nrow(nodes)) < sens_ant
      
      nodes$day_start_q[tested_cases] <- d
    } else if (interv$testing_plan == "Weekly PCR" && d %% 7 == 1) {
      tested_cases <- 
        !nodes$quarantined &
        nodes$compartment %in% c("Ip", "Is", "Ia") &
        runif(nrow(nodes)) < sens_pcr
      # One day turnaround
      nodes$day_start_q[tested_cases] <- d + 1
    } # else do nothing
    
    out$daily_cases[d] <- sum(d == nodes$day_start_q)
    nodes$quarantined[d == nodes$day_start_q] <- TRUE
    nodes$quarantined[d == nodes$day_start_q + interv$d_quarantine + 1] <- FALSE
    
    # S -> E this part is the actual transmission events, aka making S people into E
    if (d %% 7 %in% (2:6 - params$start_day)) {
      # if weekday
      edges_inf <- edges %>%
        filter(
          !nodes$quarantined[to] & !nodes$quarantined[from]
        ) %>%
        filter(
          nodes$compartment[to] == "S" & nodes$compartment[from] %in% c("Ip", "Is", "Ia")
          | nodes$compartment[to] %in% c("Ip", "Is", "Ia") & nodes$compartment[from] == "S"
        ) %>% 
        mutate(
          who_sus = if_else(
            nodes$compartment[to] == "S",
            to,
            from
          )
        ) %>%
        mutate(
          sus_rate_inf = params$rate_inf * (1 - nodes$eff_vax[who_sus]) *
            ifelse(
              mask_mandate,
              1 - interv$trigger_eff,
              if_else(type == "class", 1 - nodes$eff_mask[who_sus], 1)
            ),
          p_inf = 1 - exp(-sus_rate_inf * case_when(type == "class" ~ 1, type == "lunch" ~ 0.5) )
        )
      
      edges_inf <- edges_inf %>% 
        filter(
          runif(nrow(edges_inf)) < p_inf
        )
      
      new_infs <- unique(edges_inf$who_sus)
      nodes$compartment[new_infs] <- "E"
      nodes$day_exposed[new_infs] <- d
      nodes$d_contag[new_infs] <- params$get_d_contag(length(new_infs))
      out$daily_infs[d] <- length(new_infs)
    } else {
      # if weekend
      
      #TODO: add poiss
      outside_infs <- nodes$compartment == "S" & runif(nrow(nodes)) < community_pr[d]
      nodes$compartment[outside_infs] <- "E"
      nodes$day_exposed[outside_infs] <- d
      nodes$d_contag[outside_infs] <- params$get_d_contag(sum(outside_infs))
    }
    
    # recording data
    for (comp in c("S", "E", "I", "R")) {
      out[d, comp] <- sum(substr(nodes$compartment, 1, 1) == comp)
    }
    
    # end of day loop
  }
  
  return(out)
}

washington_data <- function() {
  case_data <- readRDS("data/time_series_mn_washington.rds")
  
  # 7 day avg daily cases
  # 600 is september 1st
  case_data <- (case_data[600:689]-case_data[593:682])/7
  
  # proportion of washington population
  # 2020 census
  case_data <- case_data/267568
  
  # inflate due to underreporting
  case_data <- case_data * 4
  
  return(as.numeric(case_data))
}