library(dplyr)
library(purrr)
library(doRNG)
library(doParallel)

# params is the list of interventions, plus number of days and infection probability

run_sims <- function(school_net, n_sims, params, interv, seed = Sys.time(), parallel = F, n_cores = 1) {
  set.seed(seed)
  
  # masks and vaccination
  nodes <- school_net$nodes
  
  nodes$mask <- F
  nodes$mask[sample(1:nrow(nodes), interv$p_mask * nrow(nodes))] <- T
  
  nodes$vax <- F
  nodes$vax[sample(1:nrow(nodes), interv$p_vax * nrow(nodes))] <- T
  
  edges <- school_net$edges

  if (parallel) {
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    print(cl)
    
    sim_results <- foreach(
      sim = 1:n_sims, 
      .export = c("sim_agents", "washington_data"), 
      .packages = "dplyr"
    ) %dorng% {
      sim_agents(nodes, edges, params, interv)
    }
    
    stopCluster(cl)
  } else {
    sim_results <- list()
    
    for (sim in 1:n_sims) {
      sim_name <- paste0("simulation_", sim)
      sim_results[[sim_name]] <- sim_agents(nodes, edges, params, interv)
    }
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
    daily_infs = rep(0, params$n_days),
    learning_lost = rep(0, params$n_days)
  )
  
  # initialize each student to be healthy and non quarantined
  nodes <- nodes %>% mutate(
    compartment = "S",
    quarantined = F,
    day_exposed = Inf,
    q_start = Inf,
    d_contag = 0
  )
  
  # community infection setup
  if (params$community_p_inf == "MN Washington County") {
    community_pr <- washington_data("9/1/21", params)
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
    Isa_to_R <- nodes$compartment %in% c("Is", "Ia") & d >= nodes$day_exposed + params$d_incubation + nodes$d_contag
    nodes$compartment[Isa_to_R] <- "R"
    
    # R -> S: Becoming susceptible again after recovery
    R_to_S <- nodes$compartment == "R" & d >= nodes$day_exposed + params$d_incubation + nodes$d_contag + params$d_immunity
    nodes$q_start[R_to_S] <- Inf
    nodes$compartment[R_to_S] <- "S"
    
    # entering/exiting quarantine
    # here we instantly enter quarantine and count a case
    # Here we assume that tests work perfectly
    # Here we use strictly greater than, based on CDC guidelines
    feeling_sick <- !nodes$quarantined & nodes$compartment == "Is" & d < nodes$q_start
    nodes$q_start[feeling_sick] <- d
    out$daily_cases[d] <- out$daily_cases[d] + sum(feeling_sick)
    
    if (interv$quarantine_contacts) {
      contacts <- get_close_contacts(nodes, edges, which(feeling_sick))
      nodes$q_start[contacts] <- d
    }
    
    if (d %% interv$test_period == 1) {
      tp <- 
        !nodes$quarantined & 
        nodes$compartment %in% c("Ip", "Is", "Ia") & 
        runif(nrow(nodes)) < params$test_sens
      fp <- 
        !nodes$quarantined & 
        nodes$compartment %in% c("S", "E") & 
        runif(nrow(nodes)) < 1 - params$test_spec
      
      nodes$q_start[tp | fp] <- d + params$test_delay
      if (d < params$n_days) {
        out$daily_cases[d + params$test_delay] <-
        out$daily_cases[d + params$test_delay] + sum(tp | fp)
      }
      
      if (interv$quarantine_contacts) {
        contacts <- get_close_contacts(nodes, edges, which(tp | fp))
        nodes$q_start[contacts] <- d + params$test_delay 
      }
    }
    
    
    nodes$quarantined[d == nodes$q_start] <- TRUE
    nodes$quarantined[d == nodes$q_start + interv$d_quarantine] <- FALSE
    
    # S -> E this part is the actual transmission events, aka making S people into E
    if (d %% 7 %in% ((2:6 - params$start_day) %% 7) ) {
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
          true_rate_inf = params$rate_inf * 
            (1 - params$eff_mask_S * nodes$mask[who_S] * (type != "lunch") ) *
            (1 - params$eff_mask_I * nodes$mask[who_I] * (type != "lunch") ) *
            (1 - params$eff_vax * nodes$vax[who_S]),
          p_inf = 1 - exp(-true_rate_inf * case_when(type == "class" ~ 1, type == "lunch" ~ 1/3) )
        )
      
      edges_inf <- edges_inf %>% filter(runif(nrow(edges_inf)) < p_inf)
      
      new_infs <- unique(edges_inf$who_S)
      nodes$compartment[new_infs] <- "E"
      nodes$day_exposed[new_infs] <- d
      nodes$d_contag[new_infs] <- params$get_d_contag(length(new_infs))
      out$daily_infs[d] <- length(new_infs)
      
      out$learning_lost[d] <- sum(nodes$quarantined)
    } else {
      # if weekend
      
      susceptibles <- which(nodes$compartment == "S")
      outside_infs <- sample(susceptibles, round(length(susceptibles) * community_pr[d]))

      nodes$compartment[outside_infs] <- "E"
      nodes$day_exposed[outside_infs] <- d
      nodes$d_contag[outside_infs] <- params$get_d_contag(length(outside_infs))
      
      out$learning_lost[d] <- 0
    }
    
    # recording data
    for (comp in c("S", "E", "I", "R")) {
      out[d, comp] <- sum(substr(nodes$compartment, 1, 1) == comp)
    }
    
    
    # end of day loop
  }
  
  return(out)
}

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