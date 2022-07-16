library(dplyr)
library(zoo)
library(tidygraph)
library(purrr)

# the simulation
seir_simulate <- function(input, nodes, edges) {
  
  out <- tibble(
    S = rep(0, input$n_days),
    E = rep(0, input$n_days),
    I = rep(0, input$n_days),
    R = rep(0, input$n_days),
    daily_cases = rep(0, input$n_days), # (test positive)
    daily_infs = rep(0, input$n_days)
  )
  mask_mandate <- F
  
  # initialize each student to be healthy and non quarantined
  nodes <- nodes %>% mutate(
    compartment = "S",
    quarantined = F,
    day_exposed = Inf,
    day_start_q = Inf,
    days_contag = 0
  )
  
  # community infection setup
  if (input$community_p_inf == "MN Washington County") {
    community_pr <- washington_data()
  } else {
    community_pr <- rep(input$community_p_inf, input$n_days)
  }
  
  # the days
  for (d in 1:input$n_days) {
    
    # introduce 1 new case (can change to one every week, etc)
    # can insert the community prevalence formula here
    if (d == 1) {
      inf_student <- sample(which(nodes$compartment == "S"), 1)
      nodes$compartment[inf_student] <- "E"
      nodes$day_exposed[inf_student] <- d
    }
    
    # mask mandate
    if (input$triggered_masks) {
      if (input$trigger_var == 1) {
        check_p <- rollsum(out$daily_cases, 7, align = "right", fill = 0)[d]/input$n_students
      }
      if (input$trigger_var == 2) {
        check_p <- sum(nodes$quarantined)/input$n_students
      }
      # print(check_p)
      mask_mandate <- check_p > input$trigger_p
    }
    
    ## stage transitions HERE
    
    # E -> Ip: Exposed students become infectious after latent period
    E_to_Ip <- nodes$compartment == "E" & d >= nodes$day_exposed + input$d_latent
    nodes$compartment[E_to_Ip] <- "Ip"
    
    # Ip -> Is/Ia: Some people start to develop symptoms after incubation period
    Ip_to_Isa <- 
      nodes$compartment == "Ip" & d >= nodes$day_exposed + input$d_incubation
    nodes$compartment[Ip_to_Isa] <- if_else(
      runif(sum(Ip_to_Isa)) < input$p_asymp,
      "Ia",
      "Is"
    )
    
    # Is/Ia -> R: Making students recover from COVID-19
    Isa_to_R <- nodes$compartment != "S" & d >= nodes$day_exposed + input$d_incubation + nodes$days_contag
    nodes$compartment[Isa_to_R] <- "R"
    
    # R -> S: Becoming susceptible again after recovery
    R_to_S <- nodes$compartment == "R" & d >= nodes$day_exposed + input$d_incubation + nodes$days_contag + input$d_immunity
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
    
    if (input$testing_plan == "Weekly antigen" && d %% 7 == 1) {
      tested_cases <- 
        !nodes$quarantined & 
        nodes$compartment %in% c("Ip", "Is", "Ia") & 
        runif(input$n_students) < sens_ant
      
      nodes$day_start_q[tested_cases] <- d
    } else if (input$testing_plan == "Weekly PCR" && d %% 7 == 1) {
      tested_cases <- 
        !nodes$quarantined &
        nodes$compartment %in% c("Ip", "Is", "Ia") &
        runif(input$n_students) < sens_pcr
      # One day turnaround
      nodes$day_start_q[tested_cases] <- d + 1
    } # else do nothing
    
    out$daily_cases[d] <- sum(d == nodes$day_start_q)
    nodes$quarantined[d == nodes$day_start_q] <- TRUE
    nodes$quarantined[d == nodes$day_start_q + input$d_quarantine + 1] <- FALSE
    
    # S -> E this part is the actual transmission events, aka making S people into E
    if (d %% 7 %in% 1:5) {
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
          true_p_inf = p_inf * (1 - eff_vax(nodes$vax[who_sus])) * ifelse(
            mask_mandate,
            1 - eff_mask(input$mask_type),
            if_else(
              type == "class",
              1 - eff_mask(nodes$mask[who_sus]),
              1
            )
          )
        ) 
      edges_inf <- edges_inf %>% 
        filter(
          runif(nrow(edges_inf)) < true_p_inf
        )
      
      
      new_infs <- unique(edges_inf$who_sus)
      nodes$compartment[new_infs] <- "E"
      nodes$day_exposed[new_infs] <- d
      nodes$days_contag[new_infs] <- get_d_contagious(length(new_infs))
      out$daily_infs[d] <- length(new_infs)
    } else {
      # if weekend
      
      #TODO: add poiss
      outside_infs <- nodes$compartment == "S" & runif(input$n_students) < community_pr[d]
      nodes$compartment[outside_infs] <- "E"
      nodes$day_exposed[outside_infs] <- d
      nodes$days_contag[outside_infs] <- get_d_contagious(sum(outside_infs))
    }
    
    # recording data
    for (comp in c("S", "E", "I", "R")) {
      out[d, comp] <- sum(substr(nodes$compartment, 1, 1) == comp)
    }
    
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
Footer