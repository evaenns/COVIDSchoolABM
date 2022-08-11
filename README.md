# covid-school-agent-model

How to use this:

1. Source `R/src-model-setup.R` and `R/src-model-sim.R`
2. Create a contact network:

    ```R
    my_network <- create_school_net(n_students = 1000, ...)
    ```
  
3. Set up your parameters and interventions:

    ```R
    params <- list(
      d_latent = 4,
      ...
    )
    interv <- list(
    p_mask = 0.10,
      ...
    )
    ```
  
4. Run your trials

    ```R
    simulations <- run_sims(
      my_network,
      50,
      params,
      interv,
      parallel = F
    )
    ```
    
5. `simulations` will be a list containing dataframes for each trial:

    ```R
    $simulation_1
    # A tibble: 100 x 7
           S     E     I     R daily_cases daily_infs learning_lost
       <dbl> <dbl> <dbl> <dbl>       <dbl>      <dbl>         <dbl>
     1  1953     1     0     0           0          0             0
     2  1953     1     0     0           0          0             0
     3  1953     1     0     0           0          0             0
    # ... with 97 more rows
    
    $simulation_2
    # A tibble: 100 x 7
           S     E     I     R daily_cases daily_infs learning_lost
       <dbl> <dbl> <dbl> <dbl>       <dbl>      <dbl>         <dbl>
     1  1953     1     0     0           0          0             0
     2  1953     1     0     0           0          0             0
     3  1953     1     0     0           0          0             0
    # ... with 97 more rows
    
    $simulation_3
    # A tibble: 100 x 7
           S     E     I     R daily_cases daily_infs learning_lost
       <dbl> <dbl> <dbl> <dbl>       <dbl>      <dbl>         <dbl>
     1  1953     1     0     0           0          0             0
     2  1953     1     0     0           0          0             0
     3  1953     1     0     0           0          0             0
    # ... with 97 more rows
    ```