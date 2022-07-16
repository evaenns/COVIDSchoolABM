library(dplyr)
library(zoo)
library(tidygraph)
library(purrr)

# setting up the simulation
create_nodes <- function(input) {
  print("Creating students...")
  
  # giving the students hours
  nodes <- map_dfc(
    1:input$n_hours,
    function(x) sample(
      rep(1:input$n_classes, input$n_students/input$n_classes + 1), 
      input$n_students
    )
  )
  colnames(nodes) <- paste0("hour_", 1:input$n_hours)
  
  # putting the students into a lunch group
  nodes$lunch <- sample(
    rep(1:input$n_lunches, input$n_students/input$n_lunches + 1), 
    input$n_students
  )
  
  # masks and vaccination
  nodes$mask <- "none"
  nodes$mask[sample(
    1:input$n_students, 
    input$p_masked * input$n_students
  )] <- input$mask_type
  
  nodes$vax <- "two doses"
  nodes$vax[sample(
    1:input$n_students, 
    input$p_boosted * input$n_students
  )] <- "booster"
  
  print("Creating students... success!")
  return(nodes) 
}

create_edges <- function(input, nodes) { 
  print("Creating network...")
  
  # edges between all students who share a class
  edges_class <- map_dfr(
    1:input$n_hours,
    function(hour) map_dfr(
      1:input$n_classes,
      function(class) as_tibble(t(combn(
        which(nodes[paste0("hour_", hour)] == class),
        2
      )))
    )
  )
  colnames(edges_class) <- c("to", "from")
  edges_class$type <-  "class"
  edges_class$p_inf <- input$p_inf_class
  
  edges_lunch <- map_dfr(
    1:input$n_lunches,
    function(lunch) as_tibble(t(combn(
      which(nodes$lunch == lunch),
      2
    )))
  )
  colnames(edges_lunch) <- c("to", "from")
  edges_lunch$type <-  "lunch"
  edges_lunch$p_inf <- input$p_inf_lunch
  
  print("Creating network... success!")
  
  E <- bind_rows(edges_class, edges_lunch)
  return(E)
  
}