library(dplyr)
library(purrr)

# setting up the simulation

create_nodes <- function(n_students, n_lunches, n_classes, n_hours) {
  
  # giving the students hours
  nodes <- map_dfc(
    1:n_hours,
    function(x) sample(
      rep(1:n_classes, n_students/n_classes + 1), 
      n_students
    )
  )
  colnames(nodes) <- paste0("hour_", 1:n_hours)
  
  # putting the students into a lunch group
  nodes$lunch <- sample(
    rep(1:n_lunches, n_students/n_lunches + 1), 
    n_students
  )
  
  # masks and vaccination
  # nodes$mask <- "none"
  # nodes$mask[sample(
  #   1:n_students, 
  #   p_masked * n_students
  # )] <- mask_type
  # 
  # nodes$vax <- "two doses"
  # nodes$vax[sample(
  #   1:n_students, 
  #   p_boosted * n_students
  # )] <- "booster"
  
  return(nodes) 
}

create_edges <- function(n_students, n_lunches, n_classes, n_hours, nodes) { 
  print("Creating network...")
  
  # edges between all students who share a class
  edges_class <- map_dfr(
    1:n_hours,
    function(hour) map_dfr(
      1:n_classes,
      function(class) as_tibble(t(combn(
        which(nodes[paste0("hour_", hour)] == class),
        2
      )))
    )
  )
  colnames(edges_class) <- c("to", "from")
  edges_class$type <-  "class"
  # edges_class$p_inf <- p_inf_class
  
  edges_lunch <- map_dfr(
    1:n_lunches,
    function(lunch) as_tibble(t(combn(
      which(nodes$lunch == lunch),
      2
    )))
  )
  colnames(edges_lunch) <- c("to", "from")
  edges_lunch$type <-  "lunch"
  # edges_lunch$p_inf <- p_inf_lunch
  
  print("Creating network... success!")
  
  E <- bind_rows(edges_class, edges_lunch)
  return(E)
}