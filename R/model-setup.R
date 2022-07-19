library(dplyr)
library(purrr)

# setting up the simulation

create_school_net <- function(n_students, n_lunches, n_classes, n_hours) {
  
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
  
  # finished nodes, now edges
  
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
  
  edges <- bind_rows(edges_class, edges_lunch)
  
  return(list(nodes = nodes, edges = edges))
}