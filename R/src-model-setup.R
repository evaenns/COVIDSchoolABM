library(dplyr)
library(purrr)

# This creates the school contact network and retuns a list of size 2 containing
# the node and edge lists
#
# n_students: The number of students in school
# n_lunches:  The number of lunch cohorts
# n_classes:  The number of classes running at any given time.
#             Students will be assigned to classes of roughly equal size
# n_hours:    The number of classes any student will attend in the day

create_school_net <- function(n_students, n_lunches, n_classes, n_hours, seed = Sys.time()) {
  set.seed(seed)
  
  # assigning students random classes for every hour
  nodes <- map_dfc(
    1:n_hours,
    function(x) sample(
      rep(1:n_classes, n_students/n_classes + 1), 
      n_students
    )
  )
  colnames(nodes) <- paste0("hour_", 1:n_hours)
  
  # assigning students a random lunch group
  nodes$lunch <- sample(
    rep(1:n_lunches, n_students/n_lunches + 1), 
    n_students
  )
  
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
  
  edges_lunch <- map_dfr(
    1:n_lunches,
    function(lunch) as_tibble(t(combn(
      which(nodes$lunch == lunch),
      2
    )))
  )
  colnames(edges_lunch) <- c("to", "from")
  edges_lunch$type <-  "lunch"
  
  edges <- bind_rows(edges_class, edges_lunch)
  
  return(list(nodes = nodes, edges = edges))
}