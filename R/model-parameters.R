# Will be deleted soon, just for my own reference

def_n_students <- 1800
def_n_classes <- 80 # number of classrooms
def_n_lunches <- 3 # lunches are unused so far

def_n_hours <- 6 # hours/class periods in a day

# Simulation
def_n_days <- 90 #try longer amounts of time, used to be 100
def_n_trials <- 1 #run more sims

#time to peak, peak location, total cases
# percent of sims that pass a threshold

# COVID parameters
def_d_latent <- 1 # can randomize this
def_d_incubation <- 3
def_d_immunity <- 90 # TODO: Do some research!!!!!!!!!!!!!
def_p_asymp <- 0.405 # haven't used yet

def_d_quarantine <- 5

## Transmission probability (BA2):
# 0.00085 baseline from Max Planck Institute Calculator
# 80 m^2 area and 3 m height
# Times 1.5 to account for BA2
def_p_inf_class <- 0.01275 
# 400 m^2 area and 10 m height. was actually 0.00051, then times 1.5
def_p_inf_lunch <- 0.000765 

## Delta probabilities:
# class: 0.0056
# lunch: 0.00034

## Masks and vaccines
# TODO: add the eff's to param list
# BEFORE Omicron - placeholder
eff_mask <- function(x) case_when(
  x == "N95" ~ 0.83,
  x == "surgical" ~ 0.66,
  x == "cloth" ~ 0.56, 
  x == "none" ~ 0
)
def_p_masked <- 0

# This DOES consider OMICRON
eff_vax <- function(x) case_when(
  x == "booster" ~ 0.61,
  x == "two doses" ~ 0.36,
  x == "none" ~ 0
)
def_p_boosted <- 0

# Attributes about students
# assumption: random sample tests, not same students who like to test. This shouldn't be random
# add attribute of the people who regularly test
# CURRENTLY UNUSED
def_p_daily_test <- 0
def_test_sensitivity <- 0.9

# daily cases per capita in the community
def_community_p_inf <- 0.0028
# taking 4000 cases per day
# times 4 for underreporting
# divide by MN population

# testing
sens_pcr <- 0.95
sens_ant <- 0.60


# infectious duration

# NBA...
nba_df <- data.frame(
  days_since_detection = c(00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12, 13, 14, 15),
  num_positive         = c(49, 44, 45, 41, 30, 25, 21, 14, 12, 05, 01, 00, 00, 00, 00, 00),
  num_samples          = c(70, 55, 53, 57, 59, 64, 63, 63, 64, 61, 58, 50, 47, 48, 45, 43)
)
nba_df$prop_positive <- nba_df$num_positive/nba_df$num_samples
nba_df$prop_positive[c(1,2)] <- 1

nba_dist_contagious <- -diff(nba_df$prop_positive)
get_d_contagious <- function(size = 1) {
  #sample(1:length(nba_dist_contagious), size, prob = nba_dist_contagious, replace = TRUE)
  rgamma(size, shape = 4.18666987506, rate = 0.788491860701)
  # mean = 5.30971857
  # var = 6.7340182374
}