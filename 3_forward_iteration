#### SCRIPT WITH FORWARD ITERATION ####

source("1_prep_function.R")
source("2_backwards_iteration.R")


###### FORWARD ITERATION ######

# The below section implements forward iteration of the model (i.e. looking at the fate of individuals in the model).

# Number of days to forward iterate for.
nb_days_forward  <- 30

# nb_timesteps/day is given from the model above.

# Number of individuals to iterate for each fat_state_init_forward Total number of individuals
# is length(fat_state_init_forward)*nb_indiv_forward:
nb_indiv_forward <- 200

# Initial states for individual (A vector of indices in fat_discretized).
fat_state_init_forward <- 20 #11:30  

temp_state_init_forward <- 2 #bats start in the awake state
#first_day <- 1 # For making figure 3 and 4

# state_forward_all[fat_state_init_forward, N, D, T]:
# State of individual N with initial state fat_state_init_forward, at day D and time T.
fat_state_forward_all <- array(data = NA, dim = c(length(fat_state_init_forward), nb_indiv_forward, nb_days_forward*nb_timesteps +1), dimnames = list(
  fat_discretized[fat_state_init_forward],
  1:nb_indiv_forward,
  1:(nb_days_forward*nb_timesteps+1)
))

temp_state_forward_all <- array(data = NA, dim = c(length(fat_state_init_forward), nb_indiv_forward, nb_days_forward*nb_timesteps +1), dimnames = list(
  rep(temp_state_init_forward, times = length(fat_state_init_forward)),
  1:nb_indiv_forward,
  1:(nb_days_forward*nb_timesteps+1)
))

decision_all <- array(data = NA, dim = c(length(fat_state_init_forward), nb_indiv_forward, nb_days_forward*nb_timesteps +1), dimnames = list(
  fat_discretized[fat_state_init_forward],
  1:nb_indiv_forward,
  1:(nb_days_forward*nb_timesteps+1)
))

day_type_all <- array(data = NA, dim = c(nb_indiv_forward, nb_days_forward*nb_timesteps +1), dimnames = list(
  1:nb_indiv_forward,
  1:(nb_days_forward*nb_timesteps+1)
))

# Keeping the sequence of day types days equal for all individuals
# weather_sequence <- sample(weather, size=nb_days_forward, replace=TRUE, prob = weather_prob)

for (j in 1:length(fat_state_init_forward)) {
  fat_state_init_forward_discretized <- fat_discretized[fat_state_init_forward[j]]
  for (n in 1:nb_indiv_forward) {
    # Set the initial state.
    fat_state_forward_all[j, n, 1] <- fat_state_init_forward_discretized
    temp_state_forward_all[j, n, 1] <- temp_state_init_forward
    
    # All bats experience their own day type sequence. To keep this similar across individuals move it out of the loop (see above) 
    weather_sequence <- sample(weather, size=nb_days_forward, replace=TRUE, prob = weather_prob)
    # weather_sequence <- c(first_day, sample(weather, size=nb_days_forward-1, replace=TRUE, prob = weather_prob)) # For making figure 3 and 4
    
    # Iterate through the time.
    for (z in 1:(nb_days_forward*nb_timesteps) ) {
      timestep_current = (z-1) %%  nb_timesteps  +1 # nb_timesteps of day
      day_current = (z-1) %/% nb_timesteps  +1 # Day
      # The current state.
      fat_state <- fat_state_forward_all[j, n, z]
      temp_state_current <- temp_state_forward_all[j, n, z]
      
      if (fat_state < fat_survival_threshold) {
        # Bat is dead. It will remain dead.
        fat_state_new_forward <- -1
        temp_new <- 0
        h <- 0
        
      } else {
        # The best decision given the current state (Found by rounding fat_state to the closest item
        # in fat_discretized).
        # TODO: interpolate the decision between the two closest optimal decisions.
        
        if (weather_sequence[day_current] == 1) {
          if (temp_state_current == 1) {
            h <- patch_choice_stable_very_warm_state1[which(abs(fat_discretized - fat_state) == min(abs(fat_discretized - fat_state)))[1], timestep_current]
            metabolism <- metabolic_cost_all[timestep_current,h,1]
            foraging   <- 0
            day_type <- 1
            
          } else if (temp_state_current == 2) {
            h <- patch_choice_stable_very_warm_state2[which(abs(fat_discretized - fat_state) == min(abs(fat_discretized - fat_state)))[1], timestep_current]
            metabolism <- metabolic_cost_all[timestep_current,h,1]
            foraging   <- foraging_benefit[timestep_current,h,1]
            day_type <- 1
            
          }
          
        } else if (weather_sequence[day_current] == 2) {
          if (temp_state_current == 1) {
            h <- patch_choice_stable_dynamic_warm_state1[which(abs(fat_discretized - fat_state) == min(abs(fat_discretized - fat_state)))[1], timestep_current]
            metabolism <- metabolic_cost_all[timestep_current,h,2]
            foraging   <- 0
            day_type <- 2
            
          } else if (temp_state_current == 2) {
            h <- patch_choice_stable_dynamic_warm_state2[which(abs(fat_discretized - fat_state) == min(abs(fat_discretized - fat_state)))[1], timestep_current]
            metabolism <- metabolic_cost_all[timestep_current,h,2]
            foraging   <- foraging_benefit[timestep_current,h,2]
            day_type <- 2
          }
          
        } else if (weather_sequence[day_current] == 3) {
          if (temp_state_current == 1) {
            h <- patch_choice_stable_stable_warm_state1[which(abs(fat_discretized - fat_state) == min(abs(fat_discretized - fat_state)))[1], timestep_current]
            metabolism <- metabolic_cost_all[timestep_current,h,3]
            foraging   <- 0
            day_type <- 3
            
          } else if (temp_state_current == 2) {
            h <- patch_choice_stable_stable_warm_state2[which(abs(fat_discretized - fat_state) == min(abs(fat_discretized - fat_state)))[1], timestep_current]
            metabolism <- metabolic_cost_all[timestep_current,h,3]
            foraging   <- foraging_benefit[timestep_current,h,3]
            day_type <- 3
          }
          
        } else if (weather_sequence[day_current] == 4) {
          if (temp_state_current == 1) {
            h <- patch_choice_stable_dynamic_cold_state1[which(abs(fat_discretized - fat_state) == min(abs(fat_discretized - fat_state)))[1], timestep_current]
            metabolism <- metabolic_cost_all[timestep_current,h,4]
            foraging   <- 0
            day_type <- 4
            
          } else if (temp_state_current == 2) {
            h <- patch_choice_stable_dynamic_cold_state2[which(abs(fat_discretized - fat_state) == min(abs(fat_discretized - fat_state)))[1], timestep_current]
            metabolism <- metabolic_cost_all[timestep_current,h,4]
            foraging   <- foraging_benefit[timestep_current,h,4]
            day_type <- 4
          }
          
        } else if (weather_sequence[day_current] == 5) {
          if (temp_state_current == 1) {
            h <- patch_choice_stable_stable_cold_state1[which(abs(fat_discretized - fat_state) == min(abs(fat_discretized - fat_state)))[1], timestep_current]
            metabolism <- metabolic_cost_all[timestep_current,h,5]
            foraging   <- 0
            day_type <- 5
            
          } else if (temp_state_current == 2) {
            h <- patch_choice_stable_stable_cold_state2[which(abs(fat_discretized - fat_state) == min(abs(fat_discretized - fat_state)))[1], timestep_current]
            metabolism <- metabolic_cost_all[timestep_current,h,5]
            foraging   <- foraging_benefit[timestep_current,h,5]
            day_type <- 5
          }
          
        } else if (weather_sequence[day_current] == 6) {
          if (temp_state_current == 1) {
            h <- patch_choice_stable_very_cold_state1[which(abs(fat_discretized - fat_state) == min(abs(fat_discretized - fat_state)))[1], timestep_current]
            metabolism <- metabolic_cost_all[timestep_current,h,6]
            foraging   <- 0
            day_type <- 6
            
          } else if (temp_state_current == 2) {
            h <- patch_choice_stable_very_cold_state2[which(abs(fat_discretized - fat_state) == min(abs(fat_discretized - fat_state)))[1], timestep_current]
            metabolism <- metabolic_cost_all[timestep_current,h,6]
            foraging   <- foraging_benefit[timestep_current,h,6]
            day_type <- 6
          }
        }
        
        if (runif(1) <= risk_predation_all[timestep_current,h]*exp(predation_risk_increase*fat_state)) { # Predation risk.
          
          # Negative fat_state = dead.
          fat_state_new_forward <- -1
          temp_new <- 0
          
        } else {
          
          if (runif(1) <= prob_finding_food) {
            success <- 1
          } else {
            success <- 0
          }
          
          fat_state_new_forward <- fat_state + foraging*success - metabolism
          
          if (h == 1) {
            temp_new <- 1
          } else {
            temp_new <- 2
          }
        }
      }
      
      # Set new state.
      fat_state_forward_all[j, n, z+1] <- fat_state_new_forward
      decision_all[j, n, z+1] <- h
      temp_state_forward_all[j, n, z+1] <- temp_new
      day_type_all[n, z] <- day_type
    }
  }
}

rm(h)
rm(j)
rm(z)


#END OF FORWARD ITERATION#
