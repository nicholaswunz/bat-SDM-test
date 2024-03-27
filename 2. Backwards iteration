#### SCRIPT WITH BACKWARDS ITERATION ####

source("1. Prep of parameters and functions.R")

# ---- Backwards Iteration ---- 

# Iterate backwards across days at nb_timesteps+1 (daily terminal fitness)
for (weather_current in 1:length(weather)) { # for all the weather values (1-6)
  for (day_current in nb_days:1) { # and then for all the days
    if (day_current > 1) { 
      if (day_current != nb_days) { #if this is not the last day (for which fitness has already been calculated)
        for (fat_current in 1:length(fat_discretized)) { # then for each fat level
          #message(paste0('Calculating end of day fitness for weather type ', weather_current, ', on day ', day_current, ' with fat level #', round(fat_current, digits = 2), ' for both states...'))
          for (temp_state_current in 1:length(temp_state)) { # and then for each state
            fat_state <- fat_discretized[fat_current] #store the fat level in fat_state (this makes the code more readable)
          } #end state loop
        } #end fat loop
      }
      
      
      # Iterate backwards from max nb_timesteps for each day
      for (timestep_current in nb_timesteps:1) { # for this day
        
        
        #message(paste0('Calculating for all states and decisions for weather type ', weather_current, ', on day ', day_current, ' at timestep ', timestep_current, '...'))
        for (fat_current in 1:length(fat_discretized)) { # and for all fat states
          
          for (temp_state_current in 1:length(temp_state)) { # and for all behaviours
            
            fat_state <- fat_discretized[fat_current] #store the fat state to make the code more readable
            fitness_all_choices <- vector(mode = 'numeric', length=3) #make a vector to temporarily store fitness values for all three patches
            
            if (fat_state < fat_survival_threshold) {
              # Bat is dead. It will remain dead.
              fat_state <- 0
              fitness_all_choices <- 0
            } else {
              
              # Calculate resulting fitness of choosing each patch
              for (patch_chosen in 1:3) { # for all patches (patch 1 = torpor, patch 2 = rest, patch 3 = forage)
                if (temp_state_current == 1) { # if bat is in state 1 (torpid)
                  
                  if (patch_chosen == 1) { #if this is patch 1 (torpid)
                    
                    if (timestep_current == nb_timesteps) { # If t=72, then the interpolation needs to replace t+1 (t=73) with t=1 from next day, accounting for the probability of day types (using a weighted average)
                      
                      fitness_if_staying_torpid <- numeric()
                      
                      for (weather_tomorrow in 1:length(weather)) { 
                        
                        fat_when_torpid <- (fat_state - metabolic_cost_all[timestep_current,patch_chosen,weather_current]) #calculate torpid fat state   
                        fitness_if_staying_torpid <- c(fitness_if_staying_torpid,interpolate(fat_when_torpid, timestep_current+1, day_current, weather_tomorrow,1)) #calculate corresponding fitness (no predation, so just interpolating the fat state)
                        
                      }
                      
                      fitness_all_choices[patch_chosen] <-  weighted.mean(fitness_if_staying_torpid,weather_prob) #store it
                      
                      
                    } else { # If timestep is not 72
                      fat_when_torpid <- (fat_state - metabolic_cost_all[timestep_current,patch_chosen,weather_current]) #calculate torpid fat state
                      fitness_if_staying_torpid <- interpolate(fat_when_torpid, timestep_current+1, day_current, weather_current,1) #calculate corresponding fitness (no predation, so just interpolating the fat state)
                      fitness_all_choices[patch_chosen] <- fitness_if_staying_torpid #store it
                      
                    }
                    
                    
                  } else { #if not choosing to stay in torpid patch (i.e. if bat chooses to go to resting or foraging patch)
                    if (timestep_current == nb_timesteps) { # If t=72, then the interpolation needs to replace t+1 (t=73) with t=1 from next day, accounting for the probability of day types (using a weighted average)
                      
                      fitness_both_food_and_no_food_patch_2 <- numeric()
                      fitness_both_food_and_no_food_patch_3 <- numeric()
                      
                      for (weather_tomorrow in 1:length(weather)) {
                        
                        fat_if_food <- min(fat_max,(fat_state + foraging_benefit[timestep_current,patch_chosen,weather_current] - metabolic_cost_all[timestep_current,patch_chosen,weather_current] - arousal_cost_all[timestep_current,1,weather_current] - competition_cost_all[timestep_current,patch_chosen]))# Ensure that fat_expected does not exceed fat_max.
                        fat_if_no_food <- (fat_state - metabolic_cost_all[timestep_current,patch_chosen,weather_current] - arousal_cost_all[timestep_current,1,weather_current] - competition_cost_all[timestep_current,patch_chosen])
                        
                        fitness_if_food <- interpolate(fat_if_food, timestep_current+1, day_current, weather_tomorrow,2) - (risk_predation_all[timestep_current,patch_chosen]*exp(predation_risk_increase*fat_state) ) # må ha med torpor-state
                        fitness_if_no_food <- interpolate(fat_if_no_food, timestep_current+1, day_current, weather_tomorrow,2) - (risk_predation_all[timestep_current,patch_chosen]*exp(predation_risk_increase*fat_state) )
                        
                        fitness_both_food_and_no_food_patch_2 <- c(fitness_both_food_and_no_food_patch_2,(fitness_if_food*(prob_finding_food*exp(foraging_success_decline*fat_state)) + fitness_if_no_food*(1-(prob_finding_food*exp(foraging_success_decline*fat_state))) + resting_fitness_benefit))
                        fitness_both_food_and_no_food_patch_3 <- c(fitness_both_food_and_no_food_patch_3,(fitness_if_food*(prob_finding_food*exp(foraging_success_decline*fat_state)) + fitness_if_no_food*(1-(prob_finding_food*exp(foraging_success_decline*fat_state)))))
                      }
                      if (patch_chosen == 2) { #If the bats choose patch 2, there is an added fitness benefit
                        #store mean fitness 
                        fitness_all_choices[patch_chosen] <- weighted.mean(fitness_both_food_and_no_food_patch_2,weather_prob) #store it
                      } else if (patch_chosen == 3) {
                        fitness_all_choices[patch_chosen] <- weighted.mean(fitness_both_food_and_no_food_patch_3,weather_prob) #store it
                      }
                      
                      
                      
                    } else {  # If timestep is not 72
                      
                      fat_if_food <- min(fat_max,(fat_state + foraging_benefit[timestep_current,patch_chosen,weather_current] - metabolic_cost_all[timestep_current,patch_chosen,weather_current] - arousal_cost_all[timestep_current,1,weather_current] - competition_cost_all[timestep_current,patch_chosen]))# Ensure that fat_expected does not exceed fat_max.
                      fat_if_no_food <- (fat_state - metabolic_cost_all[timestep_current,patch_chosen,weather_current] - arousal_cost_all[timestep_current,1,weather_current] - competition_cost_all[timestep_current,patch_chosen])
                      
                      #calculate corresponding fitness
                      fitness_if_food <- interpolate(fat_if_food, timestep_current+1, day_current, weather_current,2) - (risk_predation_all[timestep_current,patch_chosen]*exp(predation_risk_increase*fat_state) ) # må ha med torpor-state
                      fitness_if_no_food <- interpolate(fat_if_no_food, timestep_current+1, day_current, weather_current,2) - (risk_predation_all[timestep_current,patch_chosen]*exp(predation_risk_increase*fat_state) )
                      
                      
                      if (patch_chosen == 2) { #If the bats choose patch 2, there is an added fitness benefit
                        #store mean fitness 
                        fitness_all_choices[patch_chosen] <- fitness_if_food*(prob_finding_food*exp(foraging_success_decline*fat_state)) + fitness_if_no_food*(1-(prob_finding_food*exp(foraging_success_decline*fat_state))) + resting_fitness_benefit
                      } else if (patch_chosen == 3) {
                        fitness_all_choices[patch_chosen] <- fitness_if_food*(prob_finding_food*exp(foraging_success_decline*fat_state)) + fitness_if_no_food*(1-(prob_finding_food*exp(foraging_success_decline*fat_state)))
                      }
                    }
                  }
                  
                  
                } else if (temp_state_current == 2) { # if bat is in state 2 (awake)
                  if (patch_chosen == 1) { #if this is patch 1 (torpid)
                    
                    if (timestep_current == nb_timesteps) { # If t=72, then the interpolation needs to replace t+1 (t=73) with t=1 from next day, accounting for the probability of day types (using a weighted average)
                      
                      fitness_if_going_torpid <- numeric()
                      
                      for (weather_tomorrow in 1:length(weather)) {
                        
                        fat_if_going_torpid <- (fat_state - metabolic_cost_all[timestep_current,patch_chosen,weather_current])
                        fitness_if_going_torpid <- c(fitness_if_going_torpid,interpolate(fat_if_going_torpid, timestep_current+1, day_current, weather_tomorrow,1))
                        
                      }
                      
                      fitness_all_choices[patch_chosen] <-  weighted.mean(fitness_if_going_torpid,weather_prob) #store it
                      
                      
                    } else {  # If timestep is not 72
                      
                      fat_if_going_torpid <- (fat_state - metabolic_cost_all[timestep_current,patch_chosen,weather_current]) #calculate torpid fat state
                      fitness_if_going_torpid <- interpolate(fat_if_going_torpid, timestep_current+1, day_current, weather_current,1) #calculate corresponding fitness (no predation, so just interpolating the fat state)
                      fitness_all_choices[patch_chosen] <- fitness_if_going_torpid #store it
                    }
                    
                  } else { # if patch chosen is 2 or 3, calculate fat if it finds food, and fat if it does not
                    
                    if (timestep_current == nb_timesteps) { # If t=72, then the interpolation needs to replace t+1 (t=73) with t=1 from next day, accounting for the probability of day types (using a weighted average)
                      
                      fitness_both_food_and_no_food_patch_2 <- numeric()
                      fitness_both_food_and_no_food_patch_3 <- numeric()
                      
                      for (weather_tomorrow in 1:length(weather)) {
                        
                        fat_if_food <- min(fat_max,(fat_state + foraging_benefit[timestep_current,patch_chosen,weather_current] - metabolic_cost_all[timestep_current,patch_chosen,weather_current] - competition_cost_all[timestep_current,patch_chosen]))# Ensure that fat_expected does not exceed fat_max.
                        fat_if_no_food <- (fat_state - metabolic_cost_all[timestep_current,patch_chosen,weather_current] - competition_cost_all[timestep_current,patch_chosen])
                        
                        fitness_if_food <- interpolate(fat_if_food, timestep_current+1, day_current, weather_tomorrow,2) - (risk_predation_all[timestep_current,patch_chosen]*exp(predation_risk_increase*fat_state) ) # må ha med torpor-state
                        fitness_if_no_food <- interpolate(fat_if_no_food, timestep_current+1, day_current, weather_tomorrow,2) - (risk_predation_all[timestep_current,patch_chosen]*exp(predation_risk_increase*fat_state) )
                        
                        fitness_both_food_and_no_food_patch_2 <- c(fitness_both_food_and_no_food_patch_2,(fitness_if_food*(prob_finding_food*exp(foraging_success_decline*fat_state)) + fitness_if_no_food*(1-(prob_finding_food*exp(foraging_success_decline*fat_state))) + resting_fitness_benefit))
                        fitness_both_food_and_no_food_patch_3 <- c(fitness_both_food_and_no_food_patch_3,(fitness_if_food*(prob_finding_food*exp(foraging_success_decline*fat_state)) + fitness_if_no_food*(1-(prob_finding_food*exp(foraging_success_decline*fat_state)))))
                      }
                      if (patch_chosen == 2) { #If the bats choose patch 2, there is an added fitness benefit
                        #store mean fitness 
                        fitness_all_choices[patch_chosen] <- weighted.mean(fitness_both_food_and_no_food_patch_2,weather_prob) #store it
                      } else if (patch_chosen == 3) {
                        fitness_all_choices[patch_chosen] <- weighted.mean(fitness_both_food_and_no_food_patch_3,weather_prob) #store it
                      }
                      
                      
                      
                    } else {  # If timestep is not 72
                      
                      fat_if_food <- min(fat_max,(fat_state + foraging_benefit[timestep_current,patch_chosen,weather_current] - metabolic_cost_all[timestep_current,patch_chosen,weather_current] - competition_cost_all[timestep_current,patch_chosen]))# Ensure that fat_expected does not exceed fat_max.
                      fat_if_no_food <- (fat_state - metabolic_cost_all[timestep_current,patch_chosen,weather_current] - competition_cost_all[timestep_current,patch_chosen])
                      
                      #calculate corresponding fitness
                      fitness_if_food <- interpolate(fat_if_food, timestep_current+1, day_current, weather_current,2) - (risk_predation_all[timestep_current,patch_chosen]*exp(predation_risk_increase*fat_state) ) # må ha med torpor-state
                      fitness_if_no_food <- interpolate(fat_if_no_food, timestep_current+1, day_current, weather_current,2) - (risk_predation_all[timestep_current,patch_chosen]*exp(predation_risk_increase*fat_state) )
                      
                      
                      if (patch_chosen == 2) { #If the bats choose patch 2, there is an added fitness benefit
                        #store mean fitness 
                        fitness_all_choices[patch_chosen] <- fitness_if_food*(prob_finding_food*exp(foraging_success_decline*fat_state)) + fitness_if_no_food*(1-(prob_finding_food*exp(foraging_success_decline*fat_state))) + resting_fitness_benefit
                      } else if (patch_chosen == 3) {
                        fitness_all_choices[patch_chosen] <- fitness_if_food*(prob_finding_food*exp(foraging_success_decline*fat_state)) + fitness_if_no_food*(1-(prob_finding_food*exp(foraging_success_decline*fat_state)))
                      }
                    }
                  }
                  
                } # end ifelse temp_state_current
                
              }  # end for patch_chosen loop
              
            }  # end ifelse fat_state
            
            
            # find and store highest fitness patch for this timestep/day/weather/state
            fitness[fat_current, timestep_current, day_current, weather_current, temp_state_current] <- max(fitness_all_choices)[1]
            
            # choose lowest risk patch if multiple patches have the same fitness
            patch_choice[fat_current, timestep_current, day_current, weather_current, temp_state_current] <- which(fitness_all_choices == max(fitness_all_choices))[1]
            
            patch_choice_stable_very_warm_state1 <- patch_choice[,,20,1,1] # optimal decision matrix for a bat in torpid state on a very warm day
            patch_choice_stable_dynamic_warm_state1 <- patch_choice[,,20,2,1] # optimal decision matrix for a bat in torpid state on a dynamic warm day
            patch_choice_stable_stable_warm_state1 <- patch_choice[,,20,3,1] # optimal decision matrix for a bat in torpid state on a stable warm day
            patch_choice_stable_dynamic_cold_state1 <- patch_choice[,,20,4,1] # optimal decision matrix for a bat in torpid state on a dynamic cold day
            patch_choice_stable_stable_cold_state1 <- patch_choice[,,20,5,1] # optimal decision matrix for a bat in torpid state on a stable cold day
            patch_choice_stable_very_cold_state1 <- patch_choice[,,20,6,1] # optimal decision matrix for a bat in torpid state on a very cold day
            
            patch_choice_stable_very_warm_state2 <- patch_choice[,,20,1,2] # optimal decision matrix for a bat in awake state on a very warm day
            patch_choice_stable_dynamic_warm_state2 <- patch_choice[,,20,2,2] # optimal decision matrix for a bat in awake state on a dynamic warm day
            patch_choice_stable_stable_warm_state2 <- patch_choice[,,20,3,2] # optimal decision matrix for a bat in awake state on a stable warm day
            patch_choice_stable_dynamic_cold_state2 <- patch_choice[,,20,4,2] # optimal decision matrix for a bat in awake state on a dynamic cold day
            patch_choice_stable_stable_cold_state2 <- patch_choice[,,20,5,2] # optimal decision matrix for a bat in awake state on a stable cold day
            patch_choice_stable_very_cold_state2 <- patch_choice[,,20,6,2] # optimal decision matrix for a bat in awake state on a very cold day
            
            
            
          } # end j loop 
        } # end temp_state_current loop 
      } # end timestep_current loop
      fitness[fat_current, nb_timesteps+1, day_current-1, weather_current, temp_state_current] <- fitness[fat_current, timestep_current, day_current, weather_current, temp_state_current]
    } # end if day > 1 
  } # end day_current loop
} #end good/bad weather loop

rm(patch_chosen)
rm(fat_current)

#END OF BACKWARDS ITERATION#
