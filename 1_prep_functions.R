#### SCRIPT WITH PREPARATIONS FOR BACKWARDS ITERATION ####

#### Parameter values (CAN BE CHANGED) ####

#### State (fat reserves) ####
mass_zero_fat    <- 7 # Mass of bat with zero fat reserves (g)
fat_max  <- 3 # Maximum fat reserves (g)
fat_discretized <- seq(from = 0, to = fat_max, length.out=40)   # Discretized values of fat_state
predation_risk_increase <- 0.05   # Increase in predation risk due to body mass (/g fat reserves)


#### State (torpor-use) ####
temp_state <- c(1,2) # state 1 = torpid, state 2 = not torpid


#### Fitness (time, probability of good or bad night) ####

# A season is divided in days, themselves divided in timesteps.
nb_timesteps <- 72     # Number of time periods per day
nb_days <- 30    # Number of days in summer month
nb_hours <- 24 # Number of hours in a day, used later to calculate metabolic cost per timestep instead of per hour.


# Environmental stochasticity and foraging
prob_finding_food <- 0.9  # Stochasticity in foraging success
foraging_success_decline <- -0.03  # Decrease in foraging success with increasing fat reserves (fat bats are less agile)
weather <- c(1:6) # weather on that day (very warm (1),  dynamic warm (2), stable warm (3), dynamic cold (4), stable cold (5) or very cold (6)); affects the output of the get_temperature_XXX functions



#### Metabolism and physiology ####

tnz <- 29  # thermoneutral zone (TNZ) in degrees celsius, used to define when to apply resting metabolic rate (RMR) or basal metabolic rate (BMR)
bmr <-  0.0042*mass_zero_fat  # BMR, used to calculate metabolic cost of resting in the TNZ (using results from Geiser and Turbill 2000)
cost_flight_hourly <- 0.615  # Hourly cost of flight in g for a little bat
energy_gain <- 1.3
fat_survival_threshold <- 0.05 # If bats reach fat reserve levels below this threshold they die (fitness = 0)
fat_survival_decrease <- 2   #  Fat level threshold for a linear decrease in fitness until the fat_survival_threshold is reached and fitness = 0



#### Scenarios ####
Temp_scenario <- "Nittedal" #Can be one of the following 5: Trondheim, Nittedal, Bodø, Tromsø or Gamvik (affects the temperature cycles and the prob of good days)
Light_scenario <- "Nittedal" #Can be one of the following 5: Trondheim, Nittedal, Bodø, Tromsø or Gamvik (affects the predation threat cycles)


weather_prob <- if (Temp_scenario == "Nittedal") {
  weather_prob <- c(0.238, 0.476, 0.0741, 0.201, 0.0106, 0)
}  else if (Temp_scenario == "Trondheim")  {
  weather_prob <- c(0.13, 0.30, 0.0435, 0.47, 0.0395, 0.0158)
}  else if (Temp_scenario == "Bodø")  {
  weather_prob <- c(0.0612, 0.219, 0.0791, 0.507, 0.126, 0.00719)
}  else if (Temp_scenario == "Tromsø")  {
  weather_prob <- c(0.015, 0.135, 0.0262, 0.648, 0.131, 0.0449)
}  else if (Temp_scenario == "Gamvik")  {
  weather_prob <- c(0.0223, 0.108, 0.0149, 0.532, 0.156, 0.167)
}


# Predation threath scenarios (affects the intercept of the predation threath daily cycle)
pred_low <- -0.03
pred_base <- 0
pred_high <- 0.03

# Energetic competition cost scenarios (affects the intercept of the competition cost)
comp_cost_low <- -0.05
comp_cost_base <- 0
comp_cost_high <- 0.05

# Resting benefit (theta) scenarios (a per-timestep fitness benefit when choosing to rest)
theta_low <- 0
theta_base <- 0.0015
theta_high <- 0.003

# Temperature scenario (for the last part of the results)
Ta_low <- -2
Ta_base <- 0
Ta_high <- 2

# Choose the scenario to run

pred_scenario <- pred_base
resting_fitness_benefit <- theta_base
comp_cost_scenario <- comp_cost_base
Ta_scenario <- Ta_base



#External temperature on different day types in degrees celsius (affects prey availability)

get_temperature_ext_very_warm <- function(time_current){
  temperature_ext_current <- Ta_scenario + 1.658e+01 + (1.176e-01*time_current) + (2.438e-02*time_current^2) - (8.987e-05*time_current^3) - (2.954e-05*time_current^4) + (6.013e-07*time_current^5) - (3.375e-09*time_current^6)
  return(temperature_ext_current)
} 


get_temperature_ext_dynamic_warm <- function(time_current){
  temperature_ext_current <- Ta_scenario + 1.411e+01 + (1.147e-01*time_current) + (9.198e-03*time_current^2) + (2.528e-04*time_current^3) - (2.555e-05*time_current^4) + (4.470e-07*time_current^5) - (2.359e-09*time_current^6)
  return(temperature_ext_current)
} 


get_temperature_ext_stable_warm <- function(time_current){
  temperature_ext_current <- Ta_scenario + 1.395e+01 + (1.082e-01*time_current) - (6.717e-03*time_current^2) + (5.616e-04*time_current^3) - (1.815e-05*time_current^4) + (2.299e-07*time_current^5) - (1.002e-09*time_current^6)
  return(temperature_ext_current)
} 


get_temperature_ext_dynamic_cold <- function(time_current){
  temperature_ext_current <- Ta_scenario + 9.622e+00 + (3.908e-02*time_current) + (7.858e-03*time_current^2) - (8.429e-05*time_current^3) - (6.033e-06*time_current^4) + (1.238e-07*time_current^5) - (6.431e-10*time_current^6)
  return(temperature_ext_current)
} 


get_temperature_ext_stable_cold <- function(time_current){
  temperature_ext_current <- Ta_scenario + 9.390e+00 + (3.525e-02*time_current) - (4.099e-03*time_current^2) + (2.890e-04*time_current^3) - (8.411e-06*time_current^4) + (1.025e-07*time_current^5) - (4.431e-10*time_current^6)
  return(temperature_ext_current)
} 


get_temperature_ext_very_cold <- function(time_current){
  temperature_ext_current <- Ta_scenario + 6.305e+00 + (4.960e-03*time_current) + (1.621e-03*time_current^2) + (1.799e-04*time_current^3) - (9.024e-06*time_current^4) + (1.267e-07*time_current^5) - (5.682e-10*time_current^6)
  return(temperature_ext_current)
} 







# Roost temperature for the different types of days in degrees celsius (affects metabolism when resting or using torpor)
# Warm days follow the same dynamic pattern in relation to air temperature, while cold days all are 2 degrees warmer than the air temperature

get_temperature_roost_very_warm <- function(time_current){
  temperature_roost_current <- Ta_scenario + 1.658e+01 + (1.176e-01*time_current) + (2.438e-02*time_current^2) - (8.987e-05*time_current^3) - (2.954e-05*time_current^4) + (6.013e-07*time_current^5) - (3.375e-09*time_current^6) +
    (2.342e+00 + (2.708e-01*time_current) - (2.754e-04*time_current^2) - (4.875e-05*time_current^3))
  return(temperature_roost_current)
}

get_temperature_roost_dynamic_warm <- function(time_current){
  temperature_roost_current <- Ta_scenario + 1.411e+01 + (1.147e-01*time_current) + (9.198e-03*time_current^2) + (2.528e-04*time_current^3) - (2.555e-05*time_current^4) + (4.470e-07*time_current^5) - (2.359e-09*time_current^6) +
    (2.342e+00 + (2.708e-01*time_current) - (2.754e-04*time_current^2) - (4.875e-05*time_current^3))
  return(temperature_roost_current)
}

get_temperature_roost_stable_warm <- function(time_current){
  temperature_roost_current <- Ta_scenario + 1.395e+01 + (1.082e-01*time_current) - (6.717e-03*time_current^2) + (5.616e-04*time_current^3) - (1.815e-05*time_current^4) + (2.299e-07*time_current^5) - (1.002e-09*time_current^6) +
    (2.342e+00 + (2.708e-01*time_current) - (2.754e-04*time_current^2) - (4.875e-05*time_current^3))
  return(temperature_roost_current)
}


get_temperature_roost_dynamic_cold <- function(time_current){
  temperature_roost_current <- Ta_scenario + 9.622e+00 + (3.908e-02*time_current) + (7.858e-03*time_current^2) - (8.429e-05*time_current^3) - (6.033e-06*time_current^4) + (1.238e-07*time_current^5) - (6.431e-10*time_current^6) + 2
  return(temperature_roost_current)
}

get_temperature_roost_stable_cold <- function(time_current){
  temperature_roost_current <- Ta_scenario + 9.390e+00 + (3.525e-02*time_current) - (4.099e-03*time_current^2) + (2.890e-04*time_current^3) - (8.411e-06*time_current^4) + (1.025e-07*time_current^5) - (4.431e-10*time_current^6) + 2
  return(temperature_roost_current)
}

get_temperature_roost_very_cold <- function(time_current){
  temperature_roost_current <- Ta_scenario + 6.305e+00 + (4.960e-03*time_current) + (1.621e-03*time_current^2) + (1.799e-04*time_current^3) - (9.024e-06*time_current^4) + (1.267e-07*time_current^5) - (5.682e-10*time_current^6) + 2
  return(temperature_roost_current)
}







# Prey availability (depends on current outside temperature)
get_prey <- function(temperature_ext_current){
  reward_prey_current <- 1 / (1 + exp( -0.524* (temperature_ext_current - 8)))
  return(reward_prey_current)
}


# Predation threat cycle (no difference between types of days)

risk_predation_baseline <- function(time_current){
  if (Light_scenario == "Trondheim") {
    pred_current <- pred_scenario + 0.06333 + (0.006028*time_current) - (0.00004952*time_current^2) + (0.00001183*time_current^3) - (0.0000004353*time_current^4) + (0.000000003666*time_current^5)
  }  else if (Light_scenario == "Nittedal") {
    pred_current <- ifelse(time_current < 52, pred_scenario + 0.057 + (0.02739*time_current) - (0.0008094*time_current^2) + (0.00001293*time_current^3) - (0.0000002586*time_current^4) + (0.000000002255*time_current^5),
                           pred_scenario + 0.28 + 0.026*time_current - 0.000883*time_current^2 + 0.0000128*time_current^3 - 0.000000256*time_current^4 + 0.00000000238*time_current^5)
  }  else if (Light_scenario == "Bodø") {
    pred_current <- pred_scenario + 0.05231 + 0.01239*time_current - 0.0003302*time_current^2 + 0.00002246*time_current^3 - 0.0000006359*time_current^4 + 0.000000004912*time_current^5
  }  else if (Light_scenario == "Tromsø") {
    pred_current <- pred_scenario + 0.04045 + 0.0105*time_current - 0.0006918*time_current^2 + 0.00003891*time_current^3 - 0.0000008519*time_current^4 + 0.000000005794*time_current^5
  }  else if (Light_scenario == "Gamvik") {
    pred_current <- pred_scenario + 0.04051 + 0.005727*time_current + 0.0002128*time_current^2 - 0.000003837*time_current^3 - 0.0000001062*time_current^4 + 0.000000001438*time_current^5
  }
  
  return(pred_current)
}

risk_predation_baseline <- risk_predation_baseline(1:nb_timesteps)

risk_predation_baseline<-ifelse(risk_predation_baseline[1:nb_timesteps]<0.0001,0.0001,risk_predation_baseline[1:nb_timesteps])
risk_predation_baseline<-ifelse(risk_predation_baseline[1:nb_timesteps]>0.2,0.2,risk_predation_baseline[1:nb_timesteps])


get_predation <- function(time_current){
  predation_current <- risk_predation_baseline[time_current]
  return(predation_current)
}



# Light-dependent energetic foraging success cost (caused by interspecific competition)

competition_cost_baseline <- function(time_current){
  if (Light_scenario == "Trondheim") {
    comp_cost_current <- comp_cost_scenario + 0.09249 + 1.067e-02*time_current - 7.479e-04*time_current^2 + 3.239e-05*time_current^3 - 6.291e-07*time_current^4 + 4.097e-09*time_current^5
  }  else if (Light_scenario == "Nittedal") {
    comp_cost_current <- comp_cost_scenario + 0.11426 + 2.097e-02*time_current - 1.348e-03*time_current^2 + 4.724e-05*time_current^3 - 8.336e-07*time_current^4 + 5.310e-09*time_current^5
  }  else if (Light_scenario == "Bodø") {
    comp_cost_current <- comp_cost_scenario + 0.11145 + 1.199e-02*time_current - 7.351e-04*time_current^2 + 3.132e-05*time_current^3 - 6.127e-07*time_current^4 + 3.977e-09*time_current^5
  }  else if (Light_scenario == "Tromsø") {
    comp_cost_current <- comp_cost_scenario + 0.10306 + 1.000e-02*time_current - 7.121e-04*time_current^2 + 3.063e-05*time_current^3 - 5.729e-07*time_current^4 + 3.585e-09*time_current^5
  }  else if (Light_scenario == "Gamvik") {
    comp_cost_current <- comp_cost_scenario + 0.11079 + 6.323e-03*time_current - 1.990e-04*time_current^2 + 8.119e-06*time_current^3 - 1.916e-07*time_current^4 + 1.395e-09*time_current^5
    
  }
  
  return(comp_cost_current)
}

competition_cost_baseline <- competition_cost_baseline(1:nb_timesteps)

competition_cost_baseline<-ifelse(competition_cost_baseline[1:nb_timesteps]<0.0001,0.0001,competition_cost_baseline[1:nb_timesteps])
competition_cost_baseline<-ifelse(competition_cost_baseline[1:nb_timesteps]>0.2,0.2,competition_cost_baseline[1:nb_timesteps])


get_competition_cost<- function(time_current){
  competition_cost_current <- competition_cost_baseline[time_current]
  return(competition_cost_current)
}






#Metabolism, transform the hourly cost into a per timestep cost
standardize_metabo_cost <- function(cost_hourly){
  fraction <- nb_hours/nb_timesteps
  cost <- cost_hourly*fraction
  return(cost)
}


#Calculate metabolism per hour, torpor (patch 1)
#Energy expenditure for torpor, per bat with 0 extra fat per hour
get_cost_torpor_hourly <- function(temperature_roost_current){
  cost_torpor_hourly <- (0.00006*exp(0.1284*temperature_roost_current))*(mass_zero_fat+1) # From the PBZ paper with brown long-eared bats, adding 1 gram of stomach content / fat reserves as most bats usually have some energy reserves
  if(cost_torpor_hourly<bmr*0.9){
    cost_torpor_hourly
  }
  else{
    cost_torpor_hourly <- bmr*0.9
  }
  return(cost_torpor_hourly)
}
get_cost_torpor_hourly <- Vectorize(get_cost_torpor_hourly, vectorize.args = "temperature_roost_current")


#Calculate metabolism per hour, resting (patch 2)
#Energy expenditure for resting, per bat with 0 extra fat per hour
get_cost_resting_hourly <- function(temperature_roost_current){
  if(temperature_roost_current>tnz){
    cost_resting_hourly <- bmr
  }
  else{
    cost_resting_hourly <- (0.0357-(0.0011*temperature_roost_current))*(mass_zero_fat+1) # From Geiser & Brigham 2000, adding 1 gram of stomach content / fat reserves as most bats usually have some energy reserves
  }
  return(cost_resting_hourly)
}
get_cost_resting_hourly <- Vectorize(get_cost_resting_hourly, vectorize.args = "temperature_roost_current")




# Cost of rewarming from different temperatures
get_cost_arousing <- function(temperature_roost_current){
  cost_arousing <- 0.1158621 - 0.003586207 * temperature_roost_current # Turbill's equation (converted to mass loss), total expenditure for active arousals
  if (cost_arousing > 0.005) {
    cost_arousing <- cost_arousing
  } else {
    cost_arousing <- 0.005
  }
  return(cost_arousing)
}
get_cost_arousing <- Vectorize(get_cost_arousing, vectorize.args = "temperature_roost_current")

library(magrittr)



patch1 <- rep(0, times = nb_timesteps)
patch2 <- rep(0, times = nb_timesteps)
patch3 <- seq(1, nb_timesteps, by=1)
patch3_very_warm <- standardize_metabo_cost((get_prey(get_temperature_ext_very_warm(patch3)))*(energy_gain))
patch3_dynamic_warm <- standardize_metabo_cost((get_prey(get_temperature_ext_dynamic_warm(patch3)))*(energy_gain))
patch3_stable_warm <- standardize_metabo_cost((get_prey(get_temperature_ext_stable_warm(patch3)))*(energy_gain))
patch3_dynamic_cold <- standardize_metabo_cost((get_prey(get_temperature_ext_dynamic_cold(patch3)))*(energy_gain))
patch3_stable_cold <- standardize_metabo_cost((get_prey(get_temperature_ext_stable_cold(patch3)))*(energy_gain))
patch3_very_cold <- standardize_metabo_cost((get_prey(get_temperature_ext_very_cold(patch3)))*(energy_gain))

foraging_benefit = array(c(patch1,patch2,patch3_very_warm,
                           patch1,patch2,patch3_dynamic_warm,
                           patch1,patch2,patch3_stable_warm,
                           patch1,patch2,patch3_dynamic_cold,
                           patch1,patch2,patch3_stable_cold,
                           patch1,patch2,patch3_very_cold),dim =c(nb_timesteps,3,length(weather)))
rm(patch1)
rm(patch2)
rm(patch3_very_warm)
rm(patch3_dynamic_warm)
rm(patch3_stable_warm)
rm(patch3_dynamic_cold)
rm(patch3_stable_cold)
rm(patch3_very_cold)




#make dataframe that stores timestep and corresponding predation risk (independent of weather)
patch1 <- rep(0, times = nb_timesteps)
patch2 <- rep(0, times = nb_timesteps)
patch3 <- seq(1, nb_timesteps, by=1)
patch3 <- get_predation(patch3)
risk_predation_all <- data.frame(patch1, patch2, patch3)
rm(patch1)
rm(patch2)
rm(patch3)


#make dataframe that stores timestep and corresponding energetic competition cost (independent of weather)
patch1 <- rep(0, times = nb_timesteps)
patch2 <- rep(0, times = nb_timesteps)
#patch3 <- rep(0, times = nb_timesteps)
patch3 <- seq(1, nb_timesteps, by=1)
patch3 <- get_competition_cost(patch3)
competition_cost_all <- data.frame(patch1, patch2, patch3)
rm(patch1)
rm(patch2)
rm(patch3)


patch1_very_warm <- standardize_metabo_cost(get_cost_torpor_hourly(get_temperature_roost_very_warm(1:nb_timesteps)))
patch2_very_warm <- standardize_metabo_cost(get_cost_resting_hourly(get_temperature_roost_very_warm(1:nb_timesteps)))
patch1_dynamic_warm <- standardize_metabo_cost(get_cost_torpor_hourly(get_temperature_roost_dynamic_warm(1:nb_timesteps)))
patch2_dynamic_warm <- standardize_metabo_cost(get_cost_resting_hourly(get_temperature_roost_dynamic_warm(1:nb_timesteps)))
patch1_stable_warm <- standardize_metabo_cost(get_cost_torpor_hourly(get_temperature_roost_stable_warm(1:nb_timesteps)))
patch2_stable_warm <- standardize_metabo_cost(get_cost_resting_hourly(get_temperature_roost_stable_warm(1:nb_timesteps)))
patch1_dynamic_cold <- standardize_metabo_cost(get_cost_torpor_hourly(get_temperature_roost_dynamic_cold(1:nb_timesteps)))
patch2_dynamic_cold <- standardize_metabo_cost(get_cost_resting_hourly(get_temperature_roost_dynamic_cold(1:nb_timesteps)))
patch1_stable_cold <- standardize_metabo_cost(get_cost_torpor_hourly(get_temperature_roost_stable_cold(1:nb_timesteps)))
patch2_stable_cold <- standardize_metabo_cost(get_cost_resting_hourly(get_temperature_roost_stable_cold(1:nb_timesteps)))
patch1_very_cold <- standardize_metabo_cost(get_cost_torpor_hourly(get_temperature_roost_very_cold(1:nb_timesteps)))
patch2_very_cold <- standardize_metabo_cost(get_cost_resting_hourly(get_temperature_roost_very_cold(1:nb_timesteps)))
patch3 <- rep(standardize_metabo_cost(cost_flight_hourly), times = nb_timesteps)
metabolic_cost_all <- array(c(patch1_very_warm,patch2_very_warm,patch3,
                              patch1_dynamic_warm,patch2_dynamic_warm,patch3,
                              patch1_stable_warm,patch2_stable_warm,patch3,
                              patch1_dynamic_cold,patch2_dynamic_cold,patch3,
                              patch1_stable_cold,patch2_stable_cold,patch3,
                              patch1_very_cold,patch2_very_cold,patch3),dim =c(nb_timesteps,3,length(weather)))
rm(patch1_very_warm)
rm(patch2_very_warm)
rm(patch1_dynamic_warm)
rm(patch2_dynamic_warm)
rm(patch1_stable_warm)
rm(patch2_stable_warm)
rm(patch1_dynamic_cold)
rm(patch2_dynamic_cold)
rm(patch1_stable_cold)
rm(patch2_stable_cold)
rm(patch1_very_cold)
rm(patch2_very_cold)
rm(patch3)


#make dataframe for metabolic cost of arousing at any timestep on good vs bad days
arousal_cost_very_warm <- get_cost_arousing(get_temperature_roost_very_warm(1:nb_timesteps))
arousal_cost_dynamic_warm <- get_cost_arousing(get_temperature_roost_dynamic_warm(1:nb_timesteps))
arousal_cost_stable_warm <- get_cost_arousing(get_temperature_roost_stable_warm(1:nb_timesteps))
arousal_cost_dynamic_cold <- get_cost_arousing(get_temperature_roost_dynamic_cold(1:nb_timesteps))
arousal_cost_stable_cold <- get_cost_arousing(get_temperature_roost_stable_cold(1:nb_timesteps))
arousal_cost_very_cold <- get_cost_arousing(get_temperature_roost_very_cold(1:nb_timesteps))
arousal_cost_all <- array(c(arousal_cost_very_warm,
                            arousal_cost_dynamic_warm,
                            arousal_cost_stable_warm,
                            arousal_cost_dynamic_cold,
                            arousal_cost_stable_cold,
                            arousal_cost_very_cold),dim =c(nb_timesteps,1,length(weather)))
rm(arousal_cost_very_warm)
rm(arousal_cost_dynamic_warm)
rm(arousal_cost_stable_warm)
rm(arousal_cost_dynamic_cold)
rm(arousal_cost_stable_cold)
rm(arousal_cost_very_cold)



#---- Empty arrays (do not change) ----

# We make an empty array to hold the fitness values  
fitness <- array(data = NA, dim = c(length(fat_discretized), nb_timesteps+1, nb_days, length(weather), length(temp_state)), 
                 dimnames = list(fat_discretized, 1:(nb_timesteps+1), 1:(nb_days), 1:length(weather), 1:length(temp_state)) )

# And for the decision loop (patch choice)
patch_choice <- array(data = NA, dim = c(length(fat_discretized), nb_timesteps, nb_days, length(weather),length(temp_state)), 
                      dimnames = list(fat_discretized, 1:nb_timesteps, 1:nb_days, 1:length(weather), 1:length(temp_state)))


#---- Functions (do not change) ----
#### Calculating terminal fitness at the end of each day####
# Terminal fitness is the probability of surviving the last day (i.e. day_current+1)

calculate_survival_proba <- function(fat_state) { 
  if (fat_state < fat_survival_threshold) {
    survival_proba <- 0
  } else {
    if (fat_state < fat_survival_decrease) {
      survival_proba <- ((fat_state-fat_survival_threshold)/(fat_survival_decrease - fat_survival_threshold))
    } else {
      survival_proba <- 1
    }
  }
  return(survival_proba)
}

# Calculate daily terminal fitness F(fat_state, timestep_current+1, day_current), for every day, where fat_state=state
for (j in 1:length(fat_discretized)) {
  fat_state <- fat_discretized[j]
  fitness[j, 1:(nb_timesteps+1), 1:(nb_days), 1:length(weather), 1:length(temp_state)] <- calculate_survival_proba(fat_state)
}
rm(j)



#---- Interpolating the discrete state variable ----
# NOTE this function actually returns a fitness value!
# The computer discretizes the state variable  but in reality energetic reserves is a continuous variable.We overcome this using interpolation (see C&M 2.1)


interpolate <- function (fat_state, timestep_current, day_current, weather_current, temp_state_current) {
  # This function returns a fitness value, based on the fat state.
  
  # Function that returns the index of the closest discrete fat_state value. Only the first value is returned if multiple equidistant values.
  closest_discrete_x <- function(fat_state) {
    return(which(abs(fat_discretized - fat_state) == min(abs(fat_discretized - fat_state)))[1])
  }
  
  # Function that interpolates between a and b. dx (delta_x) is either 0 and 1, depending whether it is closer to a or b (see below).
  linear_interpolation <- function (a, b, dx) {
    return((1-dx)*a+ b*dx)
  }
  
  # First, if reserves are negative, return 0 (the bat is dead).
  if (fat_state < 0) {
    return(0)
  }
  
  # Otherwise, find and store the closest discretized fat value to our actual fat value.
  closest <- closest_discrete_x(fat_state)
  
  #Then, check if this closest value is larger or smaller to our actual value
  if (fat_state < fat_discretized[closest]) {
    j1 <- closest -1 #this will be used to calculate delta_x
    j2 <- closest    #this will be used to calculate delta_x
  } else if (fat_state > fat_discretized[closest]) {
    j1 <- closest    #this will be used to calculate delta_x
    j2 <- closest +1 #this will be used to calculate delta_x
  } 
  # If neither are true, the fitness value for fat_state is already present in the matrix. No need to interpolate.
  else {
    return( fitness[closest, timestep_current, day_current, weather_current, temp_state_current])
  }
  
  # Calculate how fat_state is positioned in relation to fat_discretized[j1] and fat_discretized[j2].
  # 0: closer to fat_discretized[j1]; 1: closer to fat_discretized[j2]
  delta_x <- (fat_state-fat_discretized[j1])/(fat_discretized[j2]-fat_discretized[j1])
  
  # Interpolate.
  return(linear_interpolation(fitness[j1, timestep_current, day_current, weather_current, temp_state_current], fitness[j2, timestep_current, day_current, weather_current, temp_state_current], delta_x))
}
