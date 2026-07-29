# 02.1 FIT HMM (3 STATES)
#########################

# 02.11 Format stuff
#-------------------

# 02.11 Format stuff
#-------------------

# Load STEPS data from CSV
STEPS_5min_df <- read.csv(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/STEPS_5min.csv",
  stringsAsFactors = FALSE)

# Convert date/time columns back to their original formats
STEPS_5min_df <- STEPS_5min_df %>%
  mutate(
    timestamp_5 = as.POSIXct(
      timestamp_5,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "Europe/Zurich"
    ),
    
    date_local = as.Date(date_local),
    
    sunset = as.POSIXct(
      sunset,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "Europe/Zurich"
    ),
    
    sunrise = as.POSIXct(
      sunrise,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "Europe/Zurich"
    )
  )

head(STEPS_5min_df, 5)

# Factor & co.
STEPS_5min_df <- STEPS_5min_df %>%
  mutate(
    date_local = as.Date(date_local, tz = "Europe/Zurich"),

    day_start = as.POSIXct(
      paste(date_local, "00:00:00"),
      tz = "Europe/Zurich"),

    t_03 = day_start + hours(3),
    t_06 = day_start + hours(6),
    t_11 = day_start + hours(11),
    t_14 = day_start + hours(14),
    t_16 = day_start + hours(16),
    t_23 = day_start + hours(23),

    hour = hour(timestamp_5),

    tod_class = case_when(
      timestamp_5 >= sunset & timestamp_5 < t_23 ~ "SOIR : sunset - 23H00",
      timestamp_5 >= t_23 | timestamp_5 < t_03 ~ "NUIT : 23H00 - 03H00",
      timestamp_5 >= t_03 & timestamp_5 < t_06 ~ "NUIT : 03H00 - 06H00",
      timestamp_5 >= t_06 & timestamp_5 < sunrise ~ "LEVER DU JOUR : 06H00 - sunrise",
      timestamp_5 >= sunrise & timestamp_5 < t_11 ~ "MATIN : sunrise - 11H00",
      timestamp_5 >= t_11 & timestamp_5 < t_14 ~ "MIDI : 11H00 - 14H00",
      timestamp_5 >= t_14 & timestamp_5 < t_16 ~ "APRES-MIDI : 14H00 - 16H00",
      timestamp_5 >= t_16 & timestamp_5 < sunset ~ "COUCHER DU JOUR : 16H00 - sunset",
      TRUE ~ NA_character_),

    tod_class = factor(
      tod_class,
      levels = c(
        "SOIR : sunset - 23H00",
        "NUIT : 23H00 - 03H00",
        "NUIT : 03H00 - 06H00",
        "LEVER DU JOUR : 06H00 - sunrise",
        "MATIN : sunrise - 11H00",
        "MIDI : 11H00 - 14H00",
        "APRES-MIDI : 14H00 - 16H00",
        "COUCHER DU JOUR : 16H00 - sunset")),

    week_ID = case_when(
      timestamp_5 >= as.POSIXct("2025-12-05 00:00:00") & timestamp_5 <= as.POSIXct("2025-12-07 23:59:59") ~ "Week49_2025",
      timestamp_5 >= as.POSIXct("2025-12-08 00:00:00") & timestamp_5 <= as.POSIXct("2025-12-14 23:59:59") ~ "Week50_2025",  
      timestamp_5 >= as.POSIXct("2025-12-15 00:00:00") & timestamp_5 <= as.POSIXct("2025-12-21 23:59:59") ~ "Week51_2025",
      timestamp_5 >= as.POSIXct("2025-12-22 00:00:00") & timestamp_5 <= as.POSIXct("2025-12-28 23:59:59") ~ "Week52_2025", 
      timestamp_5 >= as.POSIXct("2025-12-29 00:00:00") & timestamp_5 <= as.POSIXct("2026-01-04 23:59:59") ~ "Week1_2026",
      timestamp_5 >= as.POSIXct("2026-01-05 00:00:00") & timestamp_5 <= as.POSIXct("2026-01-11 23:59:59") ~ "Week2_2026",
      timestamp_5 >= as.POSIXct("2026-01-12 00:00:00") & timestamp_5 <= as.POSIXct("2026-01-18 23:59:59") ~ "Week3_2026", 
      timestamp_5 >= as.POSIXct("2026-01-19 00:00:00") & timestamp_5 <= as.POSIXct("2026-01-25 23:59:59") ~ "Week4_2026",
      timestamp_5 >= as.POSIXct("2026-01-26 00:00:00") & timestamp_5 <= as.POSIXct("2026-02-01 23:59:59") ~ "Week5_2026",
      timestamp_5 >= as.POSIXct("2026-02-02 00:00:00") & timestamp_5 <= as.POSIXct("2026-02-08 23:59:59") ~ "Week6_2026",
      timestamp_5 >= as.POSIXct("2026-02-09 00:00:00") & timestamp_5 <= as.POSIXct("2026-02-15 23:59:59") ~ "Week7_2026",
      timestamp_5 >= as.POSIXct("2026-02-16 00:00:00") & timestamp_5 <= as.POSIXct("2026-02-22 23:59:59") ~ "Week8_2026",
      timestamp_5 >= as.POSIXct("2026-02-23 00:00:00") & timestamp_5 <= as.POSIXct("2026-03-01 23:59:59") ~ "Week9_2026",
      timestamp_5 >= as.POSIXct("2026-03-02 00:00:00") & timestamp_5 <= as.POSIXct("2026-03-08 23:59:59") ~ "Week10_2026",
      timestamp_5 >= as.POSIXct("2026-03-09 00:00:00") & timestamp_5 <= as.POSIXct("2026-03-15 23:59:59") ~ "Week11_2026",
      timestamp_5 >= as.POSIXct("2026-03-16 00:00:00") & timestamp_5 <= as.POSIXct("2026-03-22 23:59:59") ~ "Week12_2026",
      timestamp_5 >= as.POSIXct("2026-03-23 00:00:00") & timestamp_5 <= as.POSIXct("2026-03-27 23:59:59") ~ "Week13_2026",
      TRUE ~ NA_character_),

    week = factor(
      week_ID, 
      levels = c(
        "Week49_2025",
        "Week50_2025",
        "Week51_2025",
        "Week52_2025",
        "Week1_2026",
        "Week2_2026",
        "Week3_2026",
        "Week4_2026",
        "Week5_2026",
        "Week6_2026",
        "Week7_2026",
        "Week8_2026",
        "Week9_2026",
        "Week10_2026",
        "Week11_2026",
        "Week12_2026",
        "Week13_2026")),

    veg_height = factor(
      LAND_Height_arrange,
      levels = c(0, 5, 10, 15, 20, 100, 200)),

    LAND = factor(LAND_arrange),
    diel = factor(diel),
    ALAN = factor(
      ALAN, 
      levels = c(
        "OFF",
        "ON")),
        
    ID = factor(ID),
    SNOW = factor(
      Snow_cat,
      level = c(
        "Pas de neige",
        "Peu",
        "Modérée",
        "Forte")),

    LIGHT = LIGHT_z,
    moon_fraction = moon_fraction_z,
    temp = temp_z,
    vent = wind_z,
    precip = rain_z) %>%
  
  select(
    -LIGHT_z,
    -moon_fraction_z,
    -temp_z,
    -wind_z,
    -rain_z,
    -day_start,
    -t_03,
    -t_06,
    -t_11,
    -t_14,
    -t_16,
    -t_23)

# Only data until end of March
STEPS_5min_df <- STEPS_5min_df %>%
  filter(date_local >= as.Date("2025-12-05") & 
         date_local <= as.Date("2026-03-27"))

# Select number of states
nState <- 3
stateNames <- c("resting", "foraging", "exploring")

# Define distributions to use for each data stream
dist <- list(
  step = "gamma",
  angle = "vm")

# Clean
rm(STEPS_5min)

#-----------------------------------------------------------------------------------------------------------------------

# 02.12 Setting up the starting values
#-------------------------------------

# Step length
mu0 <- c(
  mean(STEPS_5min_df$step[STEPS_5min_df$diel == "day"], na.rm = TRUE),            # resting (mean diurnal distance)
  0.5 * mean(STEPS_5min_df$step[STEPS_5min_df$diel == "night"], na.rm = TRUE),    # foraging (half mean noct. dist.)
  2 * mean(STEPS_5min_df$step[STEPS_5min_df$diel == "night"], na.rm = TRUE))      # exploring (double mean noct. dist.)

# Standard deviation of length
sigma0 <- c(
  sd(STEPS_5min_df$step[STEPS_5min_df$diel == "day"], na.rm = TRUE),              # resting (mean diurnal distance)
  0.5 * sd(STEPS_5min_df$step[STEPS_5min_df$diel == "night"], na.rm = TRUE),      # foraging (half mean noct. dist.)
  2 * sd(STEPS_5min_df$step[STEPS_5min_df$diel == "night"], na.rm = TRUE))        # exploring (double mean noct. dist.)

# Angle concentration
#--------------------

# For resting
k_day <- 0.1 # close to zero = almost random angles

# For foraging & exploring: extract values from nocturnal data
night_step <- STEPS_5min_df$step[STEPS_5min_df$diel == "night" & STEPS_5min_df$step > 0]
q1 <- quantile(night_step, 0.33, na.rm = TRUE)
q2 <- quantile(night_step, 0.66, na.rm = TRUE)

# Foraging (turning angles with low distances)
night1_angles <- STEPS_5min_df$angle[
  STEPS_5min_df$diel == "night" &
  STEPS_5min_df$step <= q1]
hist(night1_angles)
night1_angles <- night1_angles[!is.na(night1_angles)]
k_night1 <- mle.vonmises(circular(night1_angles))$kappa

# Exploring (turning angles with big distances)
night2_angles <- STEPS_5min_df$angle[
  STEPS_5min_df$diel == "night" &
  STEPS_5min_df$step >= q2]
hist(night2_angles)
night2_angles <- night2_angles[!is.na(night2_angles)]
k_night2 <- mle.vonmises(circular(night2_angles))$kappa

# Kappa
kappa0 <- c(
  k_day,      # κ ≈ 0 → almost random angles
  k_night1,   # κ ≈ 0.2 → weak concentration
  k_night2)   # κ ≈ 0.5 → stronger persistence

# Zero-mass (proportion of zeroes during the day)
zero.day <- nrow(
  STEPS_5min_df[STEPS_5min_df$diel == "day" & STEPS_5min_df$step == 0, ]) /
  nrow(STEPS_5min_df[STEPS_5min_df$diel == "day", ])

zero.night.forage <- 0.1

zero.night.explore <- 0.5 * nrow(STEPS_5min_df[STEPS_5min_df$diel == "night" & STEPS_5min_df$step == 0, ]) /
  nrow(STEPS_5min_df[STEPS_5min_df$diel == "night", ])

zeromass0 <- c(zero.day, zero.night.forage, zero.night.explore)

# Combine starting parameters
Par30 <- list(
  step  = c(mu0, sigma0, zeromass0),
  angle = kappa0)

# Clean
rm(zero.day, zero.night.forage, zero.night.explore, zeromass0, sigma0, mu0, kappa0)

#-----------------------------------------------------------------------------------------------------------------------

# 02.13 Fit a 3-state HMM without covariates
#-------------------------------------------

#... or read it
mod30 <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/mod30.rds")

mod30 <- fitHMM(
  data = STEPS_5min_df,
  nbState = nState,
  stateNames = stateNames,
  dist = dist,
  Par0 = Par30)

# Look at parameter estimates
mod30

plot(mod30, 
    ask = FALSE, 
    plotTracks = FALSE)

dec_states <- viterbi(mod30)

# Let's look at predicted states of the first 20 time steps
head(dec_states, 20)

table(dec_states)

# Calculate the probability of being in each state 
statep <- stateProbs(mod30)

# Let's look at the state probability matrix
head(statep)

#-----------------------------------------------------------------------------------------------------------------------

# 02.14 Save data
#----------------
# Save all steps
saveRDS(STEPS_5min_df, file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/STEPS_5min_3states.rds")

# Save nocturnal data
STEPS_5min_df.NIGHT <- STEPS_5min_df[STEPS_5min_df$diel == "night",]

saveRDS(
  STEPS_5min_df.NIGHT,
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/STEPS_5min_3states_night.rds")

# Save null model
saveRDS(mod30, file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/mod30.rds")

# Save dist model
saveRDS(
  dist,
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/dist.rds")
  
rm(statep, dist, mod30, Par30, STEPS_5min_df)
rm(dec_states)
