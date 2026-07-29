#######################################
# 8.1 IC pour l'ensemble des variables
#######################################

# 8.10 LOAD YOUR DATA
######################

STEPS_5min_df.NIGHT <- readRDS(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/STEPS_5min_3states_night.rds")

modfull <- readRDS(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMulti_full.rds")

################################################################################

# 8.11 CONFIDENCE INTERVALS & COEFFICIENTS
###########################################

cb <- CIbeta(modfull)

# 8.011 Convert matrices to long format
#---------------------------------------

# Estimates
est_long <- as.data.frame(cb$beta$est) %>%
  tibble::rownames_to_column("variable") %>%
  pivot_longer(
    -variable,
    names_to = "transition",
    values_to = "estimate")

# Low CI
low_long <- as.data.frame(cb$beta$lower) %>%
  tibble::rownames_to_column("variable") %>%
  pivot_longer(
    -variable,
    names_to = "transition",
    values_to = "lower")

# High CI
up_long <- as.data.frame(cb$beta$upper) %>%
  tibble::rownames_to_column("variable") %>%
  pivot_longer(
    -variable,
    names_to = "transition",
    values_to = "upper")

# Clean
rm(cb)

#-------------------------------------------------------------------------------

# 8.12 Combine data
#--------------------

# Merge
beta_df <- est_long %>%
  left_join(low_long, by = c("variable", "transition")) %>%
  left_join(up_long, by = c("variable", "transition")) %>%
  mutate(
    significant = lower > 0 | upper < 0)

# Remove zeroes (not calculated)
#-------------------------------
beta_df <- beta_df[beta_df$estimate != 0,]

#-------------------------------------------------------------------------------

# 8.13 Rename values
#---------------------

# Transition probabilities
beta_df$transition[beta_df$transition == "1 -> 2"] <- "Immobile \n> Actif"
beta_df$transition[beta_df$transition == "1 -> 3"] <- "Immobile \n> Transit"
beta_df$transition[beta_df$transition == "2 -> 1"] <- "Actif \n> Immobile"
beta_df$transition[beta_df$transition == "2 -> 3"] <- "Actif \n> Transit"
beta_df$transition[beta_df$transition == "3 -> 1"] <- "Transit \n> Immobile"
beta_df$transition[beta_df$transition == "3 -> 2"] <- "Transit \n> Actif"

# Week
beta_df$variable[beta_df$variable == "weekWeek50_2025"] <- "Sem.50_2025"
beta_df$variable[beta_df$variable == "weekWeek51_2025"] <- "Sem.51_2025"
beta_df$variable[beta_df$variable == "weekWeek52_2025"] <- "Sem.52_2025"
beta_df$variable[beta_df$variable == "weekWeek1_2026"] <- "Sem.01_2026"
beta_df$variable[beta_df$variable == "weekWeek2_2026"] <- "Sem.02_2026"
beta_df$variable[beta_df$variable == "weekWeek3_2026"] <- "Sem.03_2026"
beta_df$variable[beta_df$variable == "weekWeek4_2026"] <- "Sem.04_2026"
beta_df$variable[beta_df$variable == "weekWeek5_2026"] <- "Sem.05_2026"
beta_df$variable[beta_df$variable == "weekWeek6_2026"] <- "Sem.06_2026"
beta_df$variable[beta_df$variable == "weekWeek7_2026"] <- "Sem.07_2026"
beta_df$variable[beta_df$variable == "weekWeek8_2026"] <- "Sem.08_2026"
beta_df$variable[beta_df$variable == "weekWeek9_2026"] <- "Sem.09_2026"
beta_df$variable[beta_df$variable == "weekWeek10_2026"] <- "Sem.10_2026"
beta_df$variable[beta_df$variable == "weekWeek11_2026"] <- "Sem.11_2026"
beta_df$variable[beta_df$variable == "weekWeek12_2026"] <- "Sem.12_2026"
beta_df$variable[beta_df$variable == "weekWeek13_2026"] <- "Sem.13_2026"

# Time of the day
beta_df$variable[
  beta_df$variable == "tod_classLEVER DU JOUR : 06H00 - sunrise"] <- "Lever du jour\n06:00 - Lever de soleil"
beta_df$variable[
  beta_df$variable == "tod_classNUIT : 03H00 - 06H00"] <- "Nuit (1e partie)\n23:00 - 03:00"
beta_df$variable[
  beta_df$variable == "tod_classNUIT : 23H00 - 03H00"] <- "Nuit (2e partie)\n03:00 - 06:00"

# Vegetation Height
beta_df$variable[
  beta_df$variable == "veg_height10"] <- "VegH-010"
beta_df$variable[
  beta_df$variable == "veg_height100"] <- "VegH-100"
beta_df$variable[
  beta_df$variable == "veg_height15"] <- "VegH-015"
beta_df$variable[
  beta_df$variable == "veg_height20"] <- "VegH-020"
beta_df$variable[
  beta_df$variable == "veg_height200"] <- "VegH-200+"
beta_df$variable[
  beta_df$variable == "veg_height5"] <- "VegH-005"

# Neige
beta_df$variable[ beta_df$variable == "SNOWPeu"] <- "Neige_Peu*"
beta_df$variable[ beta_df$variable == "SNOWModérée"] <- "Neige_Mod.*"
beta_df$variable[ beta_df$variable == "SNOWForte"] <- "Neige_Abon.*"

# Land
beta_df$variable[ beta_df$variable == "LAND11"] <- "Hab.\nLabour"
beta_df$variable[ beta_df$variable == "LAND12"] <- "Hab.\nPâture"
beta_df$variable[ beta_df$variable == "LAND13"] <- "Hab.\nPrairie ext."
beta_df$variable[ beta_df$variable == "LAND14"] <- "Hab.\nPrairie int."
beta_df$variable[ beta_df$variable == "LAND15"] <- "Hab.\nSemis inconnu"
beta_df$variable[ beta_df$variable == "LAND3"] <- "Hab.\nColza"
beta_df$variable[ beta_df$variable == "LAND6"] <- "Hab.\nForêt"
beta_df$variable[ beta_df$variable == "LAND8"] <- "Hab.\nGravière"
beta_df$variable[ beta_df$variable == "LAND9"] <- "Hab.\nHaie"

# Other
beta_df$variable[ beta_df$variable == "vent"] <- "Vent"
beta_df$variable[ beta_df$variable == "temp"] <- "Temp."
beta_df$variable[ beta_df$variable == "precip"] <- "Précip."
beta_df$variable[ beta_df$variable == "moon_fraction"] <- "Illum. lune"
beta_df$variable[ beta_df$variable == "LIGHT"] <- "Distance\nlampadaire"
beta_df$variable[ beta_df$variable == "ALANON"] <- "Allumé*"
beta_df$variable[ beta_df$variable == "ALANON:LIGHT"] <- "Allumé x\nDist. lamp.*"

# Clean
rm(est_long, low_long, up_long)

# Remove intercept
beta_df <- beta_df %>%
  filter(variable != "(Intercept)")

#-------------------------------------------------------------------------------

# 8.14 Change factors order
#----------------------------

transition_order <- c(
  "Immobile \n> Actif",
  "Immobile \n> Transit",
  "Actif \n> Immobile",
  "Actif \n> Transit",
  "Transit \n> Immobile",
  "Transit \n> Actif")

variable_order <- c(
  "Allumé*",
  "Distance\nlampadaire",
  "Allumé x\nDist. lamp.*",
  
  "Temp.",
  "Précip.",
  "Vent",
  "Illum. lune",
  "Neige_Peu*",
  "Neige_Mod.*",
  "Neige_Abon.*",
  
  "Hab.\nColza",
  "Hab.\nForêt",
  "Hab.\nGravière",
  "Hab.\nHaie",
  "Hab.\nLabour",
  "Hab.\nPâture",
  "Hab.\nPrairie ext.",
  "Hab.\nPrairie int.",
  "Hab.\nSemis inconnu",
  
  "VegH-005",
  "VegH-010",
  "VegH-015",
  "VegH-020",
  "VegH-100",
  "VegH-200+",
  
  "Nuit (1e partie)\n23:00 - 03:00",
  "Nuit (2e partie)\n03:00 - 06:00",
  "Lever du jour\n06:00 - Lever de soleil",
  "Sem.50_2025",
  "Sem.51_2025",
  "Sem.52_2025",
  "Sem.01_2026",
  "Sem.02_2026",
  "Sem.03_2026",
  "Sem.04_2026",
  "Sem.05_2026",
  "Sem.06_2026",
  "Sem.07_2026",
  "Sem.08_2026",
  "Sem.09_2026",
  "Sem.10_2026",
  "Sem.11_2026",
  "Sem.12_2026",
  "Sem.13_2026")

beta_df <- beta_df %>%
  mutate(
    transition = factor(transition, levels = transition_order),
    variable = factor(variable, levels = rev(variable_order)))

table(beta_df$variable)
#-------------------------------------------------------------------------------

# 8.15 Categorize Variables
#----------------------------
beta_df$categories <- NULL

# Create lists
Temporal.ls <- c("Sem.50_2025", "Sem.51_2025", "Sem.52_2025",
                "Sem.01_2026", "Sem.02_2026", "Sem.03_2026",
                "Sem.04_2026", "Sem.05_2026", "Sem.06_2026",
                "Sem.07_2026", "Sem.08_2026", "Sem.09_2026",
                "Sem.10_2026", "Sem.11_2026", "Sem.12_2026",
                "Sem.13_2026",  "Nuit (1e partie)\n23:00 - 03:00",
                "Nuit (2e partie)\n03:00 - 06:00", "Lever du jour\n06:00 - Lever de soleil")

Spatial.ls <- c("Hab.\nColza", "Hab.\nForêt", "Hab.\nHaie", "Hab.\nLabour", 
                "Hab.\nPâture", "Hab.\nPrairie ext.", "Hab.\nPrairie int.", "Hab.\nSemis inconnu", 
                "Hab.\nGravière", "VegH-005", "VegH-010", "VegH-015", 
                "VegH-020", "VegH-100", "VegH-200+")

Abiotic.ls <- c("Illum. lune", "Neige_Abon.*", "Neige_Mod.*", "Neige_Peu*", "Précip.",
                "Temp.", "Vent")

ALAN.ls <- c("Allumé*", "Allumé x\nDist. lamp.*", "Distance\nlampadaire")

# Attribute values
beta_df$categories[beta_df$variable %in% Temporal.ls] <- "Temporal."

beta_df$categories[beta_df$variable %in% Spatial.ls] <- "Spatial."
beta_df$categories[beta_df$variable %in% Abiotic.ls] <- "Abiotic"
beta_df$categories[beta_df$variable %in% ALAN.ls] <- "ALAN"

# Clean
rm(Temporal.ls, Spatial.ls, Abiotic.ls, ALAN.ls)

################################################################################

# Chose your categories
cats <- c("ALAN", "Abiotic", "Spatial.", "Temporal.")

# Empty list of plots
plots <- vector("list", length(cats))

# List of subtitles
titles <- c(
  ALAN = "(A) Lumière artificielle nocturne (ALAN)",
  Abiotic = "(B) Conditions météorologiques et lunaire",
  Spatial. = "(C) Caractéristiques du paysage",
  Temporal. = "(D) Variables temporelles")

# List of subtitles
subtitles <- c(
  ALAN = "*Catégorie de référence: Eteint\nDist. = Distance   lamp. = lampadaire",
  Abiotic = "*Catégorie de référence: Neige_Absente\nTemp. = Température   Précip. = Précipitations   Illum. = Illumination   Mod. = Modérée   Abon. = Abondante",
  Spatial. = paste0(
    "Catégorie de référence:\n",
    "- Habitat: Chaume, friche, jachère",
    strrep("\u00A0", 7),
    "Hab. = Habitat\n",
    "- Hauteur de vég.: 0 cm",
    strrep("\u00A0", 10),   
    "VegH = Hauteur de végétation"),
  Temporal. = paste0(
    "Catégorie de référence: \n",
    "- SOIR (Coucher du soleil - 23h00) \n",
    "- Sem.49_2025 (5 au 7 décembre)",
    strrep("\u00A0", 6),
    "Sem. = Semaine"))

# Fill within for loops
for (i in seq_along(cats)) {
  
  # i <- "Spatial."

  # Subset
  d <- subset(beta_df, categories == cats[i])

  # Plot
  p <- ggplot(
    d,
    aes(x = transition, 
        y = variable, 
        fill = estimate)) +
  geom_tile() +
  geom_text(
    data = subset(d, significant),
    aes(
      label = ifelse(estimate > 0, "+", "\u2212")),
    colour = "black",
    size = 5,
    fontface = "bold") +
    
  scale_fill_gradient2(
    low = "darkred",
    mid = "white",
    high = "steelblue",
    midpoint = 0) +
    
  labs(
    title = titles[i],
    subtitle = subtitles[i],
    x = "Transition",
    y = "Covariate",
    fill = "β") +
    
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold"),
    plot.subtitle = element_text(
      size = 10,
      face = "italic"),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 8))
  
  # Specific modifications
  #-----------------------
  
  # Low plots: legend bottom
  if (i %in% c(3, 4)) {
    p <- p +
      theme(
        legend.position = "bottom")}
  
  # Plot 1
  if (i == 1) {
    p <- p +
      geom_hline(
        yintercept = 1.5,
        colour = "white",
        linewidth = 2)}
  
  # Plot 2
  if (i == 2) {
    p <- p +
      geom_hline(
        yintercept = 3.5,
        colour = "white",
        linewidth = 2)}
  
  # Plot 3
  if (i == 3) {
    p <- p +
      geom_hline(
        yintercept = 6.5,
        colour = "white",
        linewidth = 2)}
  
  # Plot 4
  if (i == 4) {
    p <- p +
      geom_hline(
        yintercept = 16.5,
        colour = "white",
        linewidth = 2)}

  plots[[i]] <- p
  
}

up <- grid.arrange(plots[[1]], plots[[2]], nrow = 2, heights = c(1, 1.2))

down <- grid.arrange(plots[[3]], plots[[4]], ncol = 2)

full <- grid.arrange(up, down, ncol = 1, heights = c(1, 1.5))

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/tableau_multi_final.png",
  plot = full,
  width = 9.5,
  height = 13,
  dpi = 300)

# Clean
#------
rm(up, down, full, i, beta_df, d, plots, cats, p, subtitles, titles)
rm(transition_order, variable_order)
rm(modfull, STEPS_5min_df.NIGHT)

#################################################################################################################

#######################################################
# 8.2 Représentation de l'évolution de chaque variable
#######################################################

# 8.20 LOAD YOUR DATA
######################

STEPS_5min_df.NIGHT <- readRDS(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/STEPS_5min_3states_night.rds")

modfull <- readRDS(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMulti_full.rds")

# Names of your hidden states
state_names <- c("Immobile", "Actif", "Transit")

# Number of values used for prediction.
n <- 20

# List of data to store predictions
pred_data <- list(
  numerical = list(),
  categorical = list(),
  interaction = list())

################################################################################

# 8.21 PREPARE YOUR DATA
#########################

# 8.211 Fix reference variables
#-------------------------------

# These variables stay fixed.
# week, TOD, LAND and VegH will be averaged over later.

base_covs <- data.frame(
  
  temp = mean(STEPS_5min_df.NIGHT$temp, na.rm = TRUE),
  precip = mean(STEPS_5min_df.NIGHT$precip, na.rm = TRUE),
  vent = mean(STEPS_5min_df.NIGHT$vent, na.rm = TRUE),
  moon_fraction = mean(STEPS_5min_df.NIGHT$moon_fraction, na.rm = TRUE),
  LIGHT = mean(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  
  SNOW = factor(
    "Pas de neige",
    levels = levels(STEPS_5min_df.NIGHT$SNOW)),
  
  ALAN = factor(
    "OFF",
    levels = levels(STEPS_5min_df.NIGHT$ALAN)),
  
  week = factor(
    levels(STEPS_5min_df.NIGHT$week)[1],
    levels = levels(STEPS_5min_df.NIGHT$week)),
  
  tod_class = factor(
    levels(STEPS_5min_df.NIGHT$tod_class)[1],
    levels = levels(STEPS_5min_df.NIGHT$tod_class)),
  
  LAND = factor(
    levels(STEPS_5min_df.NIGHT$LAND)[1],
    levels = levels(STEPS_5min_df.NIGHT$LAND)),
  
  veg_height = factor(
    levels(STEPS_5min_df.NIGHT$veg_height)[1],
    levels = levels(STEPS_5min_df.NIGHT$veg_height)))

#-------------------------------------------------------------------------------

# 8.212 Create context grid
#---------------------------

# We use the observed combinations of week, TOD, LAND and VegH.
# Each combination receives a weight equal to its observed frequency.

context_grid <- STEPS_5min_df.NIGHT %>%
  count(
    week,
    tod_class,
    LAND,
    veg_height,
    SNOW,
    ALAN,
    name = "n_obs") %>%
  
  mutate(
    weight = n_obs / sum(n_obs))

# Make sure classes / factor levels match the fitted model data
context_grid <- context_grid %>%
  mutate(
    
    week = factor(
      week,
      levels = levels(STEPS_5min_df.NIGHT$week)),
    
    tod_class = factor(
      tod_class,
      levels = levels(STEPS_5min_df.NIGHT$tod_class)),
    
    LAND = factor(
      LAND,
      levels = levels(STEPS_5min_df.NIGHT$LAND)),
    
    veg_height = factor(
      veg_height,
      levels = levels(STEPS_5min_df.NIGHT$veg_height)),
    
    SNOW = factor(
      SNOW,
      levels = levels(STEPS_5min_df.NIGHT$SNOW)),
    
    ALAN = factor(
      ALAN, 
      levels = levels(STEPS_5min_df.NIGHT$ALAN)))

################################################################################

# 8.22 MAKE PREDICTION FOR ALAN (A) 
####################################

# 8.221 ALAN (A1)
#-----------------

# Extract all ALAN categories observed in the dataset
ALAN_levels <- levels(STEPS_5min_df.NIGHT$ALAN)

# Create all combinations of:
# - ALAN category
# - observed context
ALAN_grid <- expand.grid(
  value = ALAN_levels,
  context_id = seq_len(nrow(context_grid)))

# Duplicate baseline row for each prediction
ALAN_covs <- base_covs[rep(1, nrow(ALAN_grid)), ]

# Replace focal variable
ALAN_covs$ALAN <- factor(
  ALAN_grid$value,
  levels = levels(STEPS_5min_df.NIGHT$ALAN))

# Replace contextual variables
ALAN_covs$SNOW <- context_grid$SNOW[ALAN_grid$context_id]
ALAN_covs$week <- context_grid$week[ALAN_grid$context_id]
ALAN_covs$tod_class <- context_grid$tod_class[ALAN_grid$context_id]
ALAN_covs$LAND <- context_grid$LAND[ALAN_grid$context_id]
ALAN_covs$veg_height <- context_grid$veg_height[ALAN_grid$context_id]

# Predict stationary probabilities for every ALAN category × context
ALAN_stat <- stationary(
  modfull,
  covs = ALAN_covs)

# Convert to data frame
ALAN_df_context <- as.data.frame(ALAN_stat)
names(ALAN_df_context) <- state_names

# Keep context-level predictions
ALAN_df_context <- ALAN_df_context %>%
  mutate(
    value = ALAN_grid$value,
    context_id = ALAN_grid$context_id,
    weight = context_grid$weight[ALAN_grid$context_id],
    variable = "ALAN") %>%
  
  pivot_longer(
    cols = all_of(state_names),
    names_to = "state",
    values_to = "prob_context")

# Summarise across contexts
ALAN_df <- ALAN_df_context %>%
  
  group_by(
    value,
    variable,
    state) %>%
  
  summarise(
    # Weighted mean prediction
    prob = sum(prob_context * weight, na.rm = TRUE),
    
    # Contextual envelope, not model CI
    lower = quantile(prob_context, 0.25, na.rm = TRUE),
    upper = quantile(prob_context, 0.75, na.rm = TRUE),
    
    .groups = "drop")

# Store
pred_data$categorical$ALAN <- ALAN_df

# Clean
rm(ALAN_levels, ALAN_grid, ALAN_covs, ALAN_stat, ALAN_df_context, ALAN_df)

#-------------------------------------------------------------------------------

# 8.222 Distance to the closest lamp (A2)
#-----------------------------------------

# Create distance-to-lamp sequence
LIGHT_seq <- seq(
  min(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  max(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  length.out = n)

# Create all combinations of:
# - distance-to-lamp value
# - observed context
LIGHT_grid <- expand.grid(
  value = LIGHT_seq,
  context_id = seq_len(nrow(context_grid)))

# Duplicate baseline row for each prediction
LIGHT_covs <- base_covs[rep(1, nrow(LIGHT_grid)), ]

# Replace focal variable
LIGHT_covs$LIGHT <- LIGHT_grid$value

# Fix ALAN to OFF
#----------------
LIGHT_covs$ALAN <- factor(
  "OFF", levels = levels(STEPS_5min_df.NIGHT$ALAN))

# Replace contextual variables
#-----------------------------
LIGHT_covs$SNOW <- context_grid$SNOW[LIGHT_grid$context_id]
LIGHT_covs$week <- context_grid$week[LIGHT_grid$context_id]
LIGHT_covs$tod_class <- context_grid$tod_class[LIGHT_grid$context_id]
LIGHT_covs$LAND <- context_grid$LAND[LIGHT_grid$context_id]
LIGHT_covs$veg_height <- context_grid$veg_height[LIGHT_grid$context_id]

# Predict stationary probabilities for every distance × context
LIGHT_stat <- stationary(
  modfull,
  covs = LIGHT_covs)

# Convert to data frame
LIGHT_df_context <- as.data.frame(LIGHT_stat)
names(LIGHT_df_context) <- state_names

# Keep context-level predictions
LIGHT_df_context <- LIGHT_df_context %>%
  mutate(
    value = LIGHT_grid$value,
    context_id = LIGHT_grid$context_id,
    weight = context_grid$weight[LIGHT_grid$context_id],
    variable = "LIGHT") %>%
  
  pivot_longer(
    cols = all_of(state_names),
    names_to = "state",
    values_to = "prob_context")

# Summarise across contexts
LIGHT_df <- LIGHT_df_context %>%
  
  group_by(
    value,
    variable,
    state) %>%
  
  summarise(
    # Weighted mean prediction
    prob = sum(prob_context * weight, na.rm = TRUE),
    
    # Contextual envelope, not model CI
    lower = quantile(prob_context, 0.25, na.rm = TRUE),
    upper = quantile(prob_context, 0.75, na.rm = TRUE),
    
    .groups = "drop")

# Store
pred_data$numerical$LIGHT <- LIGHT_df

# Clean
rm(LIGHT_seq, LIGHT_grid, LIGHT_covs, LIGHT_stat, LIGHT_df_context, LIGHT_df)


#-------------------------------------------------------------------------------

# 8.223 ALAN × Distance to lamp (A3)
#------------------------------------

LIGHT_seq <- seq(
  min(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  max(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  length.out = n)

ALAN_levels <- levels(STEPS_5min_df.NIGHT$ALAN)

LIGHT_ALAN_grid <- expand.grid(
  value = LIGHT_seq,
  ALAN_value = ALAN_levels,
  context_id = seq_len(nrow(context_grid)))

LIGHT_ALAN_covs <- base_covs[rep(1, nrow(LIGHT_ALAN_grid)), ]

LIGHT_ALAN_covs$LIGHT <- LIGHT_ALAN_grid$value

LIGHT_ALAN_covs$ALAN <- factor(
  LIGHT_ALAN_grid$ALAN_value,
  levels = levels(STEPS_5min_df.NIGHT$ALAN))

# # Fix LAND to a specific habitat?
# #--------------------------------
# LIGHT_ALAN_covs$LAND <- factor(
#   "14", levels = levels(STEPS_5min_df.NIGHT$LAND))

# Replace contextual variables
#-----------------------------
LIGHT_ALAN_covs$SNOW <- context_grid$SNOW[LIGHT_ALAN_grid$context_id]
LIGHT_ALAN_covs$week <- context_grid$week[LIGHT_ALAN_grid$context_id]
LIGHT_ALAN_covs$tod_class <- context_grid$tod_class[LIGHT_ALAN_grid$context_id]
LIGHT_ALAN_covs$veg_height <- context_grid$veg_height[LIGHT_ALAN_grid$context_id]
LIGHT_ALAN_covs$LAND <- context_grid$LAND[LIGHT_ALAN_grid$context_id]

LIGHT_ALAN_stat <- stationary(
  modfull,
  covs = LIGHT_ALAN_covs)

LIGHT_ALAN_df_context <- as.data.frame(LIGHT_ALAN_stat)
names(LIGHT_ALAN_df_context) <- state_names

LIGHT_ALAN_df_context <- LIGHT_ALAN_df_context %>%
  mutate(
    value = LIGHT_ALAN_grid$value,
    ALAN = LIGHT_ALAN_grid$ALAN_value,
    context_id = LIGHT_ALAN_grid$context_id,
    weight = context_grid$weight[LIGHT_ALAN_grid$context_id],
    variable = "ALAN x LIGHT") %>%
  
  pivot_longer(
    cols = all_of(state_names),
    names_to = "state",
    values_to = "prob_context")

LIGHT_ALAN_df <- LIGHT_ALAN_df_context %>%
  group_by(
    value,
    ALAN,
    variable,
    state) %>%
  
  summarise(
    prob = sum(prob_context * weight, na.rm = TRUE),
    lower = quantile(prob_context, 0.25, na.rm = TRUE),
    upper = quantile(prob_context, 0.75, na.rm = TRUE),
    .groups = "drop")

pred_data$interaction$ALAN_LIGHT <- LIGHT_ALAN_df

# Clean
rm(LIGHT_seq, ALAN_levels, LIGHT_ALAN_grid, LIGHT_ALAN_covs, LIGHT_ALAN_stat,
  LIGHT_ALAN_df_context,LIGHT_ALAN_df)

################################################################################

# 8.23 MAKE PREDICTION FOR ABIOTIC VARIABLES (B) 
#################################################

# 8.231 Temperature (B1)
#------------------------

# Create temperature sequence
temp_seq <- seq(
  min(STEPS_5min_df.NIGHT$temp, na.rm = TRUE),
  max(STEPS_5min_df.NIGHT$temp, na.rm = TRUE),
  length.out = n)

# Create all combinations of:
# - temperature value
# - observed context
temp_grid <- expand.grid(
  value = temp_seq,
  context_id = seq_len(nrow(context_grid)))

# Duplicate baseline row for each prediction
temp_covs <- base_covs[rep(1, nrow(temp_grid)), ]

# Replace focal variable
temp_covs$temp <- temp_grid$value

# Replace contextual variables
temp_covs$ALAN <- context_grid$ALAN[temp_grid$context_id]
temp_covs$SNOW <- context_grid$SNOW[temp_grid$context_id]
temp_covs$week <- context_grid$week[temp_grid$context_id]
temp_covs$tod_class <- context_grid$tod_class[temp_grid$context_id]
temp_covs$LAND <- context_grid$LAND[temp_grid$context_id]
temp_covs$veg_height <- context_grid$veg_height[temp_grid$context_id]

# Predict stationary probabilities for every temperature × context
temp_stat <- stationary(modfull, covs = temp_covs)

# Convert to data frame
temp_df_context <- as.data.frame(temp_stat)
names(temp_df_context) <- state_names

# Keep context-level predictions
temp_df_context <- temp_df_context %>%
  mutate(
    value = temp_grid$value,
    context_id = temp_grid$context_id,
    weight = context_grid$weight[temp_grid$context_id],
    variable = "temp") %>%
  
  pivot_longer(
    cols = all_of(state_names),
    names_to = "state",
    values_to = "prob_context")

# Summarise across weighted contexts
temp_df <- temp_df_context %>%
  
  group_by(
    value,
    variable,
    state) %>%
  
  summarise(
    
    # Weighted mean prediction
    prob = sum(prob_context * weight, na.rm = TRUE),
    
    # Contextual envelope, not model CI
    lower = quantile(prob_context, 0.25, na.rm = TRUE),
    upper = quantile(prob_context, 0.75, na.rm = TRUE),
    
    .groups = "drop")

# Store
pred_data$numerical$Temperature <- temp_df

# Clean
rm(temp_seq, temp_grid, temp_covs, temp_stat, temp_df_context, temp_df)

#-------------------------------------------------------------------------------

# 8.232 Precipitation (B2)
#--------------------------

# Create precipitation sequence
precip_seq <- seq(
  min(STEPS_5min_df.NIGHT$precip, na.rm = TRUE),
  max(STEPS_5min_df.NIGHT$precip, na.rm = TRUE),
  length.out = n)

# Create all combinations of:
# - precipitation value
# - observed context
precip_grid <- expand.grid(
  value = precip_seq,
  context_id = seq_len(nrow(context_grid)))

# Duplicate baseline row for each prediction
precip_covs <- base_covs[rep(1, nrow(precip_grid)), ]

# Replace focal variable
precip_covs$precip <- precip_grid$value

# Replace contextual variables
precip_covs$ALAN <- context_grid$ALAN[precip_grid$context_id]
precip_covs$SNOW <- context_grid$SNOW[precip_grid$context_id]
precip_covs$week <- context_grid$week[precip_grid$context_id]
precip_covs$tod_class <- context_grid$tod_class[precip_grid$context_id]
precip_covs$LAND <- context_grid$LAND[precip_grid$context_id]
precip_covs$veg_height <- context_grid$veg_height[precip_grid$context_id]

# Predict stationary probabilities for every precipitation × context
precip_stat <- stationary(
  modfull,
  covs = precip_covs)

# Convert to data frame
precip_df_context <- as.data.frame(precip_stat)
names(precip_df_context) <- state_names

# Keep context-level predictions
precip_df_context <- precip_df_context %>%
  
  mutate(
    value = precip_grid$value,
    context_id = precip_grid$context_id,
    weight = context_grid$weight[precip_grid$context_id],
    variable = "precip") %>%
  
  pivot_longer(
    cols = all_of(state_names),
    names_to = "state",
    values_to = "prob_context")

# Summarise across weighted contexts
precip_df <- precip_df_context %>%
  group_by(
    value,
    variable,
    state) %>%
  summarise(
    
    # Weighted mean prediction
    prob = sum(prob_context * weight, na.rm = TRUE),
    
    # Contextual envelope, not model CI
    lower = quantile(prob_context, 0.25, na.rm = TRUE),
    upper = quantile(prob_context, 0.75, na.rm = TRUE),
    
    .groups = "drop")

# Store
pred_data$numerical$Precipitation <- precip_df

# Clean
rm(precip_seq, precip_grid, precip_covs, precip_stat, precip_df_context, precip_df)

#-------------------------------------------------------------------------------

# 8.233 Wind (B3)
#-----------------

# Create wind sequence
vent_seq <- seq(
  min(STEPS_5min_df.NIGHT$vent, na.rm = TRUE),
  max(STEPS_5min_df.NIGHT$vent, na.rm = TRUE),
  length.out = n)

# Create all combinations of:
# - wind value
# - observed context
vent_grid <- expand.grid(
  value = vent_seq,
  context_id = seq_len(nrow(context_grid)))

# Duplicate baseline row for each prediction
vent_covs <- base_covs[rep(1, nrow(vent_grid)), ]

# Replace focal variable
vent_covs$vent <- vent_grid$value

# Replace contextual variables
vent_covs$ALAN <- context_grid$ALAN[vent_grid$context_id]
vent_covs$SNOW <- context_grid$SNOW[vent_grid$context_id]
vent_covs$week <- context_grid$week[vent_grid$context_id]
vent_covs$tod_class <- context_grid$tod_class[vent_grid$context_id]
vent_covs$LAND <- context_grid$LAND[vent_grid$context_id]
vent_covs$veg_height <- context_grid$veg_height[vent_grid$context_id]

# Predict stationary probabilities for every wind × context
vent_stat <- stationary(
  modfull,
  covs = vent_covs)

# Convert to data frame
vent_df_context <- as.data.frame(vent_stat)
names(vent_df_context) <- state_names

# Keep context-level predictions
vent_df_context <- vent_df_context %>%
  
  mutate(
    value = vent_grid$value,
    context_id = vent_grid$context_id,
    weight = context_grid$weight[vent_grid$context_id],
    variable = "vent") %>%
  
  pivot_longer(
    cols = all_of(state_names),
    names_to = "state",
    values_to = "prob_context")

# Summarise across weighted contexts
vent_df <- vent_df_context %>%
  
  group_by(
    value,
    variable,
    state) %>%
  
  summarise(
    
    # Weighted mean prediction
    prob = sum(prob_context * weight, na.rm = TRUE),
    
    # Contextual envelope, not model CI
    lower = quantile(prob_context, 0.25, na.rm = TRUE),
    upper = quantile(prob_context, 0.75, na.rm = TRUE),
    
    .groups = "drop")

# Store
pred_data$numerical$Wind <- vent_df

# Clean
rm(vent_seq, vent_grid, vent_covs, vent_stat, vent_df_context, vent_df)

#-------------------------------------------------------------------------------

# 8.234 Moon (B4)
#-----------------

# Create moon sequence
moon_seq <- seq(
  min(STEPS_5min_df.NIGHT$moon_fraction, na.rm = TRUE),
  max(STEPS_5min_df.NIGHT$moon_fraction, na.rm = TRUE),
  length.out = n)

# Create all combinations of:
# - moon fraction value
# - observed context
moon_grid <- expand.grid(
  value = moon_seq,
  context_id = seq_len(nrow(context_grid)))

# Duplicate baseline row for each prediction
moon_covs <- base_covs[rep(1, nrow(moon_grid)), ]

# Replace focal variable
moon_covs$moon_fraction <- moon_grid$value

# Replace contextual variables
moon_covs$ALAN <- context_grid$ALAN[moon_grid$context_id]
moon_covs$SNOW <- context_grid$SNOW[moon_grid$context_id]
moon_covs$week <- context_grid$week[moon_grid$context_id]
moon_covs$tod_class <- context_grid$tod_class[moon_grid$context_id]
moon_covs$LAND <- context_grid$LAND[moon_grid$context_id]
moon_covs$veg_height <- context_grid$veg_height[moon_grid$context_id]

# Predict stationary probabilities for every moon fraction × context
moon_stat <- stationary(
  modfull,
  covs = moon_covs)

# Convert to data frame
moon_df_context <- as.data.frame(moon_stat)
names(moon_df_context) <- state_names

# Keep context-level predictions
moon_df_context <- moon_df_context %>%
  
  mutate(
    value = moon_grid$value,
    context_id = moon_grid$context_id,
    weight = context_grid$weight[moon_grid$context_id],
    variable = "moon_fraction") %>%
  
  pivot_longer(
    cols = all_of(state_names),
    names_to = "state",
    values_to = "prob_context")

# Summarise across weighted contexts
moon_df <- moon_df_context %>%
  
  group_by(
    value,
    variable,
    state) %>%
  
  summarise(
    
    # Weighted mean prediction
    prob = sum(prob_context * weight, na.rm = TRUE),
    
    # Contextual envelope, not model CI
    lower = quantile(prob_context, 0.25, na.rm = TRUE),
    upper = quantile(prob_context, 0.75, na.rm = TRUE),
    
    .groups = "drop")

# Store
pred_data$numerical$Moon <- moon_df

# Clean
rm(moon_seq, moon_grid, moon_covs, moon_stat, moon_df_context, moon_df)

#-------------------------------------------------------------------------------

# 8.235 Snow (B5)
#-----------------

# Extract all snow categories observed in the dataset
snow_levels <- levels(STEPS_5min_df.NIGHT$SNOW)

# Create all combinations of:
# - snow category
# - observed context
snow_grid <- expand.grid(
  value = snow_levels,
  context_id = seq_len(nrow(context_grid)))

# Duplicate baseline row for each prediction
snow_covs <- base_covs[rep(1, nrow(snow_grid)), ]

# Replace focal variable
snow_covs$SNOW <- factor(
  snow_grid$value,
  levels = levels(STEPS_5min_df.NIGHT$SNOW))

# Replace contextual variables
snow_covs$ALAN <- context_grid$ALAN[snow_grid$context_id]
snow_covs$week <- context_grid$week[snow_grid$context_id]
snow_covs$tod_class <- context_grid$tod_class[snow_grid$context_id]
snow_covs$LAND <- context_grid$LAND[snow_grid$context_id]
snow_covs$veg_height <- context_grid$veg_height[snow_grid$context_id]

# Predict stationary probabilities for every snow category × context
snow_stat <- stationary(
  modfull,
  covs = snow_covs)

# Convert to data frame
snow_df_context <- as.data.frame(snow_stat)
names(snow_df_context) <- state_names

# Keep context-level predictions
snow_df_context <- snow_df_context %>%
  mutate(
    value = snow_grid$value,
    context_id = snow_grid$context_id,
    weight = context_grid$weight[snow_grid$context_id],
    variable = "SNOW") %>%
  
  pivot_longer(
    cols = all_of(state_names),
    names_to = "state",
    values_to = "prob_context")

# Summarise across contexts
snow_df <- snow_df_context %>%
  
  group_by(
    value,
    variable,
    state) %>%
  
  summarise(
    
    # Summarise across weighted contexts
    prob = sum(prob_context * weight, na.rm = TRUE),
    
    # Contextual envelope, not model CI
    lower = quantile(prob_context, 0.25, na.rm = TRUE),
    upper = quantile(prob_context, 0.75, na.rm = TRUE),
    
    .groups = "drop")


# Store
pred_data$categorical$Snow <- snow_df

# Clean
rm(snow_levels, snow_grid, snow_covs, snow_stat, snow_df_context, snow_df)

################################################################################

# 8.24 MAKE PREDICTION FOR HABITAT VARIABLES (C) 
#################################################

# 8.241 Vegetation height (C1)
#------------------------------

# Extract all vegetation height categories observed in the dataset
veg_levels <- levels(STEPS_5min_df.NIGHT$veg_height)

# Create all combinations of:
# - vegetation height category
# - observed context
veg_grid <- expand.grid(
  value = veg_levels,
  context_id = seq_len(nrow(context_grid)))

# Duplicate baseline row for each prediction
veg_covs <- base_covs[rep(1, nrow(veg_grid)), ]

# Replace focal variable
veg_covs$veg_height <- factor(
  veg_grid$value,
  levels = levels(STEPS_5min_df.NIGHT$veg_height))

# Replace contextual variables
veg_covs$ALAN <- context_grid$ALAN[veg_grid$context_id]
veg_covs$SNOW <- context_grid$SNOW[veg_grid$context_id]
veg_covs$week <- context_grid$week[veg_grid$context_id]
veg_covs$tod_class <- context_grid$tod_class[veg_grid$context_id]
veg_covs$LAND <- context_grid$LAND[veg_grid$context_id]

# Predict stationary probabilities for every vegetation height × context
veg_stat <- stationary(
  modfull,
  covs = veg_covs)

# Convert to data frame
veg_df_context <- as.data.frame(veg_stat)
names(veg_df_context) <- state_names

# Keep context-level predictions
veg_df_context <- veg_df_context %>%
  mutate(
    value = veg_grid$value,
    context_id = veg_grid$context_id,
    weight = context_grid$weight[veg_grid$context_id],
    variable = "veg_height") %>%
  
  pivot_longer(
    cols = all_of(state_names),
    names_to = "state",
    values_to = "prob_context")

# Summarise across contexts
veg_df <- veg_df_context %>%
  
  group_by(
    value,
    variable,
    state) %>%
  
  summarise(
    
    # Weighted mean prediction
    prob = sum(prob_context * weight, na.rm = TRUE),
    
    # Contextual envelope, not model CI
    lower = quantile(prob_context, 0.25, na.rm = TRUE),
    upper = quantile(prob_context, 0.75, na.rm = TRUE),
    
    .groups = "drop")

# Store
pred_data$categorical$VegHeight <- veg_df

# Clean
rm(veg_levels, veg_grid, veg_covs, veg_stat, veg_df_context, veg_df)

#-------------------------------------------------------------------------------

# 8.242 Habitat (C2)
#--------------------

# Extract all habitat categories observed in the dataset
habitat_levels <- levels(STEPS_5min_df.NIGHT$LAND)

# Create all combinations of:
# - habitat category
# - observed context
habitat_grid <- expand.grid(
  value = habitat_levels,
  context_id = seq_len(nrow(context_grid)))

# Duplicate baseline row for each prediction
habitat_covs <- base_covs[rep(1, nrow(habitat_grid)), ]

# Replace focal variable
habitat_covs$LAND <- factor(
  habitat_grid$value,
  levels = levels(STEPS_5min_df.NIGHT$LAND))

# Replace contextual variables
habitat_covs$ALAN <- context_grid$ALAN[habitat_grid$context_id]
habitat_covs$SNOW <- context_grid$SNOW[habitat_grid$context_id]
habitat_covs$week <- context_grid$week[habitat_grid$context_id]
habitat_covs$tod_class <- context_grid$tod_class[habitat_grid$context_id]
habitat_covs$veg_height <- context_grid$veg_height[habitat_grid$context_id]

# Predict stationary probabilities for every habitat category × context
habitat_stat <- stationary(
  modfull,
  covs = habitat_covs)

# Convert to data frame
habitat_df_context <- as.data.frame(habitat_stat)
names(habitat_df_context) <- state_names

# Keep context-level predictions
habitat_df_context <- habitat_df_context %>%
  mutate(
    value = habitat_grid$value,
    context_id = habitat_grid$context_id,
    weight = context_grid$weight[habitat_grid$context_id],
    variable = "LAND") %>%
  
  pivot_longer(
    cols = all_of(state_names),
    names_to = "state",
    values_to = "prob_context")

# Summarise across contexts
habitat_df <- habitat_df_context %>%
  
  group_by(
    value,
    variable,
    state) %>%
  
  summarise(
    
    # Weighted mean prediction
    prob = sum(prob_context * weight, na.rm = TRUE),
    
    # Contextual envelope, not model CI
    lower = quantile(prob_context, 0.25, na.rm = TRUE),
    upper = quantile(prob_context, 0.75, na.rm = TRUE),
    
    .groups = "drop")

# Store
pred_data$categorical$Habitat <- habitat_df

# Clean
rm(habitat_levels, habitat_grid, habitat_covs, habitat_stat, habitat_df_context,
  habitat_df)

################################################################################

# 8.25 MAKE PREDICTION FOR TEMPORAL VARIABLES (D) 
##################################################

# 8.251 Time of the day (D1)
#----------------------------

# Extract all time-of-day categories observed in the dataset
tod_levels <- levels(STEPS_5min_df.NIGHT$tod_class)

# Create all combinations of:
# - time-of-day category
# - observed context
tod_grid <- expand.grid(
  value = tod_levels,
  context_id = seq_len(nrow(context_grid)))

# Duplicate baseline row for each prediction
tod_covs <- base_covs[rep(1, nrow(tod_grid)), ]

# Replace focal variable
tod_covs$tod_class <- factor(
  tod_grid$value,
  levels = levels(STEPS_5min_df.NIGHT$tod_class))

# Replace contextual variables
tod_covs$ALAN <- context_grid$ALAN[tod_grid$context_id]
tod_covs$SNOW <- context_grid$SNOW[tod_grid$context_id]
tod_covs$week <- context_grid$week[tod_grid$context_id]
tod_covs$LAND <- context_grid$LAND[tod_grid$context_id]
tod_covs$veg_height <- context_grid$veg_height[tod_grid$context_id]

# Predict stationary probabilities for every time-of-day × context
tod_stat <- stationary(
  modfull,
  covs = tod_covs)

# Convert to data frame
tod_df_context <- as.data.frame(tod_stat)
names(tod_df_context) <- state_names

# Keep context-level predictions
tod_df_context <- tod_df_context %>%
  mutate(
    value = tod_grid$value,
    context_id = tod_grid$context_id,
    weight = context_grid$weight[tod_grid$context_id],
    variable = "tod_class") %>%
  
  pivot_longer(
    cols = all_of(state_names),
    names_to = "state",
    values_to = "prob_context")

# Summarise across contexts
tod_df <- tod_df_context %>%
  
  group_by(
    value,
    variable,
    state) %>%
  
  summarise(
    
    # Weighted mean prediction
    prob = sum(prob_context * weight, na.rm = TRUE),
    
    # Contextual envelope, not model CI
    lower = quantile(prob_context, 0.25, na.rm = TRUE),
    upper = quantile(prob_context, 0.75, na.rm = TRUE),
    
    .groups = "drop")

# Store
pred_data$categorical$TOD <- tod_df

# Clean
rm(tod_levels, tod_grid, tod_covs, tod_stat, tod_df_context, tod_df)

#-------------------------------------------------------------------------------

# 8.252 WEEK (D2)
#-----------------

# Extract all week categories observed in the dataset
week_levels <- levels(STEPS_5min_df.NIGHT$week)

# Create all combinations of:
# - week category
# - observed context
week_grid <- expand.grid(
  value = week_levels,
  context_id = seq_len(nrow(context_grid)))

# Duplicate baseline row for each prediction
week_covs <- base_covs[rep(1, nrow(week_grid)), ]

# Replace focal variable
week_covs$week <- factor(
  week_grid$value,
  levels = levels(STEPS_5min_df.NIGHT$week))

# Replace contextual variables
week_covs$ALAN <- context_grid$ALAN[week_grid$context_id]
week_covs$SNOW <- context_grid$SNOW[week_grid$context_id]
week_covs$tod_class <- context_grid$tod_class[week_grid$context_id]
week_covs$LAND <- context_grid$LAND[week_grid$context_id]
week_covs$veg_height <- context_grid$veg_height[week_grid$context_id]

# Predict stationary probabilities for every week × context
week_stat <- stationary(
  modfull,
  covs = week_covs)

# Convert to data frame
week_df_context <- as.data.frame(week_stat)
names(week_df_context) <- state_names

# Keep context-level predictions
week_df_context <- week_df_context %>%
  mutate(
    value = week_grid$value,
    context_id = week_grid$context_id,
    weight = context_grid$weight[week_grid$context_id],
    variable = "week") %>%
  
  pivot_longer(
    cols = all_of(state_names),
    names_to = "state",
    values_to = "prob_context")

# Summarise across contexts
week_df <- week_df_context %>%
  
  group_by(
    value,
    variable,
    state) %>%
  
  summarise(
    # Weighted mean prediction
    prob = sum(prob_context * weight, na.rm = TRUE),
    
    # Contextual envelope, not model CI
    lower = quantile(prob_context, 0.25, na.rm = TRUE),
    upper = quantile(prob_context, 0.75, na.rm = TRUE),
    
    .groups = "drop")

# Store
pred_data$categorical$Week <- week_df

# Clean
rm(week_levels, week_grid, week_covs, week_stat, week_df_context, week_df)

#-------------------------------------------------------------------------------

saveRDS(
  pred_data,
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/pred_data.rds")

# Final clean
rm(n, modfull, context_grid, base_covs, pred_data)

########################################################################################################################

#################################################################################
# 8.3 Visualize plots
#################################################################################

# 8.30 LOAD YOUR DATA
######################

pred_data <- readRDS(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/pred_data.rds")

# Names of your hidden states
state_names <- c("Immobile", "Actif", "Transit")

################################################################################

# 8.31 PLOT ALAN (A)
#####################

# 8.311 Plot numerical data
#---------------------------

# Combine numerical data
stat_ALAN <- pred_data$numerical$LIGHT

# State names
stat_ALAN$state <- factor(
  stat_ALAN$state,
  levels = state_names)

# Rename variables
stat_ALAN$variable[
  stat_ALAN$variable == "LIGHT"] <- "Distance au lampadaire le plus proche"

# Plot
n_light <- nrow(STEPS_5min_df.NIGHT)

plot.dist.light <- ggplot(
  data = stat_ALAN,
  aes(
    x = value,
    y = prob,
    colour = state)) +
  
  # Contextual envelope
  geom_ribbon(
    aes(
      ymin = lower,
      ymax = upper,
      fill = state,
      group = state),
    alpha = 0.2,
    colour = NA) +
  
  geom_line(
    linewidth = 1.5) +
    
  scale_color_manual(values = c("darkred", "red", "orange")) +
  scale_fill_manual(values = c("darkred", "red", "orange")) +

  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = paste0("n = ", n_light),
    hjust = -0.1,
    vjust = 1.2,
    size = 3) +
  
  coord_cartesian(ylim = c(0,1)) +  
  
  guides(
    fill = "none") +
    
  labs(
    x = "Distance au lampadaire le plus proche\n(mise à l'échelle individuelle)",
    y = "Probabilite stationnaire",
    colour = "Etat",
    title = "Distance au lampadaire le plus proche") +
    
  theme_minimal() +
  theme(
    legend.position = c(0.8, 0.8),
    axis.title.y = element_blank(),
    axis.text.y = element_blank())

plot.dist.light

#-------------------------------------------------------------------------------

# 8.312 Plot categorical data
#-----------------------------

# Combine categorical data
stat_ALAN <- pred_data$categorical$ALAN

# State names
stat_ALAN$state <- factor(
  stat_ALAN$state,
  levels = state_names)

# Rename variables
stat_ALAN$variable[
  stat_ALAN$variable == "ALAN"] <- "Allumage - Extinction"

# Rename categories
n_ALAN <- STEPS_5min_df.NIGHT %>%
  group_by(ALAN) %>%
  summarise(n = n())

labels_custom <- n_ALAN %>%
  mutate(label = paste0(
      dplyr::recode(ALAN,
                    "ON" = "Allumé",
                    "OFF" = "Eteint"),
    "\n(n=", n, ")"
  )) %>%
  select(ALAN, label) %>%
  deframe()

# Plot
plot.alan <- ggplot(
  data = stat_ALAN,
  aes(
    x = value,
    y = prob,
    fill = state)) +

  # Bars side-by-side
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    linewidth = 0.2) +

  # Error bars
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper),
    position = position_dodge(width = 0.8),
    width = 0.2,
    linewidth = 0.5) +

  scale_fill_manual(
    values = c(
      "Immobile" = "darkred",
      "Actif" = "red",
      "Transit" = "orange")) +

  scale_y_continuous(
    limits = c(0, 0.75),
    expand = expansion(mult = c(0, 0.025))) +

  scale_x_discrete(
    labels = labels_custom) +  
  
  coord_cartesian(ylim = c(0,1)) +

  labs(
    x = "Allumage - Extinction",
    y = "Probabilité d'état",
    fill = "Etat",
    title = "Allumage - Extinction") +

  theme_minimal() +
  
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none")

plot.alan

#-------------------------------------------------------------------------------

# 8.313 Plot interaction data
#-----------------------------

# Interaction data
stat_ALAN <- pred_data$interaction$ALAN_LIGHT

# State names
stat_ALAN$state <- factor(
  stat_ALAN$state,
  levels = state_names)

# ALAN order
stat_ALAN$ALAN <- factor(
  stat_ALAN$ALAN,
  levels = c("OFF", "ON"),
  labels = c("Eteint", "Allumé"))

# Plot
plot.inter.alan.dist <- ggplot(
  stat_ALAN,
  aes(
    x = value,
    y = prob,
    colour = state,
    fill = state,
    linetype = ALAN,
    group = interaction(state, ALAN))) +

  # Contextual envelope
  geom_ribbon(
    aes(
      ymin = lower,
      ymax = upper),
    alpha = 0.1,
    colour = NA) +

  # Mean prediction
  geom_line(
    linewidth = 1.5) +

  facet_wrap(
    ~ state,
    nrow = 1) +

  scale_colour_manual(
    values = c(
      "Immobile" = "darkred",
      "Actif" = "red",
      "Transit" = "orange")) +

  scale_fill_manual(
    values = c(
      "Immobile" = "darkred",
      "Actif" = "red",
      "Transit" = "orange")) +
  
  labs(
    x = "Distance au lampadaire le plus proche\n(mise à l'échelle individuelles)",
    y = "Probabilité d'état",
    colour = "ALAN",
    fill = "ALAN",
    linetype = "ALAN",
    title = "Interaction des variables Allumage - Extinction & Distance au lampadaire le plus proche") +

  theme_minimal() +
  
  guides(
    colour = "none",
    fill = "none") +

  theme(
    panel.grid.minor = element_blank(),
    legend.position = c(0.88, 0.75),
    strip.text = element_text(face = "bold"))

plot.inter.alan.dist

#-------------------------------------------------------------------------------

# 8.314 Combine plots
#---------------------

# Add labels to each plot
plot.inter.A <- arrangeGrob(
  plot.inter.alan.dist,
  top = textGrob("C)", x = unit(0, "npc"), hjust = 0,
                 gp = gpar(fontsize = 16, fontface = "bold")))

plot.cat.B <- arrangeGrob(
  plot.alan,
  top = textGrob("A)", x = unit(0, "npc"), hjust = 0,
                 gp = gpar(fontsize = 16, fontface = "bold")))

plot.num.C <- arrangeGrob(
  plot.dist.light,
  top = textGrob("(B)", x = unit(0, "npc"), hjust = 0,
                 gp = gpar(fontsize = 16, fontface = "bold")))

# Combine
up <- grid.arrange(
  plot.cat.B,
  plot.num.C,
  ncol = 2,
  widths = c(1, 1))

all <- grid.arrange(
  up,
  plot.inter.A,
  ncol = 1,
  top = textGrob(
    "Effet d'ALAN sur le rythme d'activité du lièvre brun",
    gp = gpar(fontsize = 16, fontface = "bold")))

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/Final_alan_distrib.png",
  plot = all,
  width = 8,
  height = 8,
  dpi = 300)

# Clean
rm(all, down, plot.cat.B, plot.inter.A, plot.num.C)
rm(stat_ALAN, plot.alan, plot.inter.alan.dist, plot.dist.light)

################################################################################

# 8.32 PLOT ABIOTICS (B)
#########################

# 8.321 Plot numerical data
#---------------------------

# Combine numerical data
stat_ABIOTIC <- rbind(
  pred_data$numerical$Moon,
  pred_data$numerical$Wind,
  pred_data$numerical$Precipitation,
  pred_data$numerical$Temperature)

# State names
stat_ABIOTIC$state <- factor(
  stat_ABIOTIC$state,
  levels = state_names)

# Rename variables
stat_ABIOTIC$variable[
  stat_ABIOTIC$variable == "moon_fraction"] <- "A) Cyc. lun."
stat_ABIOTIC$variable[
  stat_ABIOTIC$variable == "precip"] <- "B) Prec."
stat_ABIOTIC$variable[
  stat_ABIOTIC$variable == "temp"] <- "C) Temp."
stat_ABIOTIC$variable[
  stat_ABIOTIC$variable == "vent"] <- "D) Vent"

stat_ABIOTIC <- stat_ABIOTIC %>%
  dplyr::mutate(
    variable = dplyr::recode(
      variable,
      "A) Cyc. lun." = "A) Illum. lune",
      "B) Prec." = "B) Précip."))

# Plot
n_temp <- nrow(STEPS_5min_df.NIGHT)

plot.meteo.lune <- ggplot(
  stat_ABIOTIC,
  aes(
    x = value,
    y = prob,
    colour = state)) +

  geom_ribbon(
    aes(
      ymin = lower,
      ymax = upper,
      fill = state,
      group = state),
    alpha = 0.2,
    colour = NA) +

  geom_line(linewidth = 1.5) +

  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = paste0("n = ", n_temp),
    hjust = -0.1,
    vjust = 1.2,
    size = 3) +

  facet_wrap(
    ~ variable,
    scales = "free_x",
    ncol = 4) +

  scale_color_manual(values = c("darkred", "red", "orange")) +
  scale_fill_manual(values = c("darkred", "red", "orange")) +

  guides(fill = "none") +

  labs(
    x = "Conditions abiotiques (mise à l'échelle groupée)",
    y = "Probabilité d'état",
    colour = "Etat") +

  theme_minimal() +
  theme(
    legend.position = c(0.9, 0.5),
    strip.text = element_text(face = "bold", size = 12))

plot.meteo.lune

#-------------------------------------------------------------------------------

# 8.322 Plot categorical data
#-----------------------------

# Combine categorical data
stat_ABIOTIC <- pred_data$categorical$Snow

# State names
stat_ABIOTIC$state <- factor(
  stat_ABIOTIC$state,
  levels = state_names)

# Rename variables
stat_ABIOTIC$variable[
  stat_ABIOTIC$variable == "Snow"] <- "Couv. neig."

# Plot
n_snow <- STEPS_5min_df.NIGHT %>%
  group_by(SNOW) %>%
  summarise(n = n())

labels_custom <- n_snow %>%
  mutate(label = paste0(
    SNOW,
    "\n(n=", n, ")"
  )) %>%
  select(SNOW, label) %>%
  deframe()

plot.neige <- ggplot(
  data = stat_ABIOTIC,
  aes(
    x = value,
    y = prob,
    fill = state)) +

  # Bars side-by-side
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    linewidth = 0.2) +

  # Error bars
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper),
    position = position_dodge(width = 0.8),
    width = 0.2,
    linewidth = 0.5) +

  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = "E) Couv. neigeuse",
    hjust = -0.1,
    vjust = 1.5,
    fontface = "bold",
    size = 4) +

  scale_fill_manual(
    values = c(
      "Immobile" = "darkred",
      "Actif" = "red",
      "Transit" = "orange")) +

  scale_y_continuous(
    limits = c(0, 0.75),
    expand = expansion(mult = c(0, 0.025))) +
  
  scale_x_discrete(
    labels = labels_custom) +

  labs(
    x = "Couverture neigeuse",
    y = "Probabilité d'état",
    fill = "Etat") +

  theme_minimal() +
  
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none")

plot.neige

#-------------------------------------------------------------------------------

# 8.323 Combine plots
#---------------------

plot.meteo.neige.lune <- grid.arrange(
  plot.meteo.lune,
  plot.neige, 
  ncol = 2, 
  widths = c(1.75, 1),
  top = textGrob(
    "Effet de la météo et de la lune sur le rythme d'activité du lièvre brun",
    gp = gpar(fontsize = 16, fontface = "bold")))

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/Final_météo_lune_distrib.png",
  plot = plot.meteo.neige.lune,
  width = 8,
  height = 5,
  dpi = 300)

# Clean
rm(all, down, plot.neige, plot.meteo.lune, plot.meteo.neige.lune, stat_ABIOTIC)

################################################################################

# 8.33 PLOT HABITAT (C)
#########################

# 8.331 Habitat
# --------------

# Combine categorical data
stat_ABIOTIC <- pred_data$categorical$Habitat

# State names
stat_ABIOTIC$state <- factor(
  stat_ABIOTIC$state,
  levels = state_names)

# Rename variables
stat_ABIOTIC$variable[
  stat_ABIOTIC$variable == "Habitat"] <- "Utilisation de surface"

# Rename categories
n_land <- STEPS_5min_df.NIGHT %>%
  group_by(LAND) %>%
  summarise(n = n())

labels_custom <- n_land %>%
  mutate(label = paste0(
    case_when(
      LAND == "1" ~ "Chaume,\nCuture inconnue,\nFriche, Jachère",
      LAND == "3" ~ "Colza",
      LAND == "6" ~ "Forêt",
      LAND == "8" ~ "Gravière",
      LAND == "9" ~ "Haie",
      LAND == "11" ~ "Labour",
      LAND == "12" ~ "Pâture",
      LAND == "13" ~ "Prairie extensive",
      LAND == "14" ~ "Prairie intensive",
      LAND == "15" ~ "Semis inconnu"
    ),
    "\n(n=", n, ")"
  )) %>%
  select(LAND, label) %>%
  deframe()

# Plot
plot.habitat <- ggplot(
  data = stat_ABIOTIC,
  aes(
    x = value,
    y = prob,
    fill = state)) +

  # Bars side-by-side
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    linewidth = 0.2) +

  # Error bars
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper),
    position = position_dodge(width = 0.8),
    width = 0.2,
    linewidth = 0.5) +

  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = "A) Utilisation de surface",
    hjust = -0.1,
    vjust = 1.5,
    fontface = "bold",
    size = 4) +

  scale_fill_manual(
    values = c(
      "Immobile" = "darkred",
      "Actif" = "red",
      "Transit" = "orange")) +

  scale_x_discrete(
    labels = labels_custom) +

  scale_y_continuous(
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.025))) +
  
  labs(
    x = "Utilisation de surface",
    y = "Probabilité d'état",
    fill = "Etat") +

  theme_minimal() +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "none")

plot.habitat

rm(stat_ABIOTIC)

#------------------------------------------------------

# 8.332 Vege height
# -------------------

# Combine categorical data
stat_ABIOTIC <- pred_data$categorical$VegHeight

# State names
stat_ABIOTIC$state <- factor(
  stat_ABIOTIC$state,
  levels = state_names)

# Rename variables
stat_ABIOTIC$variable[
  stat_ABIOTIC$variable == "VegHeight"] <- "Hauteur de végétation"

# Plot
n_veg <- STEPS_5min_df.NIGHT %>%
  group_by(veg_height) %>%
  summarise(n = n())

labels_custom <- n_veg %>%
  mutate(label = paste0(
    case_when(
      veg_height == "200" ~ "200+",
      TRUE ~ veg_height),
    "\n(n=", n, ")"
  )) %>%
  select(veg_height, label) %>%
  deframe()

plot.vege <- ggplot(
  data = stat_ABIOTIC,
  aes(
    x = value,
    y = prob,
    fill = state)) +

  # Bars side-by-side
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    linewidth = 0.2) +

  # Error bars
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper),
    position = position_dodge(width = 0.8),
    width = 0.2,
    linewidth = 0.5) +

  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = "B) Hauteur de végétation",
    hjust = -0.1,
    vjust = 1.5,
    fontface = "bold",
    size = 4) +

  scale_fill_manual(
    values = c(
      "Immobile" = "darkred",
      "Actif" = "red",
      "Transit" = "orange")) +

  scale_y_continuous(
    limits = c(0, 0.8),
    expand = expansion(mult = c(0, 0.025))) +

  scale_x_discrete(
    labels = labels_custom) +
  
  labs(
    x = "Hauteur de végétation (cm)",
    y = "Probabilité d'état",
    fill = "Etat") +

  theme_minimal() +
  
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none")

plot.vege

#-------------------------------------------------------
# 8.333 Combine plots
#---------------------

plot.hab.vege <- grid.arrange(
  plot.habitat,
  plot.vege,
  nrow = 2,
  heights = c(1.5, 1.5),
  top = textGrob(
    "Effet du paysage sur le rythme d'activité du lièvre brun",
    gp = gpar(fontsize = 16, fontface = "bold")))

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/Final_habitat_distrib.png",
  plot = plot.hab.vege,
  width = 8,
  height = 8,
  dpi = 300)

# Clean
rm(all, down, plot.neige, plot.meteo.lune, plot.meteo.neige.lune, stat_ABIOTIC)

#############################################################################

# 8.34 PLOT TEMPORAL DATA (D)
##############################

# 8.341 TOD
# ----------

# Combine categorical data
stat_ABIOTIC <- pred_data$categorical$TOD

# Remove diurnal categories
unique(stat_ABIOTIC$value)

stat_ABIOTIC <- pred_data$categorical$TOD %>%
  filter(!value %in% c("MATIN : sunrise - 11H00", "MIDI : 11H00 - 14H00",            
                      "APRES-MIDI : 14H00 - 16H00", "COUCHER DU JOUR : 16H00 - sunset"))

# State names
stat_ABIOTIC$state <- factor(
  stat_ABIOTIC$state,
  levels = state_names)

# Rename variables
stat_ABIOTIC$variable[
  stat_ABIOTIC$variable == "TOD"] <- "Période de la journée"

# Plot
n_tod <- STEPS_5min_df.NIGHT %>%
  group_by(tod_class) %>%
  summarise(n = n())

labels_custom <- n_tod %>%
  mutate(label = paste0(
    case_when(
      tod_class == "SOIR : sunset - 23H00" ~ "SOIR :\nsunset - 23H00",
      tod_class == "NUIT : 23H00 - 03H00" ~ "1e part. NUIT :\n23H00 - 03H00",
      tod_class == "NUIT : 03H00 - 06H00" ~ "2e part. NUIT :\n03H00 - 06H00",
      tod_class == "LEVER DU JOUR : 06H00 - sunrise" ~ "LEVER DU JOUR :\n06H00 - sunrise"),
    "\n(n=", n, ")"
  )) %>%
  select(tod_class, label) %>%
  deframe()

plot.tod <- ggplot(
  data = stat_ABIOTIC,
  aes(
    x = value,
    y = prob,
    fill = state)) +

  # Bars side-by-side
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    linewidth = 0.2) +

  # Error bars
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper),
    position = position_dodge(width = 0.8),
    width = 0.2,
    linewidth = 0.5) +

  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = "A) Période de la nuit",
    hjust = -0.1,
    vjust = 1.5,
    fontface = "bold",
    size = 4) +

  scale_fill_manual(
    values = c(
      "Immobile" = "darkred",
      "Actif" = "red",
      "Transit" = "orange")) +

  scale_y_continuous(
    limits = c(0, 0.75),
    expand = expansion(mult = c(0, 0.025))) +

  scale_x_discrete(
    labels = labels_custom) +
  
  labs(
    x = "Période de la nuit",
    y = "Probabilité d'état",
    fill = "Etat") +

  theme_minimal() +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "none")

plot.tod

rm(stat_ABIOTIC)

###################################################################

# 8.342 Week
# -----------

# Combine categorical data
stat_ABIOTIC <- pred_data$categorical$Week

# State names
stat_ABIOTIC$state <- factor(
  stat_ABIOTIC$state,
  levels = state_names)

# Rename variables
stat_ABIOTIC$variable[
  stat_ABIOTIC$variable == "Week"] <- "Semaine de l'année"

# Plot
n_week <- STEPS_5min_df.NIGHT %>%
  group_by(week) %>%
  summarise(n = n())

labels_custom <- n_week %>%
  mutate(label = paste0(
    dplyr::recode(week,
        "Week49_2025" = "Semaine 49_2025",
        "Week50_2025" = "Semaine 50_2025",
        "Week51_2025" = "Semaine 51_2025",
        "Week52_2025" = "Semaine 52_2025",
        "Week1_2026" = "Semaine 1_2026",
        "Week2_2026" = "Semaine 2_2026",
        "Week3_2026" = "Semaine 3_2026",
        "Week4_2026" = "Semaine 4_2026",
        "Week5_2026" = "Semaine 5_2026",
        "Week6_2026" = "Semaine 6_2026",
        "Week7_2026" = "Semaine 7_2026",
        "Week8_2026" = "Semaine 8_2026",
        "Week9_2026" = "Semaine 9_2026",
        "Week10_2026" = "Semaine 10_2026",
        "Week11_2026" = "Semaine 11_2026",
        "Week12_2026" = "Semaine 12_2026",
        "Week13_2026" = "Semaine 13_2026"),
    "\n(n=", n, ")")
  ) %>%
  select(week, label) %>%
  deframe()

plot.week <- ggplot(
  data = stat_ABIOTIC,
  aes(
    x = value,
    y = prob,
    fill = state)) +

  # Bars side-by-side
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    linewidth = 0.2) +

  # Error bars
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper),
    position = position_dodge(width = 0.8),
    width = 0.2,
    linewidth = 0.5) +

  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = "B) Semaine de l'année",
    hjust = -0.1,
    vjust = 1.5,
    fontface = "bold",
    size = 4) +

  scale_fill_manual(
    values = c(
      "Immobile" = "darkred",
      "Actif" = "red",
      "Transit" = "orange")) +

  scale_x_discrete(
    labels = labels_custom) +

  scale_y_continuous(
    limits = c(0, 0.85),
    expand = expansion(mult = c(0, 0.025))) +
  
  labs(
    x = "Semaine de l'année",
    y = "Probabilité d'état",
    fill = "Etat") +

  theme_minimal() +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "none")

plot.week

#-------------------------------------------------------
# 8.343 Combine plots
#---------------------

plot.temporal <- grid.arrange(
  plot.tod,
  plot.week,
  nrow = 2,
  heights = c(1.5, 1.5),
  top = textGrob(
    "Effet de la temporalité sur le rythme d'activité du lièvre brun",
    gp = gpar(fontsize = 16, fontface = "bold")))

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/Final_temporal_distrib.png",
  plot = plot.temporal,
  width = 8,
  height = 8,
  dpi = 300)

# Clean
rm(plot.tod, plot.week, plot.temporal, stat_ABIOTIC)
