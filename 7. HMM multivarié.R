# 7.10 How do hare noctural activity patterns vary across all variable ?
#--------------------------------------------------------------------------
STEPS_5min_df.NIGHT <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/STEPS_5min_3states_night.rds")

dist <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/dist.rds")

mod30 <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/mod30.rds")

stateNames <- c("resting", "foraging", "exploring")
nState <- 3

#----------------------------------------------------
# A) MODEL
#----------------------------------------------------
# or read it...
modMulti_full <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMulti_full.rds")

formulaMulti_full <- ~ (ALAN * LIGHT) + week + tod_class + LAND + veg_height + moon_fraction + temp + precip + vent + SNOW

ParMulti_full <- getPar0(
  model = mod30,
  formula = formulaMulti_full)

modMulti_full <- fitHMM(
  data = STEPS_5min_df.NIGHT,
  nbState = nState,
  stateNames = stateNames,
  dist = dist,
  Par0 = ParMulti_full$Par,
  beta0 = ParMulti_full$beta,
  formula = formulaMulti_full,
  retryFits = 1)

modMulti_full

saveRDS(
  modMulti_full,
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMulti_full.rds")

#----------------------------------------------------
# B) PREDICT STATE PROBABILITIES (ALAN × LIGHT)
#----------------------------------------------------

alan_levels <- levels(STEPS_5min_df.NIGHT$ALAN)

light_seq <- seq(
  min(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  max(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  length.out = 20)

# valeurs de référence pour les autres variables
ref_vals <- data.frame(
  ALAN = factor(levels(STEPS_5min_df.NIGHT$ALAN)[1], levels = levels(STEPS_5min_df.NIGHT$ALAN)),
  LIGHT = mean(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  week = factor(levels(STEPS_5min_df.NIGHT$week)[1], levels = levels(STEPS_5min_df.NIGHT$week)),
  tod_class = factor(levels(STEPS_5min_df.NIGHT$tod_class)[1], levels = levels(STEPS_5min_df.NIGHT$tod_class)),
  LAND = factor(levels(STEPS_5min_df.NIGHT$LAND)[1], levels = levels(STEPS_5min_df.NIGHT$LAND)),
  veg_height = factor(levels(STEPS_5min_df.NIGHT$veg_height)[1], levels = levels(STEPS_5min_df.NIGHT$veg_height)),
  moon_fraction = mean(STEPS_5min_df.NIGHT$moon_fraction, na.rm = TRUE),
  temp = mean(STEPS_5min_df.NIGHT$temp, na.rm = TRUE),
  precip = mean(STEPS_5min_df.NIGHT$precip, na.rm = TRUE),
  vent = mean(STEPS_5min_df.NIGHT$vent, na.rm = TRUE),
  SNOW = factor(levels(STEPS_5min_df.NIGHT$SNOW)[1], levels = levels(STEPS_5min_df.NIGHT$SNOW)))

statALAN_LIGHT_list <- list()

pdf(NULL)

for(a in alan_levels){
  for(l in light_seq){
    
    newdata <- ref_vals
    newdata$ALAN <- a
    newdata$LIGHT <- l
    
    s <- plotStationary(
      modMulti_full,
      covs = newdata,
      plotCI = TRUE,
      return = TRUE)
    
    out <- rbind(
      cbind(s[[1]], state = stateNames[1]),
      cbind(s[[2]], state = stateNames[2]),
      cbind(s[[3]], state = stateNames[3]))
    
    out$ALAN <- a
    out$LIGHT <- l
    
    statALAN_LIGHT_list[[length(statALAN_LIGHT_list) + 1]] <- out 
  }
}

statALAN_LIGHT_df <- do.call(rbind, statALAN_LIGHT_list)

statALAN_LIGHT_df$state <- factor(statALAN_LIGHT_df$state, levels = stateNames)

#-------------------------------------------------------------------------------

# C) VISUALIZE
#-------------

# 1) Recharger données NON standardisées (pour échelle réelle)
HARES_5min_df.NIGHT <- readRDS(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/Hares_5min_night.rds")

# 2) Reconstituer l'échelle réelle de LIGHT
light_mean <- mean(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)
light_sd   <- sd(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)

# 3) Définir des labels lisibles (en mètres)
light_labels <- seq(
  floor(min(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)),
  ceiling(max(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)),
  by = 200)

# 4) Transformer en échelle standardisée (pour correspondre au modèle)
light_breaks <- (light_labels - light_mean) / light_sd

# 5) Nombre d’observations par condition ALAN
n_ALAN <- STEPS_5min_df.NIGHT %>%
  group_by(ALAN) %>%
  summarise(n = n())

labels_custom <- n_ALAN %>%
  mutate(label = paste0(ALAN, "\n(n=", n, ")")) %>%
  select(ALAN, label) %>%
  deframe()

# FIGURE
#-------

multi_full_distrib <- ggplot(
  data = statALAN_LIGHT_df,
  aes(
    x = LIGHT,
    y = est,
    color = state,
    fill = state,
    linetype = ALAN)) +

  geom_ribbon(
    aes(ymin = lci, ymax = uci),
    alpha = 0.15,
    colour = NA) +

  geom_line(linewidth = 1) +

  facet_grid(state ~ .) +

  scale_x_continuous(
    breaks = light_breaks,
    labels = light_labels) +

  scale_color_manual(values = c("darkred", "red", "orange")) +
  scale_fill_manual(values = c("darkred", "red", "orange")) +

  scale_linetype_manual(
    values = c(
      "OFF" = "solid",
      "ON" = "dotted"),
    labels = c("OFF", "ON")) +

  labs(
    x = "Distance to nearest lamp (m)",
    y = "Stationary state probability",
    color = "State",
    fill = "State",
    linetype = "ALAN",
    title = "Interactive effect of ALAN and distance to artificial light on behavioural states") +

  theme_minimal() +

  theme(
    legend.position = c(0.85, 0.75),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")) 

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/multi_full_distrib.png",
  plot = multi_full_distrib,
  width = 8,
  height = 8,
  dpi = 300)

# CLEAN
rm(b, cb, est, lower, newcovs, statALAN_LIGHT_list, upper, i, modMulti_full, ParMulti_full, formulaMulti_full)
rm(statALAN_LIGHT_list, statALAN_LIGHT_df, light_seq, ref_vals, n_ALAN, labels_custom, multi_full_distrib)

#############################################################################################################

# 7.11 How do hare noctural activity patterns vary across all variable without "WEEK" ?
#---------------------------------------------------------------------------------------

# A) MODEL
#----------------------------------------------------
# or read it...
modMulti_WEEK <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMulti_WEEK.rds")

formulaMulti_WEEK <- ~ (ALAN * LIGHT) + tod_class + LAND + veg_height + moon_fraction + temp + precip + vent + SNOW

ParMulti_WEEK <- getPar0(
  model = mod30,
  formula = formulaMulti_WEEK)

modMulti_WEEK <- fitHMM(
  data = STEPS_5min_df.NIGHT,
  nbState = nState,
  stateNames = stateNames,
  dist = dist,
  Par0 = ParMulti_WEEK$Par,
  beta0 = ParMulti_WEEK$beta,
  formula = formulaMulti_WEEK,
  retryFits = 1)

modMulti_WEEK

saveRDS(
  modMulti_WEEK,
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMulti_WEEK.rds")

#----------------------------------------------------
# B) PREDICT STATE PROBABILITIES (ALAN × LIGHT)
#----------------------------------------------------

alan_levels <- levels(STEPS_5min_df.NIGHT$ALAN)

light_seq <- seq(
  min(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  max(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  length.out = 20)

# valeurs de référence pour les autres variables
ref_vals <- data.frame(
  ALAN = factor(levels(STEPS_5min_df.NIGHT$ALAN)[1], levels = levels(STEPS_5min_df.NIGHT$ALAN)),
  LIGHT = mean(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  tod_class = factor(levels(STEPS_5min_df.NIGHT$tod_class)[1], levels = levels(STEPS_5min_df.NIGHT$tod_class)),
  LAND = factor(levels(STEPS_5min_df.NIGHT$LAND)[1], levels = levels(STEPS_5min_df.NIGHT$LAND)),
  veg_height = factor(levels(STEPS_5min_df.NIGHT$veg_height)[1], levels = levels(STEPS_5min_df.NIGHT$veg_height)),
  moon_fraction = mean(STEPS_5min_df.NIGHT$moon_fraction, na.rm = TRUE),
  temp = mean(STEPS_5min_df.NIGHT$temp, na.rm = TRUE),
  precip = mean(STEPS_5min_df.NIGHT$precip, na.rm = TRUE),
  vent = mean(STEPS_5min_df.NIGHT$vent, na.rm = TRUE),
  SNOW = factor(levels(STEPS_5min_df.NIGHT$SNOW)[1], levels = levels(STEPS_5min_df.NIGHT$SNOW)))

statALAN_LIGHT_list <- list()

for(a in alan_levels){
  for(l in light_seq){
    
    newdata <- ref_vals
    newdata$ALAN <- a
    newdata$LIGHT <- l
    
    s <- plotStationary(
      modMulti_WEEK,
      covs = newdata,
      plotCI = TRUE,
      return = TRUE)
    
    out <- rbind(
      cbind(s[[1]], state = stateNames[1]),
      cbind(s[[2]], state = stateNames[2]),
      cbind(s[[3]], state = stateNames[3]))
    
    out$ALAN <- a
    out$LIGHT <- l
    
    statALAN_LIGHT_list[[length(statALAN_LIGHT_list) + 1]] <- out 
  }
}

statALAN_LIGHT_df <- do.call(rbind, statALAN_LIGHT_list)

statALAN_LIGHT_df$state <- factor(statALAN_LIGHT_df$state, levels = stateNames)

#-------------------------------------------------------------------------------

# C) VISUALIZE
#-------------

# 1) Recharger données NON standardisées (pour échelle réelle)
HARES_5min_df.NIGHT <- readRDS(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/Hares_5min_night.rds")

# 2) Reconstituer l'échelle réelle de LIGHT
light_mean <- mean(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)
light_sd   <- sd(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)

# 3) Définir des labels lisibles (en mètres)
light_labels <- seq(
  floor(min(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)),
  ceiling(max(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)),
  by = 200)

# 4) Transformer en échelle standardisée (pour correspondre au modèle)
light_breaks <- (light_labels - light_mean) / light_sd

# 5) Nombre d’observations par condition ALAN
n_ALAN <- STEPS_5min_df.NIGHT %>%
  group_by(ALAN) %>%
  summarise(n = n())

labels_custom <- n_ALAN %>%
  mutate(label = paste0(ALAN, "\n(n=", n, ")")) %>%
  select(ALAN, label) %>%
  deframe()

# FIGURE
#-------

multi_WEEK_distrib <- ggplot(
  data = statALAN_LIGHT_df,
  aes(
    x = LIGHT,
    y = est,
    color = state,
    fill = state,
    linetype = ALAN)) +

  geom_ribbon(
    aes(ymin = lci, ymax = uci),
    alpha = 0.15,
    colour = NA) +

  geom_line(linewidth = 1) +

  facet_grid(state ~ .) +

  scale_x_continuous(
    breaks = light_breaks,
    labels = light_labels) +

  scale_color_manual(values = c("darkred", "red", "orange")) +
  scale_fill_manual(values = c("darkred", "red", "orange")) +

  scale_linetype_manual(
    values = c(
      "OFF" = "solid",
      "ON" = "dotted"),
    labels = c("OFF", "ON")) +

  labs(
    x = "Distance to nearest lamp (m)",
    y = "Stationary state probability",
    color = "State",
    fill = "State",
    linetype = "ALAN",
    title = "Interactive effect of ALAN and distance to artificial light on behavioural states") +

  theme_minimal() +

  theme(
    legend.position = c(0.85, 0.75),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")) 

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/multi_WEEK_distrib.png",
  plot = multi_WEEK_distrib,
  width = 8,
  height = 8,
  dpi = 300)

#--------------------------------------------------------------------------------------------------------

# D) AICs COMPARAISON
# -------------------
modMulti_full <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMulti_full.rds")

# AICs
aic_Multi_week <- AIC(modMulti_full, modMulti_WEEK)

# Comparaison
delta_Multi_week <- with(aic_Multi_week, AIC[Model == "modMulti_full"] - AIC[Model == "modMulti_WEEK"])

# Visualisation (delta > 2 ?)
delta_Multi_week

# CLEAN
rm(b, cb, est, lower, newcovs, statALAN_LIGHT_list, upper, i, modMulti_WEEK, ParMulti_WEEK, formulaMulti_WEEK)
rm(statALAN_LIGHT_list, statALAN_LIGHT_df, light_seq, ref_vals, n_ALAN, labels_custom, multi_distrib)

###############################################################################################################

# 7.11 How do hare noctural activity patterns vary across all variable without "TOD" ?
#---------------------------------------------------------------------------------------

# A) MODEL
#----------------------------------------------------
# or read it...
modMulti_TOD <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMulti_TOD.rds")

formulaMulti_TOD <- ~ (ALAN * LIGHT) + week + LAND + veg_height + moon_fraction + temp + precip + vent + SNOW

ParMulti_TOD <- getPar0(
  model = mod30,
  formula = formulaMulti_TOD)

modMulti_TOD <- fitHMM(
  data = STEPS_5min_df.NIGHT,
  nbState = nState,
  stateNames = stateNames,
  dist = dist,
  Par0 = ParMulti_TOD$Par,
  beta0 = ParMulti_TOD$beta,
  formula = formulaMulti_TOD,
  retryFits = 1)

modMulti_TOD

saveRDS(
  modMulti_TOD,
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMulti_TOD.rds")

#----------------------------------------------------
# B) PREDICT STATE PROBABILITIES (ALAN × LIGHT)
#----------------------------------------------------

alan_levels <- levels(STEPS_5min_df.NIGHT$ALAN)

light_seq <- seq(
  min(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  max(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  length.out = 20)

# valeurs de référence pour les autres variables
ref_vals <- data.frame(
  ALAN = factor(levels(STEPS_5min_df.NIGHT$ALAN)[1], levels = levels(STEPS_5min_df.NIGHT$ALAN)),
  LIGHT = mean(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  week = factor(levels(STEPS_5min_df.NIGHT$week)[1], levels = levels(STEPS_5min_df.NIGHT$week)),
  LAND = factor(levels(STEPS_5min_df.NIGHT$LAND)[1], levels = levels(STEPS_5min_df.NIGHT$LAND)),
  veg_height = factor(levels(STEPS_5min_df.NIGHT$veg_height)[1], levels = levels(STEPS_5min_df.NIGHT$veg_height)),
  moon_fraction = mean(STEPS_5min_df.NIGHT$moon_fraction, na.rm = TRUE),
  temp = mean(STEPS_5min_df.NIGHT$temp, na.rm = TRUE),
  precip = mean(STEPS_5min_df.NIGHT$precip, na.rm = TRUE),
  vent = mean(STEPS_5min_df.NIGHT$vent, na.rm = TRUE),
  SNOW = factor(levels(STEPS_5min_df.NIGHT$SNOW)[1], levels = levels(STEPS_5min_df.NIGHT$SNOW)))

statALAN_LIGHT_list <- list()

for(a in alan_levels){
  for(l in light_seq){
    
    newdata <- ref_vals
    newdata$ALAN <- a
    newdata$LIGHT <- l
    
    s <- plotStationary(
      modMulti_TOD,
      covs = newdata,
      plotCI = TRUE,
      return = TRUE)
    
    out <- rbind(
      cbind(s[[1]], state = stateNames[1]),
      cbind(s[[2]], state = stateNames[2]),
      cbind(s[[3]], state = stateNames[3]))
    
    out$ALAN <- a
    out$LIGHT <- l
    
    statALAN_LIGHT_list[[length(statALAN_LIGHT_list) + 1]] <- out 
  }
}

statALAN_LIGHT_df <- do.call(rbind, statALAN_LIGHT_list)

statALAN_LIGHT_df$state <- factor(statALAN_LIGHT_df$state, levels = stateNames)

#-------------------------------------------------------------------------------

# C) VISUALIZE
#-------------

# 1) Recharger données NON standardisées (pour échelle réelle)
HARES_5min_df.NIGHT <- readRDS(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/Hares_5min_night.rds")

# 2) Reconstituer l'échelle réelle de LIGHT
light_mean <- mean(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)
light_sd   <- sd(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)

# 3) Définir des labels lisibles (en mètres)
light_labels <- seq(
  floor(min(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)),
  ceiling(max(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)),
  by = 200)

# 4) Transformer en échelle standardisée (pour correspondre au modèle)
light_breaks <- (light_labels - light_mean) / light_sd

# 5) Nombre d’observations par condition ALAN
n_ALAN <- STEPS_5min_df.NIGHT %>%
  group_by(ALAN) %>%
  summarise(n = n())

labels_custom <- n_ALAN %>%
  mutate(label = paste0(ALAN, "\n(n=", n, ")")) %>%
  select(ALAN, label) %>%
  deframe()

# FIGURE
#-------

multi_TOD_distrib <- ggplot(
  data = statALAN_LIGHT_df,
  aes(
    x = LIGHT,
    y = est,
    color = state,
    fill = state,
    linetype = ALAN)) +

  geom_ribbon(
    aes(ymin = lci, ymax = uci),
    alpha = 0.15,
    colour = NA) +

  geom_line(linewidth = 1) +

  facet_grid(state ~ .) +

  scale_x_continuous(
    breaks = light_breaks,
    labels = light_labels) +

  scale_color_manual(values = c("darkred", "red", "orange")) +
  scale_fill_manual(values = c("darkred", "red", "orange")) +

  scale_linetype_manual(
    values = c(
      "OFF" = "solid",
      "ON" = "dotted"),
    labels = c("OFF", "ON")) +

  labs(
    x = "Distance to nearest lamp (m)",
    y = "Stationary state probability",
    color = "State",
    fill = "State",
    linetype = "ALAN",
    title = "Interactive effect of ALAN and distance to artificial light on behavioural states") +

  theme_minimal() +

  theme(
    legend.position = c(0.85, 0.75),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")) 

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/multi_TOD_distrib.png",
  plot = multi_TOD_distrib,
  width = 8,
  height = 8,
  dpi = 300)

#--------------------------------------------------------------------------------------------------------

# D) AICs COMPARAISON
# -------------------

# AICs
aic_Multi_TOD <- AIC(modMulti_full, modMulti_TOD)

# Comparaison
delta_Multi_TOD <- with(aic_Multi_TOD, AIC[Model == "modMulti_full"] - AIC[Model == "modMulti_TOD"])

# Visualisation (delta > 2 ?)
delta_Multi_TOD

# CLEAN
rm(b, cb, est, lower, newcovs, statALAN_LIGHT_list, upper, i, modMulti_TOD, ParMulti_TOD, formulaMulti_TOD)
rm(statALAN_LIGHT_list, statALAN_LIGHT_df, light_seq, ref_vals, n_ALAN, labels_custom, multi_TOD_distrib)

###############################################################################################################

# 7.12 How do hare noctural activity patterns vary across all variable without "LAND" ?
#---------------------------------------------------------------------------------------

# A) MODEL
#----------------------------------------------------
# or read it...
modMulti_LAND <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMulti_LAND.rds")

formulaMulti_LAND <- ~ (ALAN * LIGHT) + week + tod_class + veg_height + moon_fraction + temp + precip + vent + SNOW

ParMulti_LAND <- getPar0(
  model = mod30,
  formula = formulaMulti_LAND)

modMulti_LAND <- fitHMM(
  data = STEPS_5min_df.NIGHT,
  nbState = nState,
  stateNames = stateNames,
  dist = dist,
  Par0 = ParMulti_LAND$Par,
  beta0 = ParMulti_LAND$beta,
  formula = formulaMulti_LAND,
  retryFits = 1)

modMulti_LAND

saveRDS(
  modMulti_LAND,
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMulti_LAND.rds")

#----------------------------------------------------
# B) PREDICT STATE PROBABILITIES (ALAN × LIGHT)
#----------------------------------------------------

alan_levels <- levels(STEPS_5min_df.NIGHT$ALAN)

light_seq <- seq(
  min(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  max(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  length.out = 20)

# valeurs de référence pour les autres variables
ref_vals <- data.frame(
  ALAN = factor(levels(STEPS_5min_df.NIGHT$ALAN)[1], levels = levels(STEPS_5min_df.NIGHT$ALAN)),
  LIGHT = mean(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  week = factor(levels(STEPS_5min_df.NIGHT$week)[1], levels = levels(STEPS_5min_df.NIGHT$week)),
  tod_class = factor(levels(STEPS_5min_df.NIGHT$tod_class)[1], levels = levels(STEPS_5min_df.NIGHT$tod_class)),
  veg_height = factor(levels(STEPS_5min_df.NIGHT$veg_height)[1], levels = levels(STEPS_5min_df.NIGHT$veg_height)),
  moon_fraction = mean(STEPS_5min_df.NIGHT$moon_fraction, na.rm = TRUE),
  temp = mean(STEPS_5min_df.NIGHT$temp, na.rm = TRUE),
  precip = mean(STEPS_5min_df.NIGHT$precip, na.rm = TRUE),
  vent = mean(STEPS_5min_df.NIGHT$vent, na.rm = TRUE),
  SNOW = factor(levels(STEPS_5min_df.NIGHT$SNOW)[1], levels = levels(STEPS_5min_df.NIGHT$SNOW)))

statALAN_LIGHT_list <- list()

for(a in alan_levels){
  for(l in light_seq){
    
    newdata <- ref_vals
    newdata$ALAN <- a
    newdata$LIGHT <- l
    
    s <- plotStationary(
      modMulti_LAND,
      covs = newdata,
      plotCI = TRUE,
      return = TRUE)
    
    out <- rbind(
      cbind(s[[1]], state = stateNames[1]),
      cbind(s[[2]], state = stateNames[2]),
      cbind(s[[3]], state = stateNames[3]))
    
    out$ALAN <- a
    out$LIGHT <- l
    
    statALAN_LIGHT_list[[length(statALAN_LIGHT_list) + 1]] <- out 
  }
}

statALAN_LIGHT_df <- do.call(rbind, statALAN_LIGHT_list)

statALAN_LIGHT_df$state <- factor(statALAN_LIGHT_df$state, levels = stateNames)

#-------------------------------------------------------------------------------

# C) VISUALIZE
#-------------

# 1) Recharger données NON standardisées (pour échelle réelle)
HARES_5min_df.NIGHT <- readRDS(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/Hares_5min_night.rds")

# 2) Reconstituer l'échelle réelle de LIGHT
light_mean <- mean(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)
light_sd   <- sd(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)

# 3) Définir des labels lisibles (en mètres)
light_labels <- seq(
  floor(min(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)),
  ceiling(max(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)),
  by = 200)

# 4) Transformer en échelle standardisée (pour correspondre au modèle)
light_breaks <- (light_labels - light_mean) / light_sd

# 5) Nombre d’observations par condition ALAN
n_ALAN <- STEPS_5min_df.NIGHT %>%
  group_by(ALAN) %>%
  summarise(n = n())

labels_custom <- n_ALAN %>%
  mutate(label = paste0(ALAN, "\n(n=", n, ")")) %>%
  select(ALAN, label) %>%
  deframe()

# FIGURE
#-------

multi_LAND_distrib <- ggplot(
  data = statALAN_LIGHT_df,
  aes(
    x = LIGHT,
    y = est,
    color = state,
    fill = state,
    linetype = ALAN)) +

  geom_ribbon(
    aes(ymin = lci, ymax = uci),
    alpha = 0.15,
    colour = NA) +

  geom_line(linewidth = 1) +

  facet_grid(state ~ .) +

  scale_x_continuous(
    breaks = light_breaks,
    labels = light_labels) +

  scale_color_manual(values = c("darkred", "red", "orange")) +
  scale_fill_manual(values = c("darkred", "red", "orange")) +

  scale_linetype_manual(
    values = c(
      "OFF" = "solid",
      "ON" = "dotted"),
    labels = c("OFF", "ON")) +

  labs(
    x = "Distance to nearest lamp (m)",
    y = "Stationary state probability",
    color = "State",
    fill = "State",
    linetype = "ALAN",
    title = "Interactive effect of ALAN and distance to artificial light on behavioural states") +

  theme_minimal() +

  theme(
    legend.position = c(0.85, 0.75),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")) 

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/multi_LAND_distrib.png",
  plot = multi_LAND_distrib,
  width = 8,
  height = 8,
  dpi = 300)

#--------------------------------------------------------------------------------------------------------

# D) AICs COMPARAISON
# -------------------

# AICs
aic_Multi_LAND <- AIC(modMulti_full, modMulti_LAND)

# Comparaison
delta_Multi_LAND <- with(aic_Multi_LAND, AIC[Model == "modMulti_full"] - AIC[Model == "modMulti_LAND"])

# Visualisation (delta > 2 ?)
delta_Multi_LAND

# CLEAN
rm(b, cb, est, lower, newcovs, statALAN_LIGHT_list, upper, i, modMulti_LAND, ParMulti_LAND, formulaMulti_LAND)
rm(statALAN_LIGHT_list, statALAN_LIGHT_df, light_seq, ref_vals, n_ALAN, labels_custom, multi_LAND_distrib)

####################################################################################################

# 7.13 How do hare noctural activity patterns vary across all variable without "veg_height" ?
#---------------------------------------------------------------------------------------

# A) MODEL
#----------------------------------------------------
# or read it...
modMulti_veg <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMulti_veg.rds")

formulaMulti_veg <- ~ (ALAN * LIGHT) + week + tod_class + LAND + moon_fraction + temp + precip + vent + SNOW

ParMulti_veg <- getPar0(
  model = mod30,
  formula = formulaMulti_veg)

modMulti_veg <- fitHMM(
  data = STEPS_5min_df.NIGHT,
  nbState = nState,
  stateNames = stateNames,
  dist = dist,
  Par0 = ParMulti_veg$Par,
  beta0 = ParMulti_veg$beta,
  formula = formulaMulti_veg,
  retryFits = 1)

modMulti_veg

saveRDS(
  modMulti_veg,
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMulti_veg.rds")

#----------------------------------------------------
# B) PREDICT STATE PROBABILITIES (ALAN × LIGHT)
#----------------------------------------------------

alan_levels <- levels(STEPS_5min_df.NIGHT$ALAN)

light_seq <- seq(
  min(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  max(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  length.out = 20)

# valeurs de référence pour les autres variables
ref_vals <- data.frame(
  ALAN = factor(levels(STEPS_5min_df.NIGHT$ALAN)[1], levels = levels(STEPS_5min_df.NIGHT$ALAN)),
  LIGHT = mean(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  week = factor(levels(STEPS_5min_df.NIGHT$week)[1], levels = levels(STEPS_5min_df.NIGHT$week)),
  tod_class = factor(levels(STEPS_5min_df.NIGHT$tod_class)[1], levels = levels(STEPS_5min_df.NIGHT$tod_class)),
  LAND = factor(levels(STEPS_5min_df.NIGHT$LAND)[1], levels = levels(STEPS_5min_df.NIGHT$LAND)),
  moon_fraction = mean(STEPS_5min_df.NIGHT$moon_fraction, na.rm = TRUE),
  temp = mean(STEPS_5min_df.NIGHT$temp, na.rm = TRUE),
  precip = mean(STEPS_5min_df.NIGHT$precip, na.rm = TRUE),
  vent = mean(STEPS_5min_df.NIGHT$vent, na.rm = TRUE),
  SNOW = factor(levels(STEPS_5min_df.NIGHT$SNOW)[1], levels = levels(STEPS_5min_df.NIGHT$SNOW)))

statALAN_LIGHT_list <- list()

for(a in alan_levels){
  for(l in light_seq){
    
    newdata <- ref_vals
    newdata$ALAN <- a
    newdata$LIGHT <- l
    
    s <- plotStationary(
      modMulti_veg,
      covs = newdata,
      plotCI = TRUE,
      return = TRUE)
    
    out <- rbind(
      cbind(s[[1]], state = stateNames[1]),
      cbind(s[[2]], state = stateNames[2]),
      cbind(s[[3]], state = stateNames[3]))
    
    out$ALAN <- a
    out$LIGHT <- l
    
    statALAN_LIGHT_list[[length(statALAN_LIGHT_list) + 1]] <- out 
  }
}

statALAN_LIGHT_df <- do.call(rbind, statALAN_LIGHT_list)

statALAN_LIGHT_df$state <- factor(statALAN_LIGHT_df$state, levels = stateNames)

#-------------------------------------------------------------------------------

# C) VISUALIZE
#-------------

# 1) Recharger données NON standardisées (pour échelle réelle)
HARES_5min_df.NIGHT <- readRDS(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/Hares_5min_night.rds")

# 2) Reconstituer l'échelle réelle de LIGHT
light_mean <- mean(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)
light_sd   <- sd(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)

# 3) Définir des labels lisibles (en mètres)
light_labels <- seq(
  floor(min(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)),
  ceiling(max(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)),
  by = 200)

# 4) Transformer en échelle standardisée (pour correspondre au modèle)
light_breaks <- (light_labels - light_mean) / light_sd

# 5) Nombre d’observations par condition ALAN
n_ALAN <- STEPS_5min_df.NIGHT %>%
  group_by(ALAN) %>%
  summarise(n = n())

labels_custom <- n_ALAN %>%
  mutate(label = paste0(ALAN, "\n(n=", n, ")")) %>%
  select(ALAN, label) %>%
  deframe()

# FIGURE
#-------

multi_veg_distrib <- ggplot(
  data = statALAN_LIGHT_df,
  aes(
    x = LIGHT,
    y = est,
    color = state,
    fill = state,
    linetype = ALAN)) +

  geom_ribbon(
    aes(ymin = lci, ymax = uci),
    alpha = 0.15,
    colour = NA) +

  geom_line(linewidth = 1) +

  facet_grid(state ~ .) +

  scale_x_continuous(
    breaks = light_breaks,
    labels = light_labels) +

  scale_color_manual(values = c("darkred", "red", "orange")) +
  scale_fill_manual(values = c("darkred", "red", "orange")) +

  scale_linetype_manual(
    values = c(
      "OFF" = "solid",
      "ON" = "dotted"),
    labels = c("OFF", "ON")) +

  labs(
    x = "Distance to nearest lamp (m)",
    y = "Stationary state probability",
    color = "State",
    fill = "State",
    linetype = "ALAN",
    title = "Interactive effect of ALAN and distance to artificial light on behavioural states") +

  theme_minimal() +

  theme(
    legend.position = c(0.85, 0.75),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")) 

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/multi_veg_distrib.png",
  plot = multi_veg_distrib,
  width = 8,
  height = 8,
  dpi = 300)

#--------------------------------------------------------------------------------------------------------

# D) AICs COMPARAISON
# -------------------

# AICs
aic_Multi_veg <- AIC(modMulti_full, modMulti_veg)

# Comparaison
delta_Multi_veg <- with(aic_Multi_veg, AIC[Model == "modMulti_full"] - AIC[Model == "modMulti_veg"])

# Visualisation (delta > 2 ?)
delta_Multi_veg

# CLEAN
rm(b, cb, est, lower, newcovs, statALAN_LIGHT_list, upper, i, modMulti_veg, ParMulti_veg, formulaMulti_veg)
rm(statALAN_LIGHT_list, statALAN_LIGHT_df, light_seq, ref_vals, n_ALAN, labels_custom, multi_veg_distrib)

####################################################################################################

# 7.14 How do hare noctural activity patterns vary across all variable without "moon_fraction" ?
#---------------------------------------------------------------------------------------

# A) MODEL
#----------------------------------------------------
# or read it...
modMulti_moon <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMulti_moon.rds")

formulaMulti_moon <- ~ (ALAN * LIGHT) + week + tod_class + LAND + veg_height + temp + precip + vent + SNOW

ParMulti_moon <- getPar0(
  model = mod30,
  formula = formulaMulti_moon)

modMulti_moon <- fitHMM(
  data = STEPS_5min_df.NIGHT,
  nbState = nState,
  stateNames = stateNames,
  dist = dist,
  Par0 = ParMulti_moon$Par,
  beta0 = ParMulti_moon$beta,
  formula = formulaMulti_moon,
  retryFits = 1)

modMulti_moon

saveRDS(
  modMulti_moon,
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMulti_moon.rds")

#----------------------------------------------------
# B) PREDICT STATE PROBABILITIES (ALAN × LIGHT)
#----------------------------------------------------

alan_levels <- levels(STEPS_5min_df.NIGHT$ALAN)

light_seq <- seq(
  min(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  max(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  length.out = 20)

# valeurs de référence pour les autres variables
ref_vals <- data.frame(
  ALAN = factor(levels(STEPS_5min_df.NIGHT$ALAN)[1], levels = levels(STEPS_5min_df.NIGHT$ALAN)),
  LIGHT = mean(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  week = factor(levels(STEPS_5min_df.NIGHT$week)[1], levels = levels(STEPS_5min_df.NIGHT$week)),
  tod_class = factor(levels(STEPS_5min_df.NIGHT$tod_class)[1], levels = levels(STEPS_5min_df.NIGHT$tod_class)),
  LAND = factor(levels(STEPS_5min_df.NIGHT$LAND)[1], levels = levels(STEPS_5min_df.NIGHT$LAND)),
  veg_height = factor(levels(STEPS_5min_df.NIGHT$veg_height)[1], levels = levels(STEPS_5min_df.NIGHT$veg_height)),
  temp = mean(STEPS_5min_df.NIGHT$temp, na.rm = TRUE),
  precip = mean(STEPS_5min_df.NIGHT$precip, na.rm = TRUE),
  vent = mean(STEPS_5min_df.NIGHT$vent, na.rm = TRUE),
  SNOW = factor(levels(STEPS_5min_df.NIGHT$SNOW)[1], levels = levels(STEPS_5min_df.NIGHT$SNOW)))

statALAN_LIGHT_list <- list()

for(a in alan_levels){
  for(l in light_seq){
    
    newdata <- ref_vals
    newdata$ALAN <- a
    newdata$LIGHT <- l
    
    s <- plotStationary(
      modMulti_moon,
      covs = newdata,
      plotCI = TRUE,
      return = TRUE)
    
    out <- rbind(
      cbind(s[[1]], state = stateNames[1]),
      cbind(s[[2]], state = stateNames[2]),
      cbind(s[[3]], state = stateNames[3]))
    
    out$ALAN <- a
    out$LIGHT <- l
    
    statALAN_LIGHT_list[[length(statALAN_LIGHT_list) + 1]] <- out 
  }
}

statALAN_LIGHT_df <- do.call(rbind, statALAN_LIGHT_list)

statALAN_LIGHT_df$state <- factor(statALAN_LIGHT_df$state, levels = stateNames)

#-------------------------------------------------------------------------------

# C) VISUALIZE
#-------------

# 1) Recharger données NON standardisées (pour échelle réelle)
HARES_5min_df.NIGHT <- readRDS(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/Hares_5min_night.rds")

# 2) Reconstituer l'échelle réelle de LIGHT
light_mean <- mean(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)
light_sd   <- sd(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)

# 3) Définir des labels lisibles (en mètres)
light_labels <- seq(
  floor(min(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)),
  ceiling(max(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)),
  by = 200)

# 4) Transformer en échelle standardisée (pour correspondre au modèle)
light_breaks <- (light_labels - light_mean) / light_sd

# 5) Nombre d’observations par condition ALAN
n_ALAN <- STEPS_5min_df.NIGHT %>%
  group_by(ALAN) %>%
  summarise(n = n())

labels_custom <- n_ALAN %>%
  mutate(label = paste0(ALAN, "\n(n=", n, ")")) %>%
  select(ALAN, label) %>%
  deframe()

# FIGURE
#-------

multi_moon_distrib <- ggplot(
  data = statALAN_LIGHT_df,
  aes(
    x = LIGHT,
    y = est,
    color = state,
    fill = state,
    linetype = ALAN)) +

  geom_ribbon(
    aes(ymin = lci, ymax = uci),
    alpha = 0.15,
    colour = NA) +

  geom_line(linewidth = 1) +

  facet_grid(state ~ .) +

  scale_x_continuous(
    breaks = light_breaks,
    labels = light_labels) +

  scale_color_manual(values = c("darkred", "red", "orange")) +
  scale_fill_manual(values = c("darkred", "red", "orange")) +

  scale_linetype_manual(
    values = c(
      "OFF" = "solid",
      "ON" = "dotted"),
    labels = c("OFF", "ON")) +

  labs(
    x = "Distance to nearest lamp (m)",
    y = "Stationary state probability",
    color = "State",
    fill = "State",
    linetype = "ALAN",
    title = "Interactive effect of ALAN and distance to artificial light on behavioural states") +

  theme_minimal() +

  theme(
    legend.position = c(0.85, 0.75),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")) 

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/multi_moon_distrib.png",
  plot = multi_moon_distrib,
  width = 8,
  height = 8,
  dpi = 300)

#--------------------------------------------------------------------------------------------------------

# D) AICs COMPARAISON
# -------------------

# AICs
aic_Multi_moon <- AIC(modMulti_full, modMulti_moon)

# Comparaison
delta_Multi_moon <- with(aic_Multi_moon, AIC[Model == "modMulti_full"] - AIC[Model == "modMulti_moon"])

# Visualisation (delta > 2 ?)
delta_Multi_moon

# CLEAN
rm(b, cb, est, lower, newcovs, statALAN_LIGHT_list, upper, i, modMulti_moon, ParMulti_moon, formulaMulti_moon)
rm(statALAN_LIGHT_list, statALAN_LIGHT_df, light_seq, ref_vals, n_ALAN, labels_custom, multi_moon_distrib)

####################################################################################################

# 7.15 How do hare noctural activity patterns vary across all variable without "temp" ?
#---------------------------------------------------------------------------------------

# A) MODEL
#----------------------------------------------------
# or read it...
modMulti_temp <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMulti_temp.rds")

formulaMulti_temp <- ~ (ALAN * LIGHT) + week + tod_class + LAND + veg_height + moon_fraction + precip + vent + SNOW

ParMulti_temp <- getPar0(
  model = mod30,
  formula = formulaMulti_temp)

modMulti_temp <- fitHMM(
  data = STEPS_5min_df.NIGHT,
  nbState = nState,
  stateNames = stateNames,
  dist = dist,
  Par0 = ParMulti_temp$Par,
  beta0 = ParMulti_temp$beta,
  formula = formulaMulti_temp,
  retryFits = 1)

modMulti_temp

saveRDS(
  modMulti_temp,
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMulti_temp.rds")

#----------------------------------------------------
# B) PREDICT STATE PROBABILITIES (ALAN × LIGHT)
#----------------------------------------------------

alan_levels <- levels(STEPS_5min_df.NIGHT$ALAN)

light_seq <- seq(
  min(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  max(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  length.out = 20)

# valeurs de référence pour les autres variables
ref_vals <- data.frame(
  ALAN = factor(levels(STEPS_5min_df.NIGHT$ALAN)[1], levels = levels(STEPS_5min_df.NIGHT$ALAN)),
  LIGHT = mean(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  week = factor(levels(STEPS_5min_df.NIGHT$week)[1], levels = levels(STEPS_5min_df.NIGHT$week)),
  tod_class = factor(levels(STEPS_5min_df.NIGHT$tod_class)[1], levels = levels(STEPS_5min_df.NIGHT$tod_class)),
  LAND = factor(levels(STEPS_5min_df.NIGHT$LAND)[1], levels = levels(STEPS_5min_df.NIGHT$LAND)),
  veg_height = factor(levels(STEPS_5min_df.NIGHT$veg_height)[1], levels = levels(STEPS_5min_df.NIGHT$veg_height)),
  moon_fraction = mean(STEPS_5min_df.NIGHT$moon_fraction, na.rm = TRUE),
  precip = mean(STEPS_5min_df.NIGHT$precip, na.rm = TRUE),
  vent = mean(STEPS_5min_df.NIGHT$vent, na.rm = TRUE),
  SNOW = factor(levels(STEPS_5min_df.NIGHT$SNOW)[1], levels = levels(STEPS_5min_df.NIGHT$SNOW)))

statALAN_LIGHT_list <- list()

for(a in alan_levels){
  for(l in light_seq){
    
    newdata <- ref_vals
    newdata$ALAN <- a
    newdata$LIGHT <- l
    
    s <- plotStationary(
      modMulti_temp,
      covs = newdata,
      plotCI = TRUE,
      return = TRUE)
    
    out <- rbind(
      cbind(s[[1]], state = stateNames[1]),
      cbind(s[[2]], state = stateNames[2]),
      cbind(s[[3]], state = stateNames[3]))
    
    out$ALAN <- a
    out$LIGHT <- l
    
    statALAN_LIGHT_list[[length(statALAN_LIGHT_list) + 1]] <- out 
  }
}

statALAN_LIGHT_df <- do.call(rbind, statALAN_LIGHT_list)

statALAN_LIGHT_df$state <- factor(statALAN_LIGHT_df$state, levels = stateNames)

#-------------------------------------------------------------------------------

# C) VISUALIZE
#-------------

# 1) Recharger données NON standardisées (pour échelle réelle)
HARES_5min_df.NIGHT <- readRDS(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/Hares_5min_night.rds")

# 2) Reconstituer l'échelle réelle de LIGHT
light_mean <- mean(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)
light_sd   <- sd(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)

# 3) Définir des labels lisibles (en mètres)
light_labels <- seq(
  floor(min(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)),
  ceiling(max(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)),
  by = 200)

# 4) Transformer en échelle standardisée (pour correspondre au modèle)
light_breaks <- (light_labels - light_mean) / light_sd

# 5) Nombre d’observations par condition ALAN
n_ALAN <- STEPS_5min_df.NIGHT %>%
  group_by(ALAN) %>%
  summarise(n = n())

labels_custom <- n_ALAN %>%
  mutate(label = paste0(ALAN, "\n(n=", n, ")")) %>%
  select(ALAN, label) %>%
  deframe()

# FIGURE
#-------

multi_temp_distrib <- ggplot(
  data = statALAN_LIGHT_df,
  aes(
    x = LIGHT,
    y = est,
    color = state,
    fill = state,
    linetype = ALAN)) +

  geom_ribbon(
    aes(ymin = lci, ymax = uci),
    alpha = 0.15,
    colour = NA) +

  geom_line(linewidth = 1) +

  facet_grid(state ~ .) +

  scale_x_continuous(
    breaks = light_breaks,
    labels = light_labels) +

  scale_color_manual(values = c("darkred", "red", "orange")) +
  scale_fill_manual(values = c("darkred", "red", "orange")) +

  scale_linetype_manual(
    values = c(
      "OFF" = "solid",
      "ON" = "dotted"),
    labels = c("OFF", "ON")) +

  labs(
    x = "Distance to nearest lamp (m)",
    y = "Stationary state probability",
    color = "State",
    fill = "State",
    linetype = "ALAN",
    title = "Interactive effect of ALAN and distance to artificial light on behavioural states") +

  theme_minimal() +

  theme(
    legend.position = c(0.85, 0.75),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")) 

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/multi_temp_distrib.png",
  plot = multi_temp_distrib,
  width = 8,
  height = 8,
  dpi = 300)

#--------------------------------------------------------------------------------------------------------

# D) AICs COMPARAISON
# -------------------

# AICs
aic_Multi_temp <- AIC(modMulti_full, modMulti_temp)

# Comparaison
delta_Multi_temp <- with(aic_Multi_temp, AIC[Model == "modMulti_full"] - AIC[Model == "modMulti_temp"])

# Visualisation (delta > 2 ?)
delta_Multi_temp

# CLEAN
rm(b, cb, est, lower, newcovs, statALAN_LIGHT_list, upper, i, modMulti_temp, ParMulti_temp, formulaMulti_temp)
rm(statALAN_LIGHT_list, statALAN_LIGHT_df, light_seq, ref_vals, n_ALAN, labels_custom, multi_temp_distrib)

####################################################################################################

# 7.16 How do hare noctural activity patterns vary across all variable without "precip" ?
#---------------------------------------------------------------------------------------

# A) MODEL
#----------------------------------------------------
# or read it...
modMulti_precip <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMulti_precip.rds")

formulaMulti_precip <- ~ (ALAN * LIGHT) + week + tod_class + LAND + veg_height + moon_fraction + temp + vent + SNOW

ParMulti_precip <- getPar0(
  model = mod30,
  formula = formulaMulti_precip)

modMulti_precip <- fitHMM(
  data = STEPS_5min_df.NIGHT,
  nbState = nState,
  stateNames = stateNames,
  dist = dist,
  Par0 = ParMulti_precip$Par,
  beta0 = ParMulti_precip$beta,
  formula = formulaMulti_precip,
  retryFits = 1)

modMulti_precip

saveRDS(
  modMulti_precip,
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMulti_precip.rds")

#----------------------------------------------------
# B) PREDICT STATE PROBABILITIES (ALAN × LIGHT)
#----------------------------------------------------

alan_levels <- levels(STEPS_5min_df.NIGHT$ALAN)

light_seq <- seq(
  min(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  max(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  length.out = 20)

# valeurs de référence pour les autres variables
ref_vals <- data.frame(
  ALAN = factor(levels(STEPS_5min_df.NIGHT$ALAN)[1], levels = levels(STEPS_5min_df.NIGHT$ALAN)),
  LIGHT = mean(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  week = factor(levels(STEPS_5min_df.NIGHT$week)[1], levels = levels(STEPS_5min_df.NIGHT$week)),
  tod_class = factor(levels(STEPS_5min_df.NIGHT$tod_class)[1], levels = levels(STEPS_5min_df.NIGHT$tod_class)),
  LAND = factor(levels(STEPS_5min_df.NIGHT$LAND)[1], levels = levels(STEPS_5min_df.NIGHT$LAND)),
  veg_height = factor(levels(STEPS_5min_df.NIGHT$veg_height)[1], levels = levels(STEPS_5min_df.NIGHT$veg_height)),
  temp = mean(STEPS_5min_df.NIGHT$temp, na.rm = TRUE),
  moon_fraction = mean(STEPS_5min_df.NIGHT$moon_fraction, na.rm = TRUE),
  vent = mean(STEPS_5min_df.NIGHT$vent, na.rm = TRUE),
  SNOW = factor(levels(STEPS_5min_df.NIGHT$SNOW)[1], levels = levels(STEPS_5min_df.NIGHT$SNOW)))

statALAN_LIGHT_list <- list()

for(a in alan_levels){
  for(l in light_seq){
    
    newdata <- ref_vals
    newdata$ALAN <- a
    newdata$LIGHT <- l
    
    s <- plotStationary(
      modMulti_precip,
      covs = newdata,
      plotCI = TRUE,
      return = TRUE)
    
    out <- rbind(
      cbind(s[[1]], state = stateNames[1]),
      cbind(s[[2]], state = stateNames[2]),
      cbind(s[[3]], state = stateNames[3]))
    
    out$ALAN <- a
    out$LIGHT <- l
    
    statALAN_LIGHT_list[[length(statALAN_LIGHT_list) + 1]] <- out 
  }
}

statALAN_LIGHT_df <- do.call(rbind, statALAN_LIGHT_list)

statALAN_LIGHT_df$state <- factor(statALAN_LIGHT_df$state, levels = stateNames)

#-------------------------------------------------------------------------------

# C) VISUALIZE
#-------------

# 1) Recharger données NON standardisées (pour échelle réelle)
HARES_5min_df.NIGHT <- readRDS(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/Hares_5min_night.rds")

# 2) Reconstituer l'échelle réelle de LIGHT
light_mean <- mean(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)
light_sd   <- sd(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)

# 3) Définir des labels lisibles (en mètres)
light_labels <- seq(
  floor(min(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)),
  ceiling(max(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)),
  by = 200)

# 4) Transformer en échelle standardisée (pour correspondre au modèle)
light_breaks <- (light_labels - light_mean) / light_sd

# 5) Nombre d’observations par condition ALAN
n_ALAN <- STEPS_5min_df.NIGHT %>%
  group_by(ALAN) %>%
  summarise(n = n())

labels_custom <- n_ALAN %>%
  mutate(label = paste0(ALAN, "\n(n=", n, ")")) %>%
  select(ALAN, label) %>%
  deframe()

# FIGURE
#-------

multi_precip_distrib <- ggplot(
  data = statALAN_LIGHT_df,
  aes(
    x = LIGHT,
    y = est,
    color = state,
    fill = state,
    linetype = ALAN)) +

  geom_ribbon(
    aes(ymin = lci, ymax = uci),
    alpha = 0.15,
    colour = NA) +

  geom_line(linewidth = 1) +

  facet_grid(state ~ .) +

  scale_x_continuous(
    breaks = light_breaks,
    labels = light_labels) +

  scale_color_manual(values = c("darkred", "red", "orange")) +
  scale_fill_manual(values = c("darkred", "red", "orange")) +

  scale_linetype_manual(
    values = c(
      "OFF" = "solid",
      "ON" = "dotted"),
    labels = c("OFF", "ON")) +

  labs(
    x = "Distance to nearest lamp (m)",
    y = "Stationary state probability",
    color = "State",
    fill = "State",
    linetype = "ALAN",
    title = "Interactive effect of ALAN and distance to artificial light on behavioural states") +

  theme_minimal() +

  theme(
    legend.position = c(0.85, 0.75),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")) 

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/multi_precip_distrib.png",
  plot = multi_precip_distrib,
  width = 8,
  height = 8,
  dpi = 300)

#--------------------------------------------------------------------------------------------------------

# D) AICs COMPARAISON
# -------------------

# AICs
aic_Multi_precip <- AIC(modMulti_full, modMulti_precip)

# Comparaison
delta_Multi_precip <- with(aic_Multi_precip, AIC[Model == "modMulti_full"] - AIC[Model == "modMulti_precip"])

# Visualisation (delta > 2 ?)
delta_Multi_precip

# CLEAN
rm(b, cb, est, lower, newcovs, statALAN_LIGHT_list, upper, i, modMulti_precip, ParMulti_precip, formulaMulti_precip)
rm(statALAN_LIGHT_list, statALAN_LIGHT_df, light_seq, ref_vals, n_ALAN, labels_custom, multi_precip_distrib)

####################################################################################################

# 7.17 How do hare noctural activity patterns vary across all variable without "vent" ?
#---------------------------------------------------------------------------------------

# A) MODEL
#----------------------------------------------------
# or read it...
modMulti_vent <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMulti_vent.rds")

formulaMulti_vent <- ~ (ALAN * LIGHT) + week + tod_class + LAND + veg_height + moon_fraction + temp + precip + SNOW

ParMulti_vent <- getPar0(
  model = mod30,
  formula = formulaMulti_vent)

modMulti_vent <- fitHMM(
  data = STEPS_5min_df.NIGHT,
  nbState = nState,
  stateNames = stateNames,
  dist = dist,
  Par0 = ParMulti_vent$Par,
  beta0 = ParMulti_vent$beta,
  formula = formulaMulti_vent,
  retryFits = 1)

modMulti_vent

saveRDS(
  modMulti_vent,
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMulti_vent.rds")

#----------------------------------------------------
# B) PREDICT STATE PROBABILITIES (ALAN × LIGHT)
#----------------------------------------------------

alan_levels <- levels(STEPS_5min_df.NIGHT$ALAN)

light_seq <- seq(
  min(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  max(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  length.out = 20)

# valeurs de référence pour les autres variables
ref_vals <- data.frame(
  ALAN = factor(levels(STEPS_5min_df.NIGHT$ALAN)[1], levels = levels(STEPS_5min_df.NIGHT$ALAN)),
  LIGHT = mean(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  week = factor(levels(STEPS_5min_df.NIGHT$week)[1], levels = levels(STEPS_5min_df.NIGHT$week)),
  tod_class = factor(levels(STEPS_5min_df.NIGHT$tod_class)[1], levels = levels(STEPS_5min_df.NIGHT$tod_class)),
  LAND = factor(levels(STEPS_5min_df.NIGHT$LAND)[1], levels = levels(STEPS_5min_df.NIGHT$LAND)),
  veg_height = factor(levels(STEPS_5min_df.NIGHT$veg_height)[1], levels = levels(STEPS_5min_df.NIGHT$veg_height)),
  temp = mean(STEPS_5min_df.NIGHT$temp, na.rm = TRUE),
  moon_fraction = mean(STEPS_5min_df.NIGHT$moon_fraction, na.rm = TRUE),
  precip = mean(STEPS_5min_df.NIGHT$precip, na.rm = TRUE),
  SNOW = factor(levels(STEPS_5min_df.NIGHT$SNOW)[1], levels = levels(STEPS_5min_df.NIGHT$SNOW)))

statALAN_LIGHT_list <- list()

for(a in alan_levels){
  for(l in light_seq){
    
    newdata <- ref_vals
    newdata$ALAN <- a
    newdata$LIGHT <- l
    
    s <- plotStationary(
      modMulti_vent,
      covs = newdata,
      plotCI = TRUE,
      return = TRUE)
    
    out <- rbind(
      cbind(s[[1]], state = stateNames[1]),
      cbind(s[[2]], state = stateNames[2]),
      cbind(s[[3]], state = stateNames[3]))
    
    out$ALAN <- a
    out$LIGHT <- l
    
    statALAN_LIGHT_list[[length(statALAN_LIGHT_list) + 1]] <- out 
  }
}

statALAN_LIGHT_df <- do.call(rbind, statALAN_LIGHT_list)

statALAN_LIGHT_df$state <- factor(statALAN_LIGHT_df$state, levels = stateNames)

#-------------------------------------------------------------------------------

# C) VISUALIZE
#-------------

# 1) Recharger données NON standardisées (pour échelle réelle)
HARES_5min_df.NIGHT <- readRDS(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/Hares_5min_night.rds")

# 2) Reconstituer l'échelle réelle de LIGHT
light_mean <- mean(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)
light_sd   <- sd(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)

# 3) Définir des labels lisibles (en mètres)
light_labels <- seq(
  floor(min(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)),
  ceiling(max(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)),
  by = 200)

# 4) Transformer en échelle standardisée (pour correspondre au modèle)
light_breaks <- (light_labels - light_mean) / light_sd

# 5) Nombre d’observations par condition ALAN
n_ALAN <- STEPS_5min_df.NIGHT %>%
  group_by(ALAN) %>%
  summarise(n = n())

labels_custom <- n_ALAN %>%
  mutate(label = paste0(ALAN, "\n(n=", n, ")")) %>%
  select(ALAN, label) %>%
  deframe()

# FIGURE
#-------

multi_vent_distrib <- ggplot(
  data = statALAN_LIGHT_df,
  aes(
    x = LIGHT,
    y = est,
    color = state,
    fill = state,
    linetype = ALAN)) +

  geom_ribbon(
    aes(ymin = lci, ymax = uci),
    alpha = 0.15,
    colour = NA) +

  geom_line(linewidth = 1) +

  facet_grid(state ~ .) +

  scale_x_continuous(
    breaks = light_breaks,
    labels = light_labels) +

  scale_color_manual(values = c("darkred", "red", "orange")) +
  scale_fill_manual(values = c("darkred", "red", "orange")) +

  scale_linetype_manual(
    values = c(
      "OFF" = "solid",
      "ON" = "dotted"),
    labels = c("OFF", "ON")) +

  labs(
    x = "Distance to nearest lamp (m)",
    y = "Stationary state probability",
    color = "State",
    fill = "State",
    linetype = "ALAN",
    title = "Interactive effect of ALAN and distance to artificial light on behavioural states") +

  theme_minimal() +

  theme(
    legend.position = c(0.85, 0.75),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")) 

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/multi_vent_distrib.png",
  plot = multi_vent_distrib,
  width = 8,
  height = 8,
  dpi = 300)

#--------------------------------------------------------------------------------------------------------

# D) AICs COMPARAISON
# -------------------

# AICs
aic_Multi_vent <- AIC(modMulti_full, modMulti_vent)

# Comparaison
delta_Multi_vent <- with(aic_Multi_vent, AIC[Model == "modMulti_full"] - AIC[Model == "modMulti_vent"])

# Visualisation (delta > 2 ?)
delta_Multi_vent

# CLEAN
rm(b, cb, est, lower, newcovs, statALAN_LIGHT_list, upper, i, modMulti_vent, ParMulti_vent, formulaMulti_vent)
rm(statALAN_LIGHT_list, statALAN_LIGHT_df, light_seq, ref_vals, n_ALAN, labels_custom, multi_vent_distrib)

####################################################################################################

# 7.18 How do hare noctural activity patterns vary across all variable without "SNOW" ?
#---------------------------------------------------------------------------------------

# A) MODEL
#----------------------------------------------------
# or read it...
modMulti_SNOW <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMulti_SNOW.rds")

formulaMulti_SNOW <- ~ (ALAN * LIGHT) + week + tod_class + LAND + veg_height + moon_fraction + temp + precip + vent

ParMulti_SNOW <- getPar0(
  model = mod30,
  formula = formulaMulti_SNOW)

modMulti_SNOW <- fitHMM(
  data = STEPS_5min_df.NIGHT,
  nbState = nState,
  stateNames = stateNames,
  dist = dist,
  Par0 = ParMulti_SNOW$Par,
  beta0 = ParMulti_SNOW$beta,
  formula = formulaMulti_SNOW,
  retryFits = 1)

modMulti_SNOW

saveRDS(
  modMulti_SNOW,
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMulti_SNOW.rds")

#----------------------------------------------------
# B) PREDICT STATE PROBABILITIES (ALAN × LIGHT)
#----------------------------------------------------

alan_levels <- levels(STEPS_5min_df.NIGHT$ALAN)

light_seq <- seq(
  min(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  max(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  length.out = 20)

# valeurs de référence pour les autres variables
ref_vals <- data.frame(
  ALAN = factor(levels(STEPS_5min_df.NIGHT$ALAN)[1], levels = levels(STEPS_5min_df.NIGHT$ALAN)),
  LIGHT = mean(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  week = factor(levels(STEPS_5min_df.NIGHT$week)[1], levels = levels(STEPS_5min_df.NIGHT$week)),
  tod_class = factor(levels(STEPS_5min_df.NIGHT$tod_class)[1], levels = levels(STEPS_5min_df.NIGHT$tod_class)),
  LAND = factor(levels(STEPS_5min_df.NIGHT$LAND)[1], levels = levels(STEPS_5min_df.NIGHT$LAND)),
  veg_height = factor(levels(STEPS_5min_df.NIGHT$veg_height)[1], levels = levels(STEPS_5min_df.NIGHT$veg_height)),
  temp = mean(STEPS_5min_df.NIGHT$temp, na.rm = TRUE),
  moon_fraction = mean(STEPS_5min_df.NIGHT$moon_fraction, na.rm = TRUE),
  precip = mean(STEPS_5min_df.NIGHT$precip, na.rm = TRUE),
  vent = mean(STEPS_5min_df.NIGHT$vent, na.rm = TRUE))

statALAN_LIGHT_list <- list()

for(a in alan_levels){
  for(l in light_seq){
    
    newdata <- ref_vals
    newdata$ALAN <- a
    newdata$LIGHT <- l
    
    s <- plotStationary(
      modMulti_SNOW,
      covs = newdata,
      plotCI = TRUE,
      return = TRUE)
    
    out <- rbind(
      cbind(s[[1]], state = stateNames[1]),
      cbind(s[[2]], state = stateNames[2]),
      cbind(s[[3]], state = stateNames[3]))
    
    out$ALAN <- a
    out$LIGHT <- l
    
    statALAN_LIGHT_list[[length(statALAN_LIGHT_list) + 1]] <- out 
  }
}

statALAN_LIGHT_df <- do.call(rbind, statALAN_LIGHT_list)

statALAN_LIGHT_df$state <- factor(statALAN_LIGHT_df$state, levels = stateNames)

#-------------------------------------------------------------------------------

# C) VISUALIZE
#-------------

# 1) Recharger données NON standardisées (pour échelle réelle)
HARES_5min_df.NIGHT <- readRDS(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/Hares_5min_night.rds")

# 2) Reconstituer l'échelle réelle de LIGHT
light_mean <- mean(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)
light_sd   <- sd(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)

# 3) Définir des labels lisibles (en mètres)
light_labels <- seq(
  floor(min(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)),
  ceiling(max(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)),
  by = 200)

# 4) Transformer en échelle standardisée (pour correspondre au modèle)
light_breaks <- (light_labels - light_mean) / light_sd

# 5) Nombre d’observations par condition ALAN
n_ALAN <- STEPS_5min_df.NIGHT %>%
  group_by(ALAN) %>%
  summarise(n = n())

labels_custom <- n_ALAN %>%
  mutate(label = paste0(ALAN, "\n(n=", n, ")")) %>%
  select(ALAN, label) %>%
  deframe()

# FIGURE
#-------

multi_SNOW_distrib <- ggplot(
  data = statALAN_LIGHT_df,
  aes(
    x = LIGHT,
    y = est,
    color = state,
    fill = state,
    linetype = ALAN)) +

  geom_ribbon(
    aes(ymin = lci, ymax = uci),
    alpha = 0.15,
    colour = NA) +

  geom_line(linewidth = 1) +

  facet_grid(state ~ .) +

  scale_x_continuous(
    breaks = light_breaks,
    labels = light_labels) +

  scale_color_manual(values = c("darkred", "red", "orange")) +
  scale_fill_manual(values = c("darkred", "red", "orange")) +

  scale_linetype_manual(
    values = c(
      "OFF" = "solid",
      "ON" = "dotted"),
    labels = c("OFF", "ON")) +

  labs(
    x = "Distance to nearest lamp (m)",
    y = "Stationary state probability",
    color = "State",
    fill = "State",
    linetype = "ALAN",
    title = "Interactive effect of ALAN and distance to artificial light on behavioural states") +

  theme_minimal() +

  theme(
    legend.position = c(0.85, 0.75),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")) 

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/multi_SNOW_distrib.png",
  plot = multi_SNOW_distrib,
  width = 8,
  height = 8,
  dpi = 300)

#--------------------------------------------------------------------------------------------------------

# D) AICs COMPARAISON
# -------------------

# AICs
aic_Multi_SNOW <- AIC(modMulti_full, modMulti_SNOW)

# Comparaison
delta_Multi_SNOW <- with(aic_Multi_SNOW, AIC[Model == "modMulti_full"] - AIC[Model == "modMulti_SNOW"])

# Visualisation (delta > 2 ?)
delta_Multi_SNOW

# CLEAN
rm(b, cb, est, lower, newcovs, statALAN_LIGHT_list, upper, i, modMulti_SNOW, ParMulti_SNOW, formulaMulti_SNOW)
rm(statALAN_LIGHT_list, statALAN_LIGHT_df, light_seq, ref_vals, n_ALAN, labels_custom, multi_SNOW_distrib)

#########################################################################################################

# 7.19 How do hare noctural activity patterns vary across all variable and "ALAN * LIGHT * LAND" interaction ?
#--------------------------------------------------------------------------------------------------------------

# A) MODEL
#----------------------------------------------------
formulaMulti_3_intera <- ~ (ALAN * LIGHT * LAND) + week + tod_class + veg_height + moon_fraction + temp + precip + vent + SNOW

ParMulti_3_intera <- getPar0(
  model = mod30,
  formula = formulaMulti_3_intera)

modMulti_3_intera <- fitHMM(
  data = STEPS_5min_df.NIGHT,
  nbState = nState,
  stateNames = stateNames,
  dist = dist,
  Par0 = ParMulti_3_intera$Par,
  beta0 = ParMulti_3_intera$beta,
  formula = formulaMulti_3_intera,
  retryFits = 1)

modMulti_3_intera

saveRDS(
  modMulti_3_intera,
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMulti_3_intera.rds")

# CLEAN
rm(modMulti_3_intera, ParMulti_3_intera, formulaMulti_3_intera)

#----------------------------------------------------------------------------------------------------------

# D) AICs COMPARAISON
# -------------------
modMulti_full <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMulti_full.rds")

# AICs
aic_Multi_3_intera <- AIC(modMulti_full, modMulti_3_intera)

# Comparaison
delta_Multi_3_intera <- with(aic_Multi_3_intera, AIC[Model == "modMulti_full"] - AIC[Model == "modMulti_3_intera"])

# Visualisation (delta > 2 ?)
delta_Multi_3_intera

# CLEAN
rm(b, cb, est, lower, newcovs, statALAN_LIGHT_list, upper, i, modMulti_3_intera, ParMulti_3_intera, formulaMulti_3_intera)
rm(statALAN_LIGHT_list, statALAN_LIGHT_df, light_seq, ref_vals, n_ALAN, labels_custom, multi_3_intera_distrib)

#######################################################################################################################

# Résumer les AICs en 1 figure
# ----------------------------

# Créer un tableau avec toutes les variables testées
results <- data.frame(
  variable = c("Semaines de l'année", "Période de la nuit", "Utilisation de surface", "Hauteur de végétation", 
                "Vent", "Température", "Précipitations", "Couverture neigeuse", "Lune"),
  delta_AIC = c(delta_Multi_week, delta_Multi_TOD, delta_Multi_LAND, delta_Multi_veg, 
                delta_Multi_vent, delta_Multi_temp, delta_Multi_precip, delta_Multi_SNOW, delta_Multi_moon))

results

AIC_multi_results <- ggplot(
  results, aes(x = reorder(variable, delta_AIC), y = delta_AIC)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_hline(yintercept = 2, linetype = "dashed", color = "red") +
  geom_hline(yintercept = -2, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    title = "Comparaison des modèles multivariés au modèle nul (Δ AIC)",
    x = "Variables absentes",
    y = "Δ AIC (modèle nul - modèles multivariés)") +
  annotate(
    "label",
      x = Inf,
      y = min(results$delta_AIC),
      hjust = 0.03,
      vjust = 1.2,
      label = "-2 > Δ AIC > 2\n=> l'absence de la variable influence\nsignificativement l'ajustement du modèle") +
  theme_minimal()

ggsave(
 "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/AIC_multi_results.png",
 plot = AIC_multi_results,
 width = 10,
 height = 6,
 dpi = 300)
