# 4 FIT HMM (3 STATES) - Night only
#########################

# 4.1  Load data
#################

STEPS_5min_df <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/STEPS_5min_3states.rds")

STEPS_5min_df.NIGHT <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/STEPS_5min_3states_night.rds")

dist <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/dist.rds")

mod30 <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/mod30.rds")

stateNames <- c("resting", "foraging", "exploring")
# Pour renommer les CI en français : stateNames <- c("Immobile", "Actif", "Transit")
nState <- 3

# Reconstruction d'un df avant scaling pour la mise en page des plot à variables numériques scallées
HARES_5min <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/Hares_5min.rds")

HARES_5min_df.NIGHT <- HARES_5min[HARES_5min$diel == "night",]

HARES_5min_df.NIGHT <- HARES_5min_df.NIGHT %>%
  filter(date_local >= as.Date("2025-12-05") & 
         date_local <= as.Date("2026-03-27"))

saveRDS(
  HARES_5min_df.NIGHT, 
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/HARES_5min_night.rds")

########################################################################################################################

# 4.2 EXPLORING TEMPORAL COVARIATES
####################################
#-------------------------------------------------------------------
# 4.21 How do hare noctural activity patterns vary across the year?
#-------------------------------------------------------------------

# A) MODEL
#---------
# or read it...
modWEEK <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modWEEK.rds")

formulaWEEK <- ~ week

ParWEEK <- getPar0(
  model = mod30,
  formula = formulaWEEK)

modWEEK <- fitHMM(
  data = STEPS_5min_df.NIGHT,
  nbState = nState,
  stateNames = stateNames,
  dist = dist,
  Par0 = ParWEEK$Par,
  beta0 = ParWEEK$beta,
  formula = formulaWEEK,
  retryFits = 1)

modWEEK

saveRDS(
  modWEEK,
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modWEEK.rds")

# B) TEST EFFECT OF VARIABLES
#----------------------------

cb <- CIbeta(modWEEK)

# Extract beta table
est   <- cb$beta$est
lower <- cb$beta$lower
upper <- cb$beta$upper

# Keep only WEEK covariate rows
week_rows <- grepl("^week", rownames(est))

b <- data.frame(
  week = rownames(est)[week_rows],
  transition = rep(colnames(est), each = sum(week_rows)),
  estimate = as.vector(est[week_rows, ]),
  lower = as.vector(lower[week_rows, ]),
  upper = as.vector(upper[week_rows, ]))

# Clean WEEK labels
b$week <- sub("^week", "", b$week)

# Replace state numbers by state names
for(i in seq_along(stateNames)) {
  b$transition <- gsub(
    paste0("\\b", i, "\\b"),
    stateNames[i],
    b$transition)
}

# Remove rows with NaN / NA CI
b <- b[complete.cases(b[, c("estimate", "lower", "upper")]), ]

# Significant if CI does not overlap 0
b$significant <- b$lower > 0 | b$upper < 0

CI_week <- ggplot(b, aes(x = week, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(ymin = lower, ymax = upper,
        colour = significant,
        shape = significant),
    linewidth = 0.5) +
  facet_wrap(~ transition) +
  coord_flip() +
  scale_colour_manual(
    values = c("FALSE" = "grey60", "TRUE" = "red"),
    labels = c("Non significatif", "Significatif")) +
  scale_shape_manual(
    values = c("FALSE" = 1, "TRUE" = 16),
    labels = c("Non significatif", "Significatif")) +
  labs(
    x = "Semaines de l'année",
    y = "Estimation Beta ± 95% IC",
    colour = "",
    shape = "",
    title = "Effet des semaines de l'année sur les transitions d'états") +
  theme_classic()

# C) PREDICT STATE PROBABILITIES FOR EACH CATEGORY
#----------------------------------------------------

pdf(NULL)

statWEEK <- plotStationary(
  modWEEK,
  plotCI = TRUE,
  alpha = 0.95,
  return = TRUE)

dev.off()

statWEEK_df <- rbind(
  transform(statWEEK$week$resting,   state = "resting"),
  transform(statWEEK$week$foraging,  state = "foraging"),
  transform(statWEEK$week$exploring, state = "exploring"))

names(statWEEK_df)[names(statWEEK_df) == "cov"] <- "week"

statWEEK_df$week <- factor(
  statWEEK_df$week,
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
    "Week13_2026"))

statWEEK_df$state <- factor(
  statWEEK_df$state,
  levels = c("resting", "foraging", "exploring"))

# D) VISUALIZE
#-------------
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

week_distrib <- ggplot(
  data = statWEEK_df,
  aes(x = week, y = est, fill = state)) +
  
  geom_col(
    position = position_dodge(0.7),
    width = 0.6) +
  
  geom_errorbar(
    aes(ymin = lci, ymax = uci),
    position = position_dodge(0.7),
    width = 0.15) +
  
  labs(
    x = "Semaines 2025-26",
    y = "Probabilité d'état",
    fill = "Etat",
    title = "Effet des semaines de l'année sur le rythme d'activité du lièvre brun") +
  
  scale_fill_manual(
    values = c("darkred", "red", "orange"),
    labels = c("immobile", "actif", "transit")) +
  
  scale_x_discrete(
    labels = labels_custom) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.45, 0.88))

ggsave(
 "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/WEEK_distrib.png",
 plot = week_distrib,
 width = 10,
 height = 6,
 dpi = 300)

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/CI_week.png",
  plot = CI_week,
  width = 8,
  height = 6,
  dpi = 300)

grid.arrange(CI_week, week_distrib)

rm(b, cb, est, lower, modWEEK, parWEEK, statWEEK, statWEEK_df, n_week)
rm(CI_week, formulaWEEK, i, week_distrib, week_rows, ParWEEK)

###########################################################################################################
# 4.3 EXPLORING SPATIAL COVARIATES
###################################

# 4.31 How do noctural activity patterns of hares vary among habitat types?
#---------------------------------------------------------------------------

# A) MODEL
#----------------------------------------------------
# or read it...
modHabitat <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modHabitat.rds")

formulaHabitat <- ~ LAND

ParHabitat <- getPar0(
  model = mod30,
  formula = formulaHabitat)

modHabitat <- fitHMM(
  data = STEPS_5min_df.NIGHT,
  nbState = nState,
  stateNames = stateNames,
  dist = dist,
  Par0 = ParHabitat$Par,
  beta0 = ParHabitat$beta,
  formula = formulaHabitat,
  retryFits = 1)

modHabitat

saveRDS(
  modHabitat,
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modHabitat.rds")

# B) TEST EFFECT OF VARIABLES
#----------------------------

cb <- CIbeta(modHabitat)

# Extract beta table
est   <- cb$beta$est
lower <- cb$beta$lower
upper <- cb$beta$upper

# Keep only TOD covariate rows
Habitat_rows <- grepl("^LAND", rownames(est))

b <- data.frame(
  Habitat = rownames(est)[Habitat_rows],
  transition = rep(colnames(est), each = sum(Habitat_rows)),
  estimate = as.vector(est[Habitat_rows, ]),
  lower = as.vector(lower[Habitat_rows, ]),
  upper = as.vector(upper[Habitat_rows, ]))

# Clean TOD labels
b$Habitat <- sub("^LAND", "", b$Habitat)

# Rename habitat codes
habitat_labels <- c(
  "1"  = "Chaume-C.I.-Friche-Jachère",
  "3"  = "Colza",
  "6"  = "Forêt",
  "8"  = "Gravière",
  "9"  = "Haie",
  "11" = "Labour",
  "12" = "Pâture",
  "13" = "Prairie extensive",
  "14" = "Prairie intensive",
  "15" = "Semis inconnu")

# Keep only habitats that have labels
b <- b[b$Habitat %in% names(habitat_labels), ]

# Replace codes by names
b$Habitat <- habitat_labels[b$Habitat]

# Replace state numbers by state names
for(i in seq_along(stateNames)) {
  b$transition <- gsub(
    paste0("\\b", i, "\\b"),
    stateNames[i],
    b$transition)
}

# Remove rows with NaN / NA CI
b <- b[complete.cases(b[, c("estimate", "lower", "upper")]), ]

# Significant if CI does not overlap 0
b$significant <- b$lower > 0 | b$upper < 0

CI_land <- ggplot(b, aes(x = Habitat, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(ymin = lower, ymax = upper,
        colour = significant,
        shape = significant),
        linewidth = 0.5) +
  facet_wrap(~ transition) +
  coord_flip() +
  scale_colour_manual(
    values = c("FALSE" = "grey60", "TRUE" = "red"),
    labels = c("Non significatif", "Significatif")) +
  scale_shape_manual(
    values = c("FALSE" = 1, "TRUE" = 16),
    labels = c("Non significatif", "Significatif")) +
  labs(
    x = "Utilisation de surface",
    y = "Estimation Beta ± 95% IC",
    colour = "",
    shape = "",
    title = "Effet de l'utilisation de surface sur les transitions d'états") +
  theme_classic() 

# C) PREDICT STATE PROBABILITIES FOR EACH HABITAT TYPE
#-----------------------------------------------------

pdf(NULL)

statHabitat <- plotStationary(
  modHabitat,
  plotCI = TRUE,
  alpha = 0.95,
  return = TRUE)

dev.off()

statHabitat_df <- rbind(
  cbind(statHabitat$LAND$resting,   state = "resting"),
  cbind(statHabitat$LAND$foraging,  state = "foraging"),
  cbind(statHabitat$LAND$exploring, state = "exploring"))

names(statHabitat_df)[names(statHabitat_df) == "cov"] <- "LAND"

statHabitat_df$state <- factor(statHabitat_df$state, levels = stateNames)

#----------------------------------------------------
# D) VISUALIZE
#----------------------------------------------------
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

habitat_distrib <- ggplot(
  data = statHabitat_df,
  aes(x = LAND, y = est, fill = state)) +
  
  geom_col(
    position = position_dodge(0.7),
    width = 0.6) +
  
  geom_errorbar(
    aes(ymin = lci, ymax = uci),
    position = position_dodge(0.7),
    width = 0.15) +
  
  labs(
    x = "Utilisation de surface",
    y = "Probabilité d'état",
    fill = "Etat",
    title = "Effet de l'utilisation de surface sur le rythme d'activité du lièvre brun") +
  
  scale_fill_manual(
    values = c("darkred", "red", "orange"),
    labels = c("immobile", "actif", "transit")) +
  
  scale_x_discrete(
    labels = labels_custom) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.90, 0.95))

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/habitat_distrib.png",
  plot = habitat_distrib,
  width = 8,
  height = 6,
  dpi = 300)

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/CI_land.png",
  plot = CI_land,
  width = 8,
  height = 6,
  dpi = 300)

grid.arrange(CI_land ,habitat_distrib)

# CLEAN
rm(b, cb, est, lower, modHabitat, ParHabitat, statHabitat_df, statHabitat, n_land)
rm(formulaHabitat, habitat_distrib, habitat_labels, Habitat_rows, i, CI_land)

#----------------------------------------------------
# 4.32 How does vegetation height affect hare noctural activity?
#----------------------------------------------------

# A) MODEL 
#----------------------------------------------------
# or read it...
modVeg <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modHeight_Veg.rds")

formulaVeg <- ~ veg_height

ParVeg <- getPar0(
  model = mod30,
  formula = formulaVeg)

modVeg <- fitHMM(
  data = STEPS_5min_df.NIGHT,
  nbState = nState,
  stateNames = stateNames,
  dist = dist,
  Par0 = ParVeg$Par,
  beta0 = ParVeg$beta,
  formula = formulaVeg,
  retryFits = 1)

modVeg

saveRDS(
  modVeg,
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modHeight_Veg.rds")

# B) TEST EFFECT OF VARIABLES
#----------------------------

cb <- CIbeta(modVeg)

# Extract beta table
est   <- cb$beta$est
lower <- cb$beta$lower
upper <- cb$beta$upper

# Keep only vegetation height rows
veg_rows <- grepl("^veg_height", rownames(est))

b <- data.frame(
  veg_height = rownames(est)[veg_rows],
  transition = rep(colnames(est), each = sum(veg_rows)),
  estimate = as.vector(est[veg_rows, ]),
  lower = as.vector(lower[veg_rows, ]),
  upper = as.vector(upper[veg_rows, ]))

# Clean labels
b$veg_height <- sub("^veg_height", "", b$veg_height)

# Replace state numbers by state names
for(i in seq_along(stateNames)) {
  b$transition <- gsub(
    paste0("\\b", i, "\\b"),
    stateNames[i],
    b$transition)}

# Remove rows with NaN / NA CI
b <- b[complete.cases(b[, c("estimate", "lower", "upper")]), ]

# Significant if CI does not overlap 0
b$significant <- b$lower > 0 | b$upper < 0

CI_veg <- ggplot(b, aes(x = veg_height, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(
      ymin = lower,
      ymax = upper,
      colour = significant,
      shape = significant),
    linewidth = 0.5) +
  facet_wrap(~ transition) +
  coord_flip() +
  scale_colour_manual(
    values = c("FALSE" = "grey60", "TRUE" = "red"),
    labels = c("Non significatif", "Significatif")) +
  scale_shape_manual(
    values = c("FALSE" = 1, "TRUE" = 16),
    labels = c("Non significatif", "Significatif")) +
  labs(
    x = "Hauteur de végétation",
    y = "Estimation Beta ± 95% IC",
    colour = "",
    shape = "",
    title = "Effet de la hauteur de végétation sur les transitions d'états") +
  theme_classic()

# C) PREDICT STATE PROBABILITIES
#----------------------------------------------------

pdf(NULL)

statVeg <- plotStationary(
  modVeg,
  plotCI = TRUE,
  alpha = 0.95,
  return = TRUE)

dev.off()

statVeg_df <- rbind(
  cbind(statVeg$veg_height$resting,   state = "resting"),
  cbind(statVeg$veg_height$foraging,  state = "foraging"),
  cbind(statVeg$veg_height$exploring, state = "exploring"))

names(statVeg_df)[names(statVeg_df) == "cov"] <- "veg_height"

statVeg_df$veg_height <- factor(
  statVeg_df$veg_height,
  levels = levels(STEPS_5min_df$veg_height))

statVeg_df$state <- factor(
  statVeg_df$state,
  levels = stateNames)

# D) VISUALIZE
#-------------
n_veg <- STEPS_5min_df.NIGHT %>%
  group_by(veg_height) %>%
  summarise(n = n())

labels_custom <- n_veg %>%
  mutate(label = paste0(
    case_when(
      veg_height == "150" ~ "200+",
      TRUE ~ veg_height),
    "\n(n=", n, ")"
  )) %>%
  select(veg_height, label) %>%
  deframe()

veg_distrib <- ggplot(
  data = statVeg_df,
  aes(x = veg_height, y = est, fill = state)) +

  geom_col(
    position = position_dodge(0.7),
    width = 0.6) +

  geom_errorbar(
    aes(ymin = lci, ymax = uci),
    position = position_dodge(0.7),
    width = 0.15) +

  labs(
    x = "Hauteur de végétation (cm)",
    y = "Probabilité d'état",
    fill = "Etat",
    title = "Effet de la hauteur de la végétation sur le rythme d'activité du lièvre brun") +

  scale_fill_manual(
    values = c("darkred", "red", "orange"),
    labels = c("immobile", "actif", "transit")) +
  
  scale_x_discrete(
    labels = labels_custom) +

  theme_minimal() +
  theme(legend.position = c(0.15, 0.90))

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/Height_veg_distrib.png",
  plot = veg_distrib,
  width = 8,
  height = 6,
  dpi = 300)

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/CI_veg.png",
  plot = CI_veg,
  width = 8,
  height = 6,
  dpi = 300)

grid.arrange(CI_veg ,veg_distrib)

# CLEAN
rm(b, cb, est, lower, modVeg, ParVeg, statVeg, statVeg_df, upper, n_veg)
rm(formulaVeg, i, veg_distrib, veg_rows, CI_veg)

##############################################################################################################
# 4.4 EXPLORING ABIOTIC FACTORS
################################

# 4.41 How do hare noctural activity patterns vary across the lunar cycle?
#--------------------------------------------------------------------------
#----------------------------------------------------
# A) MODEL
#----------------------------------------------------
# or read it...
modMoon <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMoon.rds")

formulaMoon <- ~ moon_fraction

ParMoon <- getPar0(
  model = mod30,
  formula = formulaMoon)

modMoon <- fitHMM(
  data = STEPS_5min_df.NIGHT,
  nbState = nState,
  stateNames = stateNames,
  dist = dist,
  Par0 = ParMoon$Par,
  beta0 = ParMoon$beta,
  formula = formulaMoon,
  retryFits = 1)

modMoon

saveRDS(
  modMoon,
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMoon.rds")

# B) TEST EFFECT OF VARIABLES
#----------------------------

cb <- CIbeta(modMoon)

# Extract beta table
est   <- cb$beta$est
lower <- cb$beta$lower
upper <- cb$beta$upper

# Keep only moon_fraction row
moon_rows <- grepl("^moon_fraction$", rownames(est))

b <- data.frame(
  variable = rownames(est)[moon_rows],
  transition = colnames(est),
  estimate = as.numeric(est[moon_rows, ]),
  lower = as.numeric(lower[moon_rows, ]),
  upper = as.numeric(upper[moon_rows, ]))

# Replace state numbers by state names

for(i in seq_along(stateNames)) {
  b$transition <- gsub(
    paste0("\\b", i, "\\b"),
    stateNames[i],
    b$transition)
}

# Remove rows with NaN / NA CI
b <- b[complete.cases(b[, c("estimate", "lower", "upper")]), ]

# Significant if CI does not overlap 0
b$significant <- b$lower > 0 | b$upper < 0

CI_moon <- ggplot(b, aes(x = transition, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(
      ymin = lower,
      ymax = upper,
      colour = significant,
      shape = significant),
    linewidth = 0.5) +
  coord_flip() +
  scale_colour_manual(
    values = c("FALSE" = "grey60", "TRUE" = "red"),
    labels = c("Non significatif", "Significatif")) +
  scale_shape_manual(
    values = c("FALSE" = 1, "TRUE" = 16),
    labels = c("Non significatif", "Significatif")) +
  labs(
    x = "Transitions d'états",
    y = "Estimation Beta ± 95% IC",
    colour = "",
    shape = "",
    title = "Effet de l'illumination de la lune sur les transitions d'états") +
  theme_classic() +
  theme(legend.position = 'none') +
  theme(legend.position = 'none')

# C) PREDICT STATE PROBABILITIES FOR ALL COMBINATIONS
#----------------------------------------------------

moon_seq <- seq(
  min(STEPS_5min_df.NIGHT$moon_fraction, na.rm = TRUE),
  max(STEPS_5min_df.NIGHT$moon_fraction, na.rm = TRUE),
  length.out = 10)

statMoon_df <- data.frame()

pdf(NULL)

statMoon_list <- lapply(moon_seq, function(x) {
  
  s <- plotStationary(
    modMoon,
    covs = data.frame(moon_fraction = x),
    plotCI = TRUE,
    alpha = 0.95,
    return = TRUE)
  
  out <- rbind(
    cbind(s$moon_fraction$resting,   state = "resting"),
    cbind(s$moon_fraction$foraging,  state = "foraging"),
    cbind(s$moon_fraction$exploring, state = "exploring"))

  names(out)[names(out) == "cov"] <- "moon_fraction"
  out
})

statMoon_df <- do.call(rbind, statMoon_list)

dev.off()

statMoon_df$state <- factor(statMoon_df$state, levels = stateNames)

#-------------------------------------------------------------------------------

# D) VISUALIZE
#-------------

# Recontruire axe x après scaling
HARES_5min_df.NIGHT <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/Hares_5min_night.rds")

moon_mean <- mean(HARES_5min_df.NIGHT$moon_fraction, na.rm = TRUE)
moon_sd   <- sd(HARES_5min_df.NIGHT$moon_fraction, na.rm = TRUE)

moon_labels <- seq(0, 1, by = 0.25)

moon_breaks <- (moon_labels - moon_mean) / moon_sd

# Figure
n_moon <- nrow(STEPS_5min_df.NIGHT)

modMoon_distrib <- ggplot(
  data = statMoon_df,
  aes(
    x = moon_fraction,
    y = est,
    color = state,
    fill = state)) +

  geom_ribbon(
    aes(ymin = lci, ymax = uci),
    alpha = 0.2,
    colour = NA) +

  geom_line(linewidth = 1) +

  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = paste0("n = ", n_moon),
    hjust = -0.1,
    vjust = 1.2,
    size = 4) +

  labs(
    x = "Pourcentage d'illumination de la lune",
    y = "Probabilité d'état",
    color = "Etat",
    fill = "Etat",
    title = "Effet du pourcentage d'illumination de la lune sur le rythme d'activité du lièvre brun") +

  scale_x_continuous(
    breaks = moon_breaks,
    labels = scales::percent(moon_labels, accuracy = 1)) +

  scale_color_manual(
    values = c("darkred", "red", "orange"),
    labels = c("immobile", "actif", "transit")) +
  
  scale_fill_manual(
    values = c("darkred", "red", "orange"),
    labels = c("immobile", "actif", "transit")) +
  
  theme_minimal() +
  theme(legend.position = c(0.85, 0.65))

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/modMoon_distrib.png",
  plot = modMoon_distrib,
  width = 8,
  height = 6,
  dpi = 300)

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/CI_moon.png",
  plot = CI_moon,
  width = 8,
  height = 6,
  dpi = 300)

grid.arrange(CI_moon, modMoon_distrib, ncol = 2)

# CLEAN
rm(b, cb, est, lower, modMoon, newcovs, ParMoon, moon_seq, statMoon_df, n_moon)
rm(tmp_list, upper, CI_moon, formulaMoon, i, main_dir, modMoon_distrib, moon_rows)

#-----------------------------------------------------------------------------------------------------------------------

# 4.42 How does snow cover influence the activity patterns of hares?
#--------------------------------------------------------------------

# A) MODEL
#---------
# or read it...
modSNOW <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modSNOW.rds")

formulaSNOW <- ~ SNOW

ParSNOW <- getPar0(
  model = mod30,
  formula = formulaSNOW)

modSNOW <- fitHMM(
  data = STEPS_5min_df.NIGHT,
  nbState = nState,
  stateNames = stateNames,
  dist = dist,
  Par0 = ParSNOW$Par,
  beta0 = ParSNOW$beta,
  formula = formulaSNOW,
  retryFits = 1)

modSNOW

saveRDS(
  modSNOW,
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modSNOW.rds")

# B) TEST EFFECT OF VARIABLES
#----------------------------

cb <- CIbeta(modSNOW)

# Extract beta table
est   <- cb$beta$est
lower <- cb$beta$lower
upper <- cb$beta$upper

# Keep only SNOW covariate rows
snow_rows <- grepl("^SNOW", rownames(est))

b <- data.frame(
  SNOW = rownames(est)[snow_rows],
  transition = rep(colnames(est), each = sum(snow_rows)),
  estimate = as.vector(est[snow_rows, ]),
  lower = as.vector(lower[snow_rows, ]),
  upper = as.vector(upper[snow_rows, ]))

# Clean Snow labels
b$SNOW <- sub("^SNOW", "", b$SNOW)

# Replace state numbers by state names
for(i in seq_along(stateNames)) {
  b$transition <- gsub(
    paste0("\\b", i, "\\b"),
    stateNames[i],
    b$transition)
}

# Remove rows with NaN / NA CI
b <- b[complete.cases(b[, c("estimate", "lower", "upper")]), ]

# Significant if CI does not overlap 0
b$significant <- b$lower > 0 | b$upper < 0

CI_snow <- ggplot(b, aes(x = SNOW, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(ymin = lower, ymax = upper,
        colour = significant,
        shape = significant),
    linewidth = 0.5) +
  facet_wrap(~ transition) +
  coord_flip() +
  scale_colour_manual(
    values = c("FALSE" = "grey60", "TRUE" = "red"),
    labels = c("Non significatif", "Significatif")) +
  scale_shape_manual(
    values = c("FALSE" = 1, "TRUE" = 16),
    labels = c("Non significatif", "Significatif")) +
  labs(
    x = "Couverture neigeuse",
    y = "Estimation Beta ± 95% IC",
    colour = "",
    shape = "",
    title = "Effet de la couverture neigeuse sur les transitions d'états comparée à l'absence de neige") +
  theme_classic()

# C) PREDICT STATE PROBABILITIES FOR EACH CATEGORY
#----------------------------------------------------

pdf(NULL)

statSNOW <- plotStationary(
  modSNOW,
  plotCI = TRUE,
  alpha = 0.95,
  return = TRUE)

dev.off()

statSNOW_df <- rbind(
  transform(statSNOW$SNOW$resting,   state = "resting"),
  transform(statSNOW$SNOW$foraging,  state = "foraging"),
  transform(statSNOW$SNOW$exploring, state = "exploring"))

names(statSNOW_df)[names(statSNOW_df) == "cov"] <- "SNOW"

statSNOW_df$SNOW <- factor(
  statSNOW_df$SNOW,
  levels = c(
    "Pas de neige",
    "Peu",
    "Modérée",
    "Forte"))

statSNOW_df$state <- factor(
  statSNOW_df$state,
  levels = c("resting", "foraging", "exploring"))

# D) VISUALIZE
#-------------
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

snow_distrib <- ggplot(
  data = statSNOW_df,
  aes(x = SNOW, y = est, fill = state)) +
  
  geom_col(
    position = position_dodge(0.7),
    width = 0.6) +
  
  geom_errorbar(
    aes(ymin = lci, ymax = uci),
    position = position_dodge(0.7),
    width = 0.15) +
  
  labs(
    x = "Couverture neigeuse",
    y = "Probabilité d'état",
    fill = "Etat",
    title = "Effet de la couverture neigeuse sur le rythme d'activité du lièvre brun") +
  
  scale_fill_manual(
    values = c("darkred", "red", "orange"),
    labels = c("immobile", "actif", "transit")) +
  
  scale_x_discrete(
    labels = labels_custom) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.20, 0.90))

ggsave(
 "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/snow_distrib.png",
 plot = snow_distrib,
 width = 8,
 height = 6,
 dpi = 300)

ggsave(
 "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/CI_snow.png",
 plot = CI_snow,
 width = 8,
 height = 6,
 dpi = 300)

grid.arrange(CI, snow_distrib, ncol = 2)

rm(b, cb, est, lower, modSNOW, ParSNOW, statSNOW, statSNOW_df, n_snow)
rm(CI_snow, formulaSNOW, i, snow_distrib, snow_rows)

# ---------------------------------------------------------------------

# 4.43 How does temperature, precipitation and wind influence the noctural activity patterns of hares?
#-------------------------------------------------------------------------------------

# 4.43.1 Températures
# --------------------

# A) MODEL
#----------------------------------------------------
# or read it...
modTemp <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modTemp.rds")

formulaTemp <- ~ temp

ParTemp <- getPar0(
  model = mod30,
  formula = formulaTemp)

modTemp <- fitHMM(
  data = STEPS_5min_df.NIGHT,
  nbState = nState,
  stateNames = stateNames,
  dist = dist,
  Par0 = ParTemp$Par,
  beta0 = ParTemp$beta,
  formula = formulaTemp,
  retryFits = 1)

modTemp

saveRDS(
  modTemp,
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modTemp.rds")

# B) TEST EFFECT OF VARIABLES
#----------------------------

cb <- CIbeta(modTemp)

# Extract beta table
est   <- cb$beta$est
lower <- cb$beta$lower
upper <- cb$beta$upper

# Keep only temp row
temp_rows <- grepl("^temp$", rownames(est))

b <- data.frame(
  variable = rownames(est)[temp_rows],
  transition = colnames(est),
  estimate = as.numeric(est[temp_rows, ]),
  lower = as.numeric(lower[temp_rows, ]),
  upper = as.numeric(upper[temp_rows, ]))

# Replace state numbers by state names
for(i in seq_along(stateNames)) {
  b$transition <- gsub(
    paste0("\\b", i, "\\b"),
    stateNames[i],
    b$transition)
}

# Remove rows with NaN / NA CI
b <- b[complete.cases(b[, c("estimate", "lower", "upper")]), ]

# Significant if CI does not overlap 0
b$significant <- b$lower > 0 | b$upper < 0

CI_temp <- ggplot(b, aes(x = transition, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(
      ymin = lower,
      ymax = upper,
      colour = significant,
      shape = significant),
    linewidth = 0.5) +
  coord_flip() +
  scale_colour_manual(
    values = c("FALSE" = "grey60", "TRUE" = "red"),
    labels = c("Non significatif", "Significatif")) +
  scale_shape_manual(
    values = c("FALSE" = 1, "TRUE" = 16),
    labels = c("Non significatif", "Significatif")) +
  labs(
    x = "Transitions d'états",
    y = "Estimation Beta ± 95% IC",
    colour = "",
    shape = "",
    title = "Effet de la température sur les transitions d'états") +
  theme_classic() 

# C) PREDICT STATE PROBABILITIES FOR ALL COMBINATIONS
#----------------------------------------------------

temp_seq <- seq(
  min(STEPS_5min_df.NIGHT$temp, na.rm = TRUE),
  max(STEPS_5min_df.NIGHT$temp, na.rm = TRUE),
  length.out = 10)

statTemp_df <- data.frame()

pdf(NULL)

statTemp_list <- lapply(temp_seq, function(x) {
  
  s <- plotStationary(
    modTemp,
    covs = data.frame(temp = x),
    plotCI = TRUE,
    alpha = 0.95,
    return = TRUE)
  
  out <- rbind(
    cbind(s$temp$resting,   state = "resting"),
    cbind(s$temp$foraging,  state = "foraging"),
    cbind(s$temp$exploring, state = "exploring"))

  names(out)[names(out) == "cov"] <- "temp"
  out
})

statTemp_df <- do.call(rbind, statTemp_list)

dev.off()

statTemp_df$state <- factor(statTemp_df$state, levels = stateNames)

#-------------------------------------------------------------------------------

# D) VISUALIZE
#-------------

# Recontruire axe x après scaling
HARES_5min_df.NIGHT <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/Hares_5min_night.rds")

temp_mean <- mean(HARES_5min_df.NIGHT$`température_°C Neuchâtel`, na.rm = TRUE)
temp_sd   <- sd(HARES_5min_df.NIGHT$`température_°C Neuchâtel`, na.rm = TRUE)

temp_labels <- seq(
  floor(min(HARES_5min_df.NIGHT$`température_°C Neuchâtel`, na.rm = TRUE)),
  ceiling(max(HARES_5min_df.NIGHT$`température_°C Neuchâtel`, na.rm = TRUE)),
  by = 5)

temp_breaks <- (temp_labels - temp_mean) / temp_sd

# Figure
n_temp <- nrow(STEPS_5min_df.NIGHT)

modTemp_distrib <- ggplot(
  data = statTemp_df,
  aes(
    x = temp,
    y = est,
    color = state,
    fill = state)) +

  geom_ribbon(
    aes(ymin = lci, ymax = uci),
    alpha = 0.2,
    colour = NA) +

  geom_line(linewidth = 1) +

  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = paste0("n = ", n_temp),
    hjust = -0.1,
    vjust = 1.2,
    size = 4) +

  labs(
    x = "Température (°C)",
    y = "Probablilité d'état",
    color = "Etat",
    fill = "Etat",
    title = "Effet de la température sur le rythme d'ctivité du lièvre brun") +

  scale_x_continuous(
    breaks = temp_breaks,
    labels = temp_labels) +

  scale_color_manual(
    values = c("darkred", "red", "orange"),
    labels = c("immobile", "actif", "transit")) +

  scale_fill_manual(
    values = c("darkred", "red", "orange"),
    labels = c("immobile", "actif", "transit")) +
  
  theme_minimal() +
  theme(legend.position = c(0.85, 0.65))

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/modTemp_distrib.png",
  plot = modTemp_distrib,
  width = 8,
  height = 6,
  dpi = 300)

ggsave(
 "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/CI_temp.png",
 plot = CI_temp,
 width = 8,
 height = 6,
 dpi = 300)

grid.arrange(CI_temp, modTemp_distrib, ncol = 2)

# CLEAN
rm(b, cb, est, lower, modTemp, newcovs, ParTemp, temp_seq, statTemp_df, n_temp)
rm(tmp_list, upper, CI_temp, formulaTemp, i, modTemp_distrib, temp_rows)

#----------------------------------------------------------------------------------------------------------

# 4.43.2 Précipitations
# ----------------------

# A) MODEL
#----------------------------------------------------
# or read it...
modPrecip <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modPrecip.rds")

formulaPrecip <- ~ precip

ParPrecip <- getPar0(
  model = mod30,
  formula = formulaPrecip)

modPrecip <- fitHMM(
  data = STEPS_5min_df.NIGHT,
  nbState = nState,
  stateNames = stateNames,
  dist = dist,
  Par0 = ParPrecip$Par,
  beta0 = ParPrecip$beta,
  formula = formulaPrecip,
  retryFits = 1)

modPrecip

saveRDS(
  modPrecip,
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modPrecip.rds")

# B) TEST EFFECT OF VARIABLES
#----------------------------

cb <- CIbeta(modPrecip)

# Extract beta table
est   <- cb$beta$est
lower <- cb$beta$lower
upper <- cb$beta$upper

# Keep only precip row
precip_rows <- grepl("^precip$", rownames(est))

b <- data.frame(
  variable = rownames(est)[precip_rows],
  transition = colnames(est),
  estimate = as.numeric(est[precip_rows, ]),
  lower = as.numeric(lower[precip_rows, ]),
  upper = as.numeric(upper[precip_rows, ]))

# Replace state numbers by state names
for(i in seq_along(stateNames)) {
  b$transition <- gsub(
    paste0("\\b", i, "\\b"),
    stateNames[i],
    b$transition)
}

# Remove rows with NaN / NA CI
b <- b[complete.cases(b[, c("estimate", "lower", "upper")]), ]

# Significant if CI does not overlap 0
b$significant <- b$lower > 0 | b$upper < 0

CI_precip <- ggplot(b, aes(x = transition, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(
      ymin = lower,
      ymax = upper,
      colour = significant,
      shape = significant),
    linewidth = 0.5) +
  coord_flip() +
  scale_colour_manual(
    values = c("FALSE" = "grey60", "TRUE" = "red"),
    labels = c("Non significatif", "Significatif")) +
  scale_shape_manual(
    values = c("FALSE" = 1, "TRUE" = 16),
    labels = c("Non significatif", "Significatif")) +
  labs(
    x = "Transitions d'états",
    y = "Estimation Beta ± 95% IC",
    colour = "",
    shape = "",
    title = "Effet des précipitations sur les transitions d'états") +
  theme_classic() 

# C) PREDICT STATE PROBABILITIES FOR ALL COMBINATIONS
#----------------------------------------------------

precip_seq <- seq(
  min(STEPS_5min_df.NIGHT$precip, na.rm = TRUE),
  max(STEPS_5min_df.NIGHT$precip, na.rm = TRUE),
  length.out = 10)

statPrecip_df <- data.frame()

pdf(NULL)

statPrecip_list <- lapply(precip_seq, function(x) {

  s <- plotStationary(
    modPrecip,
    covs = data.frame(precip = x),
    plotCI = TRUE,
    alpha = 0.95,
    return = TRUE)

  out <- rbind(
    cbind(s$precip$resting,   state = "resting"),
    cbind(s$precip$foraging,  state = "foraging"),
    cbind(s$precip$exploring, state = "exploring"))

  names(out)[names(out) == "cov"] <- "precip"
  out
})

statPrecip_df <- do.call(rbind, statPrecip_list)

dev.off()

statPrecip_df$state <- factor(statPrecip_df$state, levels = stateNames)

#-------------------------------------------------------------------------------

# D) VISUALIZE
#-------------

# Recontruire axe x après scaling
HARES_5min_df.NIGHT <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/Hares_5min_night.rds")

precip_mean <- mean(HARES_5min_df.NIGHT$`précipitation_mm Neuchâtel`, na.rm = TRUE)
precip_sd   <- sd(HARES_5min_df.NIGHT$`précipitation_mm Neuchâtel`, na.rm = TRUE)

precip_labels <- seq(
  floor(min(HARES_5min_df.NIGHT$`précipitation_mm Neuchâtel`, na.rm = TRUE)),
  ceiling(max(HARES_5min_df.NIGHT$`précipitation_mm Neuchâtel`, na.rm = TRUE)),
  by = 0.5)

precip_breaks <- (precip_labels - precip_mean) / precip_sd

# Figure
n_precip <- nrow(STEPS_5min_df.NIGHT)

modPrecip_distrib <- ggplot(
  data = statPrecip_df,
  aes(
    x = precip,
    y = est,
    color = state,
    fill = state)) +

  geom_ribbon(
    aes(ymin = lci, ymax = uci),
    alpha = 0.2,
    colour = NA) +

  geom_line(linewidth = 1) +

  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = paste0("n = ", n_precip),
    hjust = -0.1,
    vjust = 1.2,
    size = 4) +
  
  labs(
    x = "Précipitation (mm/h)",
    y = "Probabilité d'état",
    color = "Etat",
    fill = "Etat",
    title = "Effet des précipitations sur le rythme d'activité du lièvre brun") +

  scale_x_continuous(
    breaks = precip_breaks,
    labels = precip_labels) +

  scale_color_manual(
    values = c("darkred", "red", "orange"),
    labels = c("immobile", "actif", "transit")) +
  
  scale_fill_manual(
    values = c("darkred", "red", "orange"),
    labels = c("immobile", "actif", "transit")) +
  
  theme_minimal() +
  theme(legend.position = c(0.15, 0.65))

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/modPrecip_distrib.png",
  plot = modPrecip_distrib,
  width = 8,
  height = 6,
  dpi = 300)

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/CI_precip.png",
  plot = CI_precip,
  width = 8,
  height = 6,
  dpi = 300)

grid.arrange(CI, modPrecip_distrib, ncol = 2)

# CLEAN
rm(b, cb, est, lower, modPrecip, newcovs, ParPrecip, precip_seq, statPrecip_df, n_precip)
rm(tmp_list, upper, CI_precip, formulaPrecip, i, modPrecip_distrib, precip_rows)

# --------------------------------------------------------------
# 4.43.3 Vent
# ----------------------

# A) MODEL
#----------------------------------------------------
# or read it...
modVent <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modVent.rds")

formulaVent <- ~ vent

ParVent <- getPar0(
  model = mod30,
  formula = formulaVent)

modVent <- fitHMM(
  data = STEPS_5min_df.NIGHT,
  nbState = nState,
  stateNames = stateNames,
  dist = dist,
  Par0 = ParVent$Par,
  beta0 = ParVent$beta,
  formula = formulaVent,
  retryFits = 1)

modVent

saveRDS(
  modVent,
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modVent.rds")

# B) TEST EFFECT OF VARIABLES
#----------------------------

cb <- CIbeta(modVent)

# Extract beta table
est   <- cb$beta$est
lower <- cb$beta$lower
upper <- cb$beta$upper

# Keep only vent row
vent_rows <- grepl("^vent$", rownames(est))

b <- data.frame(
  variable = rownames(est)[vent_rows],
  transition = colnames(est),
  estimate = as.numeric(est[vent_rows, ]),
  lower = as.numeric(lower[vent_rows, ]),
  upper = as.numeric(upper[vent_rows, ]))

# Replace state numbers by state names
for(i in seq_along(stateNames)) {
  b$transition <- gsub(
    paste0("\\b", i, "\\b"),
    stateNames[i],
    b$transition)
}

# Remove rows with NaN / NA CI
b <- b[complete.cases(b[, c("estimate", "lower", "upper")]), ]

# Significant if CI does not overlap 0
b$significant <- b$lower > 0 | b$upper < 0

CI_vent <- ggplot(b, aes(x = transition, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(
      ymin = lower,
      ymax = upper,
      colour = significant,
      shape = significant),
    linewidth = 0.5) +
  coord_flip() +
  scale_colour_manual(
    values = c("FALSE" = "grey60", "TRUE" = "red"),
    labels = c("Non significatif", "Significatif")) +
  scale_shape_manual(
    values = c("FALSE" = 1, "TRUE" = 16),
    labels = c("Non significatif", "Significatif")) +
  labs(
    x = "Transitions d'états",
    y = "Estimation Beta ± 95% IC",
    colour = "",
    shape = "",
    title = "Effet du vent sur les transitions d'états") +
  theme_classic()

# C) PREDICT STATE PROBABILITIES FOR ALL COMBINATIONS
#----------------------------------------------------

vent_seq <- seq(
  min(STEPS_5min_df.NIGHT$vent, na.rm = TRUE),
  max(STEPS_5min_df.NIGHT$vent, na.rm = TRUE),
  length.out = 10)

statVent_df <- data.frame()

pdf(NULL)

statVent_list <- lapply(vent_seq, function(x) {

  s <- plotStationary(
    modVent,
    covs = data.frame(vent = x),
    plotCI = TRUE,
    alpha = 0.95,
    return = TRUE)

  out <- rbind(
    cbind(s$vent$resting,   state = "resting"),
    cbind(s$vent$foraging,  state = "foraging"),
    cbind(s$vent$exploring, state = "exploring"))

  names(out)[names(out) == "cov"] <- "vent"
  out
})

statVent_df <- do.call(rbind, statVent_list)

dev.off()

statVent_df$state <- factor(statVent_df$state, levels = stateNames)

#-------------------------------------------------------------------------------

# D) VISUALIZE
#-------------

# Recontruire axe x après scaling
HARES_5min_df.NIGHT <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/Hares_5min_night.rds")

vent_mean <- mean(HARES_5min_df.NIGHT$`vent (km/h) Neuchâtel`, na.rm = TRUE)
vent_sd   <- sd(HARES_5min_df.NIGHT$`vent (km/h) Neuchâtel`, na.rm = TRUE)

vent_labels <- seq(
  floor(min(HARES_5min_df.NIGHT$`vent (km/h) Neuchâtel`, na.rm = TRUE)),
  ceiling(max(HARES_5min_df.NIGHT$`vent (km/h) Neuchâtel`, na.rm = TRUE)),
  by = 5)

vent_breaks <- (vent_labels - vent_mean) / vent_sd

#Figure
n_vent <- nrow(STEPS_5min_df.NIGHT)

modVent_distrib <- ggplot(
  data = statVent_df,
  aes(
    x = vent,
    y = est,
    color = state,
    fill = state)) +

  geom_ribbon(
    aes(ymin = lci, ymax = uci),
    alpha = 0.2,
    colour = NA) +

  geom_line(linewidth = 1) +

  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = paste0("n = ", n_vent),
    hjust = -0.1,
    vjust = 1.2,
    size = 4) +

  labs(
    x = "Vent (km/h)",
    y = "Probabilité d'état",
    color = "Etat",
    fill = "Etat",
    title = "Effet du vent sur le rythme d'activité du lièvre brun") +

  scale_x_continuous(
    breaks = vent_breaks,
    labels = vent_labels) +

  scale_color_manual(values = c("darkred", "red", "orange")) +
  scale_fill_manual(values = c("darkred", "red", "orange")) +
  theme_minimal() +
  theme(legend.position = c(0.85, 0.65))

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/modVent_distrib.png",
  plot = modVent_distrib,
  width = 8,
  height = 6,
  dpi = 300)

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/CI_vent.png",
  plot = CI_vent,
  width = 8,
  height = 6,
  dpi = 300)

grid.arrange(CI_vent, modVent_distrib, ncol = 2)

# CLEAN
rm(b, cb, est, lower, modVent, newcovs, ParVent, vent_seq, statVent_df, n_vent)
rm(tmp_list, upper, CI_vent, formulaVent, i, modVent_distrib, vent_rows)

########################################################################################################################

# 4.5 EXPLORING ALAN 
#####################

# 4.51 How does distance to the closest lamp influence the noctural activity patterns of hares?
#-----------------------------------------------------------------------------------------------

#-----------------------------------------------------
# A) MODEL 
#-----------------------------------------------------
# or read it...
modLIGHT <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modLIGHT.rds")

formulaLIGHT <- ~ LIGHT

ParLIGHT <- getPar0(
  model = mod30,
  formula = formulaLIGHT)

modLIGHT <- fitHMM(
  data = STEPS_5min_df.NIGHT,
  nbState = nState,
  stateNames = stateNames,
  dist = dist,
  Par0 = ParLIGHT$Par,
  beta0 = ParLIGHT$beta,
  formula = formulaLIGHT,
  retryFits = 1)

modLIGHT

saveRDS(
  modLIGHT,
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modLIGHT.rds")

# B) TEST EFFECT OF VARIABLES
#----------------------------

cb <- CIbeta(modLIGHT)

# Extract beta table
est   <- cb$beta$est
lower <- cb$beta$lower
upper <- cb$beta$upper

# Keep only LIGHT row
LIGHT_rows <- grepl("^LIGHT$", rownames(est))

b <- data.frame(
  variable = rownames(est)[LIGHT_rows],
  transition = colnames(est),
  estimate = as.numeric(est[LIGHT_rows, ]),
  lower = as.numeric(lower[LIGHT_rows, ]),
  upper = as.numeric(upper[LIGHT_rows, ]))

# Replace state numbers by state names
for(i in seq_along(stateNames)) {
  b$transition <- gsub(
    paste0("\\b", i, "\\b"),
    stateNames[i],
    b$transition)}

# Remove rows with NaN / NA CI
b <- b[complete.cases(b[, c("estimate", "lower", "upper")]), ]

# Significant if CI does not overlap 0
b$significant <- b$lower > 0 | b$upper < 0

CI_light <- ggplot(b, aes(x = transition, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(
      ymin = lower,
      ymax = upper,
      colour = significant,
      shape = significant),
    linewidth = 0.5) +
  coord_flip() +
  scale_colour_manual(
    values = c("FALSE" = "grey60", "TRUE" = "red"),
    labels = c("Non significatif", "Significatif")) +
  scale_shape_manual(
    values = c("FALSE" = 1, "TRUE" = 16),
    labels = c("Non significatif", "Significatif")) +
  labs(
    x = "Transitions d'états",
    y = "Estimation Beta ± 95% IC",
    colour = "",
    shape = "",
    title = "Effet de la distance au lampadaire le plus proche sur les transitions d'états\nIndépendamment de la variable Allumage - Extinction") +
  theme_classic()

# C) PREDICT STATE PROBABILITIES FOR ALL COMBINATIONS
#----------------------------------------------------

light_seq <- seq(
  min(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  max(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  length.out = 10)

statLIGHT_df <- data.frame()

pdf(NULL)

statlight_list <- lapply(light_seq, function(x) {

    s <- plotStationary(
      modLIGHT,
      covs = data.frame(LIGHT = x),
      plotCI = TRUE,
      alpha = 0.95,
      return = TRUE)

    out <- rbind(
      cbind(s$LIGHT$resting,   state = "resting"),
      cbind(s$LIGHT$foraging,  state = "foraging"),
      cbind(s$LIGHT$exploring, state = "exploring"))

    names(out)[names(out) == "cov"] <- "LIGHT"

    out
  })

statLIGHT_df <- do.call(rbind, statlight_list)

dev.off()

statLIGHT_df$state <- factor(statLIGHT_df$state, levels = stateNames)

# D) VISUALIZE
#-------------

# Recontruire axe x après scaling
HARES_5min_df.NIGHT <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/Hares_5min_night.rds")

light_mean <- mean(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)
light_sd   <- sd(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)

light_labels <- seq(
  floor(min(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)),
  ceiling(max(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)),
  by = 200)

light_breaks <- (light_labels - light_mean) / light_sd

# Figure
n_light <- nrow(STEPS_5min_df.NIGHT)

Dist_light_distrib <- ggplot(
  data = statLIGHT_df,
  aes(
    x = LIGHT,
    y = est,
    color = state,
    fill = state)) +

  geom_ribbon(
    aes(ymin = lci, ymax = uci),
    alpha = 0.2,
    colour = NA) +

  geom_line(linewidth = 1) +

  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = paste0("n = ", n_light),
    hjust = -0.1,
    vjust = 1.2,
    size = 4) +

  labs(
    x = "Distance au lampadaire le plus proche (m)",
    y = "Probabilité d'état",
    color = "Etat",
    fill = "Etat",
    title = "Effet de la distance au lampadaire le plus proche sur le rythme d'activité du lièvre brun\nIndépendamment de la variable Allumage - Extinction") +

  scale_x_continuous(
    breaks = light_breaks,
    labels = light_labels) +

  scale_color_manual(
    values = c("darkred", "red", "orange"),
    labels = c("immobile", "actif", "transit")) +
  scale_fill_manual(
    values = c("darkred", "red", "orange"),
    labels = c("immobile", "actif", "transit")) +
  theme_minimal() +
  theme(legend.position = c(0.85, 0.85))

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/Dist_light_distrib.png",
  plot = Dist_light_distrib,
  width = 8,
  height = 6,
  dpi = 300)

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/CI_light.png",
  plot = CI_light,
  width = 8,
  height = 6,
  dpi = 300)

grid.arrange(CI_light, Dist_light_distrib, ncol = 2)

# CLEAN
rm(b, cb, dist, est, lower, modLIGHT, newcovs, ParLIGHT, statLIGHT_df, n_light)
rm(tmp_list, upper, CI, Dist_light_distrib, i, LIGHT_rows, light_seq)
rm(formulaLIGHT)

#-----------------------------------------------------------------------------------------------------------------------

# 4.52 How does ALAN (switch ON / OFF) influence the noctural activity patterns of hares?
#-----------------------------------------------------------------------------------------

# A) MODEL
#---------
# or read it...
modALAN <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modALAN.rds")

formulaALAN <- ~ ALAN

ParALAN <- getPar0(
  model = mod30,
  formula = formulaALAN)

modALAN <- fitHMM(
  data = STEPS_5min_df.NIGHT,
  nbState = nState,
  stateNames = stateNames,
  dist = dist,
  Par0 = ParALAN$Par,
  beta0 = ParALAN$beta,
  formula = formulaALAN,
  retryFits = 1)

modALAN

saveRDS(
  modALAN,
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modALAN.rds")

# B) TEST EFFECT OF VARIABLES
#----------------------------

cb <- CIbeta(modALAN)

# Extract beta table
est   <- cb$beta$est
lower <- cb$beta$lower
upper <- cb$beta$upper

# Keep only ALAN rows
ALAN_rows <- grepl("^ALAN", rownames(est))

b <- data.frame(
  ALAN = rownames(est)[ALAN_rows],
  transition = rep(colnames(est), each = sum(ALAN_rows)),
  estimate = as.vector(est[ALAN_rows, ]),
  lower = as.vector(lower[ALAN_rows, ]),
  upper = as.vector(upper[ALAN_rows, ])
)

# Clean labels
b$ALAN <- sub("^ALAN", "", b$ALAN)

# Optional: nicer labels
alan_labels <- c(
  "OFF" = "Eteint",
  "ON" = "Allumé")

# Keep only defined categories
b <- b[b$ALAN %in% names(alan_labels), ]

# Replace codes by labels
b$ALAN <- alan_labels[b$ALAN]

# Preserve order
b$ALAN <- factor(
  b$ALAN,
  levels = alan_labels)

# Replace state numbers by state names

for(i in seq_along(stateNames)) {
  b$transition <- gsub(
    paste0("\\b", i, "\\b"),
    stateNames[i],
    b$transition)
}

# Remove rows with NaN / NA CI
b <- b[complete.cases(b[, c("estimate", "lower", "upper")]), ]

# Significant if CI does not overlap 0
b$significant <- b$lower > 0 | b$upper < 0

CI_ALAN <- ggplot(b, aes(x = ALAN, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(
    aes(
      ymin = lower,
      ymax = upper,
      colour = significant,
      shape = significant),
    linewidth = 0.5) +
  facet_wrap(~ transition) +
  coord_flip() +
  scale_colour_manual(
    values = c("FALSE" = "grey60", "TRUE" = "red"),
    labels = c("Non significatif", "Significatif")) +
  scale_shape_manual(
    values = c("FALSE" = 1, "TRUE" = 16),
    labels = c("Non significatif", "Significatif")) +
  labs(
    x = "ALAN",
    y = "Estimation Beta ± 95% IC",
    colour = "",
    shape = "",
    title = "Effet de l'allumage de l'éclairage public sur les transtions d'états\ncomparé à la période d'extinction") +
  theme_classic()

# C) PREDICT STATE PROBABILITIES FOR ALL COMBINATIONS
#----------------------------------------------------

newcovs <- expand.grid(
  ALAN = c("ON", "OFF"))

statALAN_df <- data.frame()

pdf(NULL)

for (i in 1:nrow(newcovs)) {

  s <- plotStationary(
    modALAN,
    covs = newcovs[i, , drop = FALSE],
    plotCI = TRUE,
    alpha = 0.95,
    return = TRUE)

  out <- rbind(
    cbind(s$ALAN$resting,   state = "resting"),
    cbind(s$ALAN$foraging,  state = "foraging"),
    cbind(s$ALAN$exploring, state = "exploring"))

  out <- out[out$cov == newcovs$ALAN[i], ]
  out$ALAN <- newcovs$ALAN[i]
  out$cov <- NULL

  statALAN_df <- rbind(statALAN_df, out)
}

dev.off()

statALAN_df$ALAN <- factor(statALAN_df$ALAN, levels = c("ON", "OFF"))
statALAN_df$state <- factor(statALAN_df$state, levels = stateNames)

# D) VISUALIZE
#-------------
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

ALAN_distrib <- ggplot(
  data = statALAN_df,
  aes(
    x = ALAN,
    y = est,
    fill = state)) +
  
  geom_col(
    position = position_dodge(0.7),
    width = 0.6) +

  geom_errorbar(
    aes(ymin = lci, ymax = uci),
    position = position_dodge(0.7),
    width = 0.15) +

  labs(
    x = "ALAN",
    y = "Probabilité d'état",
    fill = "Etat",
    title = "Effet de l'allumage et extinction de l'éclairage public sur le rythme d'activité du lièvre brun\nIndépendamment de la distance au lampadaire le plus proche") +

  scale_x_discrete(
    labels = labels_custom) +

  scale_fill_manual(
    values = c("darkred", "red", "orange"),
    labels = c("immobile", "actif", "transit")) +
  
  theme_minimal() +
  theme(legend.position = c(0.85, 0.85))

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/ALAN_distrib.png",
  plot = ALAN_distrib,
  width = 8,
  height = 6,
  dpi = 300)

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/CI_ALAN.png",
  plot = CI_ALAN,
  width = 8,
  height = 6,
  dpi = 300)

grid.arrange(CI, ALAN_distrib, ncol = 2)

# CLEAN
rm(b, cb, est, lower, newcovs, out, s, statALAN_df)
rm(upper, ALAN_distrib, alan_labels, ALAN_rows, CI_ALAN, i)

#-----------------------------------------------------------------------------------------------------------------------

# 4.53 How does ALAN, coupled with distance to the closest lamp, influence the noctural activity patterns of hares?
#-------------------------------------------------------------------------------------------------------------------

# A) MODEL 
#-----------------------------------------------------
# or read it...
modDistALAN <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modDistALAN.rds")

formulaDistALAN <- ~ ALAN * LIGHT

ParDistALAN <- getPar0(
  model = mod30,
  formula = formulaDistALAN)

modDistALAN <- fitHMM(
  data = STEPS_5min_df.NIGHT,
  nbState = nState,
  stateNames = stateNames,
  dist = dist,
  Par0 = ParDistALAN$Par,
  beta0 = ParDistALAN$beta,
  formula = formulaDistALAN,
  retryFits = 1)

modDistALAN

saveRDS(
  modDistALAN,
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modDistALAN.rds")

# B) TEST EFFECT OF VARIABLES
#----------------------------

cb <- CIbeta(modDistALAN)

# Extract beta table
est   <- cb$beta$est
lower <- cb$beta$lower
upper <- cb$beta$upper

# Remove intercept
rows_keep <- rownames(est) != "(Intercept)"

b <- data.frame(
  term = rep(rownames(est)[rows_keep], times = ncol(est)),
  transition = rep(colnames(est), each = sum(rows_keep)),
  estimate = as.vector(est[rows_keep, ]),
  lower = as.vector(lower[rows_keep, ]),
  upper = as.vector(upper[rows_keep, ]))

# Replace state numbers by state names
for(i in seq_along(stateNames)) {
  b$transition <- gsub(
    paste0("\\b", i, "\\b"),
    stateNames[i],
    b$transition)
}

# Clean labels
b$term <- gsub("ALANON", "Allumé", b$term)
b$term <- gsub(":LIGHT", " Allumage X Distance au \nlampadaire le plus proche", b$term)
b$term <- gsub("^LIGHT$", "Distance au lampadaire\nle plus proche", b$term)

# Remove NA rows
b <- b[complete.cases(b[, c("estimate", "lower", "upper")]), ]

# Significant if CI excludes 0
b$significant <- b$lower > 0 | b$upper < 0

CI_distALAN <- ggplot(b, aes(x = term, y = estimate)) +

  geom_hline(yintercept = 0, linetype = "dashed") +

  geom_pointrange(
    aes(
      ymin = lower,
      ymax = upper,
      colour = significant,
      shape = significant),
      linewidth = 0.5) +

  facet_wrap(~ transition) +

  coord_flip() +

  scale_colour_manual(
    values = c(
      "FALSE" = "grey60",
      "TRUE" = "red"),
    labels = c(
      "Non significatif", "Significatif")) +

  scale_shape_manual(
    values = c(
      "FALSE" = 1,
      "TRUE" = 16),
    labels = c(
      "Non significatif", "Significatif")) +

  labs(
    x = "",
    y = "Estimation Beta ± 95% IC",
    colour = "",
    shape = "",
    title = "Effet de l'allumage, de la distance au lampadaire le plus proche \net des 2 combinés sur les transitions d'états\ncomparé à l'extinction ") +

  theme_classic()

#-------------------------------------------------------------------------------

# C) PREDICT STATE PROBABILITIES FOR ALL COMBINATIONS
#----------------------------------------------------

light_seq <- seq(
  min(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  max(STEPS_5min_df.NIGHT$LIGHT, na.rm = TRUE),
  length.out = 50)

newcovs <- expand.grid(
  ALAN = c("OFF", "ON"))

statDistALAN_df <- data.frame()

pdf(NULL)

for (i in 1:nrow(newcovs)) {

  ALANdist_list <- lapply(light_seq, function(x) {

    s <- plotStationary(
      modDistALAN,
      covs = data.frame(
        ALAN = newcovs$ALAN[i],
        LIGHT = x),
      plotCI = TRUE,
      alpha = 0.95,
      return = TRUE)

    out <- rbind(
      cbind(s$LIGHT$resting,   state = "resting"),
      cbind(s$LIGHT$foraging,  state = "foraging"),
      cbind(s$LIGHT$exploring, state = "exploring"))

    names(out)[names(out) == "cov"] <- "LIGHT"

    out$ALAN <- newcovs$ALAN[i]

    out
  })

  statDistALAN_df <- rbind(
    statDistALAN_df,
    do.call(rbind, ALANdist_list))
}

dev.off()

statDistALAN_df$ALAN <- factor(
  statDistALAN_df$ALAN,
  levels = c("OFF", "ON"))

statDistALAN_df$state <- factor(
  statDistALAN_df$state,
  levels = stateNames)

#-------------------------------------------------------------------------------

# D) VISUALIZE
#-------------

# Recontruire axe x après scaling
HARES_5min_df.NIGHT <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/Hares_5min_night.rds")

light_mean <- mean(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)
light_sd   <- sd(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)

light_labels <- seq(
  floor(min(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)),
  ceiling(max(HARES_5min_df.NIGHT$LIGHT, na.rm = TRUE)),
  by = 200)

light_breaks <- (light_labels - light_mean) / light_sd

# Figure
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

ALAN_Final_3 <- ggplot(
  data = statDistALAN_df,
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

  facet_grid(~ state,
    labeller = labeller(
      state = c(
        "resting" = "Immobile",
        "foraging" = "Actif",
        "exploring" = "Transit"))) +

  labs(
    x = "Distance au lampadaire le plus proche (m)",
    y = "Probabilité d'état",
    color = "Etat",
    fill = "Etat",
    linetype = "ALAN",
    title = "Effet de l'intensité lumineuse (ALAN et distance au lampadaire le plus proche)\nsur le rythme d'activité du lièvre brun") +

  scale_color_manual(
    values = c("darkred", "red", "orange"),
    labels = c("immobile", "actif", "transit")) +
  
  scale_fill_manual(
    values = c("darkred", "red", "orange"),
    labels = c("immobile", "actif", "transit")) +
  
  scale_linetype_manual(
    values = c(
      "OFF" = "solid",
      "ON" = "dotted"),
    labels = c(
      "Eteint", "Allumé")) +

  scale_x_continuous(
    breaks = light_breaks,
    labels = light_labels) +

  theme_minimal() +

  theme(
    legend.position = c(0.85, 0.75))

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/ALAN_Final_3.png",
  plot = ALAN_Final_3,
  width = 8,
  height = 6,
  dpi = 300)

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/CI_distALAN.png",
  plot = CI_distALAN,
  width = 8,
  height = 6,
  dpi = 300)

grid.arrange(CI, ALAN_Final_3)

# CLEAN
rm(b, cb, est, lower, modDistALAN, newcovs, statDistALAN_df, n_ALAN)
rm(tmp_list, upper, CI, i, light_seq, rows_keep, ALAN_Final_3)
