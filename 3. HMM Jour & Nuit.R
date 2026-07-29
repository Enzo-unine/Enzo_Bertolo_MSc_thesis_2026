# 03 FIT HMM (3 STATES)
#########################

# 03.10 Load data
#----------------
STEPS_5min_df <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/STEPS_5min_3states.rds")

dist <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/dist.rds")

mod30 <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/mod30.rds")

stateNames <- c("resting", "foraging", "exploring")
nState <- 3

# 03.1 EXPLORING TEMPORAL COVARIATES
####################################

# 03.11 How do diel light–dark cycles affect the temporal activity patterns of hares ?
#-------------------------------------------------------------------------------------

# A) MODEL
#---------
# or read it...
modDiel <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modDiel.rds")

formulaDiel <- ~ diel

ParDiel <- getPar0(
  model = mod30,
  formula = formulaDiel)

modDiel <- fitHMM(
  data = STEPS_5min_df,
  nbState = nState,
  stateNames = stateNames,
  dist = dist,
  Par0 = ParDiel$Par,
  beta0 = ParDiel$beta,
  formula = formulaDiel,
  retryFits = 1)

modDiel

saveRDS(modDiel, file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modDiel.rds")

# B) TEST EFFECT OF VARIABLES
#----------------------------

cb <- CIbeta(modDiel)

# original transition names
trans <- names(cb$beta$est["dielnight", ])

# Replace numbers with state names
trans_named <- trans
for(i in seq_along(stateNames)) {
  trans_named <- gsub(paste0("\\b", i, "\\b"),
                       stateNames[i],
                       trans_named)}

b <- data.frame(
  transition = trans_named,
  estimate = as.numeric(cb$beta$est["dielnight", ]),
  lower = as.numeric(cb$beta$lower["dielnight", ]),
  upper = as.numeric(cb$beta$upper["dielnight", ]))

# Significant if CI does not overlap 0
b$significant <- b$lower > 0 | b$upper < 0

CI_diel <- ggplot(
  data = b,
  aes(
    x = transition,
    y = estimate,
    colour = significant)) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed") +
  geom_pointrange(
    aes(
      ymin = lower,
      ymax = upper)) +
  coord_flip() +
  scale_colour_manual(
    values = c("FALSE" = "grey60", "TRUE" = "red"),
    labels = c("Non significatif", "Significatif")) +
  labs(
    x = "Transitions d'états",
    y = "Estimation beta ± 95% IC",
    title = "Effet de la nuit sur les transitions d'états comparé au jour") +
  theme_classic()

# C) PREDICT STATE PROBABILITIES FOR EACH CATEGORY
#-------------------------------------------------

pdf(NULL)

statDiel <- plotStationary(
  modDiel,
  plotCI = TRUE,
  alpha = 0.95,
  return = TRUE)

dev.off()

statDiel_df <- rbind(
  cbind(statDiel$diel$resting,   state = "resting"),
  cbind(statDiel$diel$foraging,  state = "foraging"),
  cbind(statDiel$diel$exploring, state = "exploring"))

names(statDiel_df)[names(statDiel_df) == "cov"] <- "diel"

statDiel_df$diel <- factor(statDiel_df$diel, levels = c("day", "night"))
statDiel_df$state <- factor(statDiel_df$state, levels = stateNames)

# D) VISUALIZE
#-------------
n_diel <- STEPS_5min_df %>%
  group_by(diel) %>%
  summarise(n = n())

labels_custom <- n_diel %>%
  mutate(
    label = paste0(
      dplyr::recode(diel,
                    "day" = "jour",
                    "night" = "nuit"),
      "\n(n=", n, ")")
  ) %>%
  select(diel, label) %>%
  deframe()

day_night_distrib <- ggplot(
  data = statDiel_df,
  aes(x = diel, y = est, fill = state)) +

  geom_col(
    position = position_dodge(0.7),
    width = 0.6) +

  geom_errorbar(
    aes(ymin = lci, ymax = uci),
    position = position_dodge(0.7),
    width = 0.15) +

  labs(
    x = "Jour / Nuit",
    y = "Probabilité d'état",
    fill = "Etats",
    title = "Effet du jour et de la nuit sur le rythme d'activité du lièvre brun") +

  scale_fill_manual(
    values = c("darkred", "red", "orange"),
    labels = c("immobile", "actif", "transit")) +

  scale_x_discrete(
    labels = labels_custom) +

  theme_minimal() +
  theme(legend.position = c(0.85, 0.85))

ggsave("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/day_night_distrib_3.png",
       plot = day_night_distrib,
       width = 8,
       height = 6,
       dpi = 300)

ggsave("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/CI_diel.png",
       plot = CI_diel,
       width = 8,
       height = 6,
       dpi = 300)

grid.arrange(CI_diel, day_night_distrib, ncol = 2)


rm(formulaDiel, ParDiel, statDiel, statDiel_df, modDiel, day_night_distrib, n_diel)
rm(CI, i, trans, trans_named)

#-----------------------------------------------------------------------------------------------------------------------

# 03.12 How does time of day influence the activity patterns of hares?
#---------------------------------------------------------------------

# A) MODEL
#---------
# or read it...
modTOD <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modTOD.rds")

formulaTOD <- ~ tod_class

ParTOD <- getPar0(
  model = mod30,
  formula = formulaTOD)

modTOD <- fitHMM(
  data = STEPS_5min_df,
  nbState = nState,
  stateNames = stateNames,
  dist = dist,
  Par0 = ParTOD$Par,
  beta0 = ParTOD$beta,
  formula = formulaTOD,
  retryFits = 1)

modTOD

saveRDS(
  modTOD,
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modTOD.rds")

# B) TEST EFFECT OF VARIABLES
#----------------------------

cb <- CIbeta(modTOD)

# Extract beta table
est   <- cb$beta$est
lower <- cb$beta$lower
upper <- cb$beta$upper

# Keep only TOD covariate rows
tod_rows <- grepl("^tod_class", rownames(est))

b <- data.frame(
  tod_class = rownames(est)[tod_rows],
  transition = rep(colnames(est), each = sum(tod_rows)),
  estimate = as.vector(est[tod_rows, ]),
  lower = as.vector(lower[tod_rows, ]),
  upper = as.vector(upper[tod_rows, ]))

# Clean TOD labels
b$tod_class <- sub("^tod_class", "", b$tod_class)

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

CI_tod <- ggplot(b, aes(x = tod_class, y = estimate)) +
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
    x = "Période de la journée",
    y = "Estimation Beta ± 95% IC",
    colour = "",
    shape = "",
    title = "Effet des périodes de la journée sur les transitions d'états") +
  theme_classic()

# C) PREDICT STATE PROBABILITIES FOR EACH CATEGORY
#----------------------------------------------------
pdf(NULL)

statTOD <- plotStationary(
  modTOD,
  plotCI = TRUE,
  alpha = 0.95,
  return = TRUE)

dev.off()

statTOD_df <- rbind(
  transform(statTOD$tod_class$resting,   state = "resting"),
  transform(statTOD$tod_class$foraging,  state = "foraging"),
  transform(statTOD$tod_class$exploring, state = "exploring"))

names(statTOD_df)[names(statTOD_df) == "cov"] <- "tod_class"

statTOD_df$tod_class <- factor(
  statTOD_df$tod_class,
  levels = c(
    "SOIR : sunset - 23H00", 
    "NUIT : 23H00 - 03H00",
    "NUIT : 03H00 - 06H00", 
    "LEVER DU JOUR : 06H00 - sunrise", 
    "MATIN : sunrise - 11H00",
    "MIDI : 11H00 - 14H00", 
    "APRES-MIDI : 14H00 - 16H00", 
    "COUCHER DU JOUR : 16H00 - sunset"))

statTOD_df$state <- factor(
  statTOD_df$state,
  levels = c("resting", "foraging", "exploring"))

# D) VISUALIZE
#-------------
n_tod <- STEPS_5min_df %>%
  group_by(tod_class) %>%
  summarise(n = n())

labels_custom <- n_tod %>%
  mutate(label = paste0(
    tod_class,
    "\n(n=", n, ")"
  )) %>%
  select(tod_class, label) %>%
  deframe()

tod_distrib <- ggplot(
  statTOD_df,
  aes(x = tod_class, y = est, fill = state)) +
  
  geom_col(
    position = position_dodge(0.7),
    width = 0.6) +
  
  geom_errorbar(
    aes(ymin = lci, ymax = uci),
    position = position_dodge(0.7),
    width = 0.15) +
  
  scale_fill_manual(
    values = c("darkred", "red", "orange"),
    labels = c("immobile", "actif", "transit")) +
      
  labs(
    x = "Période de la journée",
    y = "Probabilité d'état",
    fill = "Etat",
    title = "Effet des périodes de la journée sur le rythme d'activité du lièvre brun") +

  scale_x_discrete(
    labels = labels_custom) +
    
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.10, 0.85))

ggsave(
 "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/TOD_distrib.png",
 plot = tod_distrib,
 width = 8,
 height = 6,
 dpi = 300)

ggsave(
 "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/CI_tod.png",
 plot = CI_tod,
 width = 8,
 height = 6,
 dpi = 300)

grid.arrange(CI, tod_distrib, ncol = 2)

rm(b, cb, est, lower, modTOD, parTOD, statTOD, statTOD_df, n_tod)
rm(CI, formulaTOD, i, tod_distrib, tod_rows, ParTOD, tod_rows)

#########################################################################################################
