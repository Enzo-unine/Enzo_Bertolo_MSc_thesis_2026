# 10.1 Lièvres data
# -----------------
# Dataframe
# Dataframe
STEPS_5min_df <- read.csv(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/STEPS_5min_lm.csv",
  stringsAsFactors = FALSE
)

# Convert date/time columns
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

STEPS_5min_night_df <- subset(
  STEPS_5min_df,
  diel == "night"
)

# -------------------------------------------------
# Garder uniquement la période des lamp. mobiles
# -------------------------------------------------
Lamp_mob_night_df <- STEPS_5min_night_df %>%
  filter(
    timestamp_5 >= as.POSIXct("2026-03-28 12:00:00",
                              tz = "Europe/Zurich") &
    timestamp_5 <= as.POSIXct("2026-05-11 12:00:00",
                              tz = "Europe/Zurich"))
nrow(Lamp_mob_night_df)
# ==========================================================
# Définition des 5 périodes expérimentales
# ==========================================================

Lamp_mob_night_df$Period <- NA

# -----------------------
# OFF1
# -----------------------

Lamp_mob_night_df$Period[
  Lamp_mob_night_df$ID == "G" &
    Lamp_mob_night_df$timestamp_5 >= as.POSIXct("2026-03-28 12:00:00", tz="Europe/Zurich") &
    Lamp_mob_night_df$timestamp_5 <  as.POSIXct("2026-04-04 12:00:00", tz="Europe/Zurich")
] <- "OFF1"

Lamp_mob_night_df$Period[
  Lamp_mob_night_df$ID %in% c("B","S") &
    Lamp_mob_night_df$timestamp_5 >= as.POSIXct("2026-03-28 12:00:00", tz="Europe/Zurich") &
    Lamp_mob_night_df$timestamp_5 <  as.POSIXct("2026-04-05 12:00:00", tz="Europe/Zurich")
] <- "OFF1"

# -----------------------
# ON1
# -----------------------

Lamp_mob_night_df$Period[
  Lamp_mob_night_df$ID=="G" &
    Lamp_mob_night_df$timestamp_5 >= as.POSIXct("2026-04-04 12:00:00", tz="Europe/Zurich") &
    Lamp_mob_night_df$timestamp_5 <= as.POSIXct("2026-04-12 12:00:00", tz="Europe/Zurich")
] <- "ON1"

Lamp_mob_night_df$Period[
  Lamp_mob_night_df$ID %in% c("B","S") &
    Lamp_mob_night_df$timestamp_5 >= as.POSIXct("2026-04-05 12:00:00", tz="Europe/Zurich") &
    Lamp_mob_night_df$timestamp_5 <= as.POSIXct("2026-04-12 12:00:00", tz="Europe/Zurich")
] <- "ON1"

# -----------------------
# OFF2
# -----------------------

Lamp_mob_night_df$Period[
  Lamp_mob_night_df$ID=="G" &
    Lamp_mob_night_df$timestamp_5 > as.POSIXct("2026-04-12 12:00:00", tz="Europe/Zurich") &
    Lamp_mob_night_df$timestamp_5 < as.POSIXct("2026-04-20 12:00:00", tz="Europe/Zurich")
] <- "OFF2"

Lamp_mob_night_df$Period[
  Lamp_mob_night_df$ID %in% c("B","S") &
    Lamp_mob_night_df$timestamp_5 > as.POSIXct("2026-04-12 12:00:00", tz="Europe/Zurich") &
    Lamp_mob_night_df$timestamp_5 < as.POSIXct("2026-04-27 12:00:00", tz="Europe/Zurich")
] <- "OFF2"

# -----------------------
# ON2
# -----------------------

Lamp_mob_night_df$Period[
  Lamp_mob_night_df$ID=="G" &
    Lamp_mob_night_df$timestamp_5 >= as.POSIXct("2026-04-20 12:00:00", tz="Europe/Zurich") &
    Lamp_mob_night_df$timestamp_5 <= as.POSIXct("2026-04-27 12:00:00", tz="Europe/Zurich")
] <- "ON2"

Lamp_mob_night_df$Period[
  Lamp_mob_night_df$ID %in% c("B","S") &
    Lamp_mob_night_df$timestamp_5 >= as.POSIXct("2026-04-27 12:00:00", tz="Europe/Zurich") &
    Lamp_mob_night_df$timestamp_5 <= as.POSIXct("2026-05-02 12:00:00", tz="Europe/Zurich")
] <- "ON2"

# -----------------------
# OFF3
# -----------------------

Lamp_mob_night_df$Period[
  Lamp_mob_night_df$ID %in% c("B","S") &
    Lamp_mob_night_df$timestamp_5 > as.POSIXct("2026-05-02 12:00:00", tz="Europe/Zurich") &
    Lamp_mob_night_df$timestamp_5 <= as.POSIXct("2026-05-11 12:00:00", tz="Europe/Zurich")
] <- "OFF3"

Lamp_mob_night_df$Period <- factor(
  Lamp_mob_night_df$Period,
  levels=c("OFF1","ON1","OFF2","ON2","OFF3"))

head(Lamp_mob_night_df, 5)

# Convert sf points to terra vector
hares_df <- data.frame(
  x = Lamp_mob_night_df$x,
  y = Lamp_mob_night_df$y)

# 10.2 Lampadaires data (lampadaires mobiles seulement)
# -----------------------------------------------------
# Runner "3. Data" jusqu'à crop(LIGHT)

# Raster template vide
r_template <- rast(LIGHT)

hares_vect <- vect(
  hares_df,
  geom = c("x", "y"),
  crs = crs(r_template))

values(r_template) <- NA

# Coordonnées des lampadaires mobiles
lamps_mob <- data.frame(
  x = c(2557001, 2556866, 2557269),
  y = c(1206689,1207006, 1209530))

lamps_vect <- vect(lamps_mob,
                   geom = c("x", "y"),
                   crs = crs(r_template))

# Rasteriser uniquement ces points
lamps_rast <- rasterize(lamps_vect,
                        r_template,
                        field = 1,
                        background = NA)

# Raster de distance
dist_lamps_mob <- distance(lamps_rast)

# Visualisation
out_file <- "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/lamp_mob_map.png"

LIGHT_mob.map <- ggplot() +
  
  # DISTANCE TO LIGHT
  geom_spatraster( 
    data = dist_lamps_mob, 
    alpha = 1) + 

  labs(
    x = "Coordonnées GPS X",
    y = "Coordonnées GPS Y",
    title = "Distance au lampadaires mobiles le \nplus proche sur les deux sites d'étude") +
  
  # LAMPS 
  geom_sf(
    data = st_as_sf(lamps_vect),
    size = 1.5,
    aes(shape = "Lampadaires"),
    colour = "black") +

scale_fill_gradientn(
    colours = colorRampPalette(c("gold", "darkblue"))(20),
    name = "Distance (m)",
    limits = c(0, 2500),
    breaks = seq(0, 2500, 500),
    oob = scales::squish) +

  scale_shape_manual(
    name = "Légende",
    values = c("Lampadaires" = 16)) +

  theme_minimal() +
  theme(
    legend.position = c(0.95, 0.25),
    legend.background = element_rect(fill = "white", colour = "grey70"),
    legend.key = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9)
  )

LIGHT_mob.map

ggsave("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/Lampes_Mob_Distances.png",
       plot = LIGHT_mob.map,
       width = 8,
       height = 6,
       dpi = 300)

# Add distance "hare - mobile lamp" to df
vals <- terra::extract(dist_lamps_mob, hares_vect)

Lamp_mob_night_df$dist_lamps_mob <- vals[, 2]

# Control and save
head(Lamp_mob_night_df, 5)

saveRDS(Lamp_mob_night_df, file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/Lamp_mob_night_df.rds")

# 10.3 Visualize
################ 
# =========================
# 10.30 Chargement / préparation du plot de base
# =========================
# 10.31. On crée un boxplot de base avec LIGHT en fonction de ALAN
Lamp_mob_night_df <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/Lamp_mob_night_df.rds")

boxplot_dist_lm <- ggplot(Lamp_mob_night_df, aes(x = ALAN_mob, y = dist_lamps_mob)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "ALAN", y = "Distance")

# =========================
# 10.32. Modèle ANOVA global
# =========================
# Test si la lumière diffère entre les 3 catégories ALAN

anova_model <- aov(dist_lamps_mob ~ Period,
                   data = Lamp_mob_night_df)

summary(anova_model)

p_value <- summary(anova_model)[[1]]["Period","Pr(>F)"]

# =========================
# 10.33. Test post-hoc (Tukey)
# =========================
# Comparaisons entre toutes les paires de groupes (ON_Evening, OFF, ON_Morning)

tukey <- TukeyHSD(anova_model)

tukey

# =========================
# 10.34. Moyennes par groupe
# =========================
# Calcul des moyennes de LIGHT pour chaque catégorie ALAN
means <- Lamp_mob_night_df %>%
  group_by(Period) %>%
  summarise(mean_dist = mean(dist_lamps_mob, na.rm=TRUE))

# =========================
# 10.35. Différences de distance (effets)
# =========================
# Calcul des différences entre groupes
diff_OFF1_ON1 <-
  means$mean_dist[means$Period=="ON1"]-
  means$mean_dist[means$Period=="OFF1"]

diff_ON1_OFF2 <-
  means$mean_dist[means$Period=="OFF2"]-
  means$mean_dist[means$Period=="ON1"]

diff_OFF2_ON2 <-
  means$mean_dist[means$Period=="ON2"]-
  means$mean_dist[means$Period=="OFF2"]

diff_ON2_OFF3 <-
  means$mean_dist[means$Period=="OFF3"]-
  means$mean_dist[means$Period=="ON2"]

# =========================
# 10.36. Texte ANOVA formaté
# =========================
# Création du label ANOVA affiché sur le plot

anova_label <- paste0(
  "ANOVA p = ",
  format.pval(p_value,digits=3))

diff_label <- paste(
  "Différences entre les moyennes :",
  paste0("OFF1 → ON1 = ", round(diff_OFF1_ON1,1), " m"),
  paste0("ON1 → OFF2 = ", round(diff_ON1_OFF2,1), " m"),
  paste0("OFF2 → ON2 = ", round(diff_OFF2_ON2,1), " m"),
  paste0("ON2 → OFF3 = ", round(diff_ON2_OFF3,1), " m"),
  sep = "\n")

# =========================
# 10.37. Position des annotations sur le graphique
# =========================
# On place les textes automatiquement au-dessus des boxplots

x_pos <- 2 

y_max <- max(Lamp_mob_night_df$dist_lamps_mob, na.rm = TRUE)

y_anova <- y_max * 0.95
y_diff  <- y_max * 1.05

# =========================
# 10.38. Figure finale
# =========================
# Boxplot + couleurs + stats + annotations

y_max <- max(Lamp_mob_night_df$dist_lamps_mob,na.rm=TRUE)

boxplot_dist_lm <- ggplot(
  
  Lamp_mob_night_df, aes(Period,
           dist_lamps_mob,
           fill=Period))+

  geom_boxplot()+

  scale_fill_manual(values=c(
    OFF1="royalblue3",
    ON1="orange",
    OFF2="royalblue3",
    ON2="orange",
    OFF3="royalblue3"))+

  labs(
    title="Distance au lampadaire mobile le plus proche",
    x="Période expérimentale",
    y="Distance (m)")+
  
  theme_minimal()+
  theme(
    legend.position="none")+

  annotate("text",
    x=3,
    y=y_max*1.20,
    label=anova_label,
    size=5)+
  
  annotate("text",
    x = 3,
    y = max(Lamp_mob_night_df$dist_lamps_mob) * 1.15,
    label = "Δ = différence entre les moyennes",
    fontface = "italic",
    size = 4)+

  annotate("text",
         x = 1.5,
         y = y_max*1.08,
         label = paste0("Δ = ", round(diff_OFF1_ON1,1), " m"))+

  annotate("text",
         x = 2.5,
         y = y_max*1.08,
         label = paste0("Δ = ", round(diff_ON1_OFF2,1), " m"))+

  annotate("text",
         x = 3.5,
         y = y_max*1.08,
         label = paste0("Δ = ", round(diff_OFF2_ON2,1), " m"))+

  annotate("text",
         x = 4.5,
         y = y_max*1.08,
         label = paste0("Δ = ", round(diff_ON2_OFF3,1), " m"))

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/boxplot_dist_lm.png",
  plot = boxplot_dist_lm,
  width = 10,
  height = 7,
  dpi = 300)

###############################################################################################

###########################################################
# 10.40 Boxplots individuels
###########################################################

Lamp_mob_night_df <- readRDS(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/Lamp_mob_night_df.rds")

###########################################################
# Liste des individus
###########################################################

IDs <- unique(Lamp_mob_night_df$ID)

###########################################################
# Boucle individus
###########################################################

for(id_i in IDs){

  animal_name <- case_when(
  id_i == "B" ~ "Boguet",
  id_i == "S" ~ "Shy",
  id_i == "G" ~ "Giorgia")

  cat("\n========================\n")
  cat("Individu :", id_i, "\n")
  cat("========================\n")

  #########################################################
  # Sous-ensemble individu
  #########################################################

  df_ind <- Lamp_mob_night_df %>%
    filter(ID == id_i)

  #########################################################
  # Supprimer périodes vides
  #########################################################

  df_ind <- df_ind %>%
    filter(!is.na(Period))

  #########################################################
  # Facteur période
  #########################################################

  if(id_i == "G"){

    period_levels <- c(
      "OFF1",
      "ON1",
      "OFF2",
      "ON2")

  } else {

    period_levels <- c(
      "OFF1",
      "ON1",
      "OFF2",
      "ON2",
      "OFF3")
  }

  df_ind$Period <- factor(
    df_ind$Period,
    levels = period_levels)

  #########################################################
  # ANOVA
  #########################################################

  anova_model <- aov(
    dist_lamps_mob ~ Period,
    data = df_ind)

  p_value <- summary(anova_model)[[1]][
    "Period",
    "Pr(>F)"]

  anova_label <- paste0(
    "ANOVA p = ",
    format.pval(p_value, digits = 3))

  #########################################################
  # Moyennes
  #########################################################

  means <- df_ind %>%
    group_by(Period) %>%
    summarise(
      mean_dist = mean(
        dist_lamps_mob,
        na.rm = TRUE),
      .groups = "drop")

  #########################################################
  # Différences successives
  #########################################################

  delta_values <- diff(means$mean_dist)

  delta_labels <- paste0(
    "Δ = ",
    round(delta_values,1),
    " m")

  #########################################################
  # Positionnement
  #########################################################

  y_max <- max(
    df_ind$dist_lamps_mob,
    na.rm = TRUE)

  #########################################################
  # Figure
  #########################################################

  p <- ggplot(
    df_ind,
    aes(
      x = Period,
      y = dist_lamps_mob,
      fill = Period)) +

    geom_boxplot() +

    scale_fill_manual(
      values = c(
        OFF1 = "royalblue3",
        ON1  = "orange",
        OFF2 = "royalblue3",
        ON2  = "orange",
        OFF3 = "royalblue3")) +

    labs(
      title = paste(
        "Distance au lampadaire mobile le plus proche\n",
        animal_name),
      x = "Période expérimentale",
      y = "Distance (m)") +

    theme_minimal() +

    theme(
      legend.position = "none")

  #########################################################
  # ANOVA
  #########################################################

  p <- p +
    annotate(
      "text",
      x = (length(period_levels)+1)/2,
      y = y_max*1.20,
      label = anova_label,
      size = 4)

  #########################################################
  # Texte explicatif
  #########################################################

  p <- p +
    annotate(
      "text",
      x = (length(period_levels)+1)/2,
      y = y_max*1.14,
      label = "Δ = différence entre les moyennes",
      fontface = "italic",
      size = 3)

  #########################################################
  # Ajouter chaque Δ
  #########################################################

  xpos <- seq(
    1.5,
    length(period_levels)-0.5,
    by = 1)

  for(i in seq_along(delta_labels)){

    p <- p +
      annotate(
        "text",
        x = xpos[i],
        y = y_max*1.08,
        label = delta_labels[i])
  }

  #########################################################
  # t-test
  #########################################################
  comparisons <- list(
    c("OFF1", "ON1"),
    c("ON1", "OFF2"),
    c("OFF2", "ON2")  )

  if("OFF3" %in% period_levels){
    comparisons <- c(comparisons, list(c("ON2", "OFF3")))
  }

  p <- p +
    stat_compare_means(
      comparisons = comparisons,
      method = "t.test",
      label = "p.signif")

  #########################################################
  # Affichage
  #########################################################

  print(p)

  #########################################################
  # Sauvegarde
  #########################################################

  ggsave(
  filename = paste0(
    "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/Dist_lamp_mob_",
    animal_name,
    ".png"),
  plot = p,
  width = 8,
  height = 6,
  dpi = 300)

}


while (!is.null(dev.list())) dev.off()



