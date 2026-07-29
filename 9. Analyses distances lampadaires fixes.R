# 9.1 Calcul et comparaison distances au lampadaire le plus proche
##############################################################
# =========================
# 9.10 Chargement / préparation du plot de base
# =========================
# 9.11. On crée un boxplot de base avec LIGHT en fonction de ALAN
# =========================
# Load data from CSV
STEPS_5min_night_df <- read.csv(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/STEPS_5min_dist_lamp.csv",
  stringsAsFactors = FALSE)

# Convert date/time columns
STEPS_5min_night_df <- STEPS_5min_night_df %>%
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

head(STEPS_5min_night_df, 5)

# Les 5 premières et les 5 dernières lignes de chaque ID et de chaque condition ALAN
resultat <- STEPS_5min_night_df %>%
  group_by(ID, ALAN) %>%
  group_modify(~ bind_rows(
    slice_head(.x, n = 5),
    slice_tail(.x, n = 5)
  ) %>%
    distinct()) %>%
  ungroup()

View(resultat)

STEPS_5min_night_df <- subset(
  STEPS_5min_night_df,
  diel == "night"
)

boxplot_dist <- ggplot(STEPS_5min_night_df, aes(x = ALAN, y = LIGHT)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "ALAN", y = "Distance")

# =========================
# 9.12. Modèle ANOVA global
# =========================
# Test si la lumière diffère entre les 3 catégories ALAN

anova_model <- aov(LIGHT ~ ALAN, data = STEPS_5min_night_df)
summary(anova_model)

# Extraction de la p-value globale de l’ANOVA
p_value <- summary(anova_model)[[1]]["ALAN", "Pr(>F)"]

# =========================
# 9.13. Test post-hoc (Tukey)
# =========================
# Comparaisons entre toutes les paires de groupes (ON_Evening, OFF, ON_Morning)

tukey <- TukeyHSD(anova_model)
tukey

# =========================
# 9.14. Moyennes par groupe
# =========================
# Calcul des moyennes de LIGHT pour chaque catégorie ALAN
means <- STEPS_5min_night_df %>%
  group_by(ALAN) %>%
  summarise(mean_LIGHT = mean(LIGHT, na.rm = TRUE))

# =========================
# 9.15. Différences de distance (effets)
# =========================
# Calcul des différences entre groupes
diff_ON_Evening_OFF <- means$mean_LIGHT[means$ALAN == "OFF"] -
                       means$mean_LIGHT[means$ALAN == "ON_Evening"]

diff_OFF_ON_Morning <- means$mean_LIGHT[means$ALAN == "ON_Morning"] -
                       means$mean_LIGHT[means$ALAN == "OFF"]

diff_label <- paste0("Δ Soir-Allumé vs Eteint = ", round(diff_ON_Evening_OFF, 2),
                     "\nΔ Eteint vs Allumé-Matin = ", round(diff_OFF_ON_Morning, 2))

# =========================
# 9.16. Préparation du facteur ALAN (ordre des boîtes)
# =========================
# On force l’ordre d’affichage des boxplots

STEPS_5min_night_df$ALAN <- factor(
  STEPS_5min_night_df$ALAN,
  levels = c("ON_Evening", "OFF", "ON_Morning"))

# =========================
# 14.17. Texte ANOVA formaté
# =========================
# Création du label ANOVA affiché sur le plot

anova_label <- paste0("ANOVA p = ", format.pval(p_value, digits = 3))

# =========================
# 9.18. Position des annotations sur le graphique
# =========================
# On place les textes automatiquement au-dessus des boxplots

x_pos <- 2  # centre entre 3 groupes

y_max <- max(STEPS_5min_night_df$LIGHT, na.rm = TRUE)

y_anova <- y_max * 0.95
y_diff  <- y_max * 1.05

# =========================
# 9.19. Figure finale
# =========================
# Boxplot + couleurs + stats + annotations

boxplot_dist <- ggplot(STEPS_5min_night_df,
                       aes(x = ALAN, y = LIGHT, fill = ALAN)) +
  
  geom_boxplot() +
  
  scale_fill_manual(
  values = c(
    "ON_Evening" = "orange",
    "OFF" = "blue",
    "ON_Morning" = "red"),
  labels = c(
    "ON_Evening" = "Allumé soir",
    "OFF" = "Éteint",
    "ON_Morning" = "Allumé matin")) +

scale_x_discrete(
  labels = c(
    "ON_Evening" = "Allumé soir",
    "OFF" = "Éteint",
    "ON_Morning" = "Allumé matin")) +
  
  labs(
    title = "Distance lièvre - lampadaire le plus proche",
    x = "ALAN",
    y = "distance") +
  
  theme_minimal() +
  theme(legend.position = "none") +

  annotate("text",
    x = x_pos,
    y = y_diff * 1.10,
    label = "Δ = différence entre les moyennes",
    fontface = "italic",
    size = 4)+

  
  # Différences entre groupes
  annotate("text",
           x = x_pos,
           y = y_diff,
           label = diff_label,
           size = 4) +
  
  # ANOVA globale
  annotate("text",
           x = x_pos,
           y = y_anova,
           label = anova_label,
           size = 3) +
  
  # Comparaisons statistiques entre groupes
  stat_compare_means(
    comparisons = list(
      c("ON_Evening", "OFF"),
      c("OFF", "ON_Morning")
    ),
    method = "t.test",
    label = "p.signif")

ggsave(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/boxplot_dist.png",
  plot = boxplot_dist,
  width = 10,
  height = 7,
  dpi = 300)

################################################################################################

###########################################################
# 9.20 Boxplots individuels
###########################################################

IDs <- unique(STEPS_5min_night_df$ID)

for(id_i in IDs){

  print(id_i)

  df_ind <- STEPS_5min_night_df %>%
  filter(ID == id_i)

  print(nrow(df_ind))
  #########################################################
  # Nom individu
  #########################################################

  animal_name <- case_when(
    id_i == "B" ~ "Boguet",
    id_i == "S" ~ "Shy",
    id_i == "G" ~ "Giorgia"
  )

  cat("\n========================\n")
  cat("Individu :", animal_name, "\n")
  cat("========================\n")

  #########################################################
  # Sous-ensemble
  #########################################################

  df_ind <- STEPS_5min_night_df %>%
    filter(ID == id_i)

  #########################################################
  # Ordre catégories
  #########################################################

  df_ind$ALAN <- factor(
    df_ind$ALAN,
    levels = c(
      "ON_Evening",
      "OFF",
      "ON_Morning")
  )

  #########################################################
  # ANOVA
  #########################################################

  anova_model <- aov(
    LIGHT ~ ALAN,
    data = df_ind)

  p_value <- summary(anova_model)[[1]][
    "ALAN",
    "Pr(>F)"
  ]

  anova_label <- paste0(
    "ANOVA p = ",
    format.pval(p_value, digits = 3)
  )

  #########################################################
  # Moyennes
  #########################################################

  means <- df_ind %>%
    group_by(ALAN) %>%
    summarise(
      mean_LIGHT = mean(
        LIGHT,
        na.rm = TRUE),
      .groups = "drop")

  #########################################################
  # Différences
  #########################################################

  diff_ON_Evening_OFF <-
    means$mean_LIGHT[means$ALAN=="OFF"] -
    means$mean_LIGHT[means$ALAN=="ON_Evening"]

  diff_OFF_ON_Morning <-
    means$mean_LIGHT[means$ALAN=="ON_Morning"] -
    means$mean_LIGHT[means$ALAN=="OFF"]

  #########################################################
  # Labels
  #########################################################

  diff_label <- paste0(
    "Δ Soir-Allumé vs Éteint = ",
    round(diff_ON_Evening_OFF,1),
    " m",
    "\nΔ Éteint vs Matin-Allumé = ",
    round(diff_OFF_ON_Morning,1),
    " m"
  )

  #########################################################
  # Position
  #########################################################

  y_max <- max(
    df_ind$LIGHT,
    na.rm = TRUE)

  #########################################################
  # Figure
  #########################################################

  p <- ggplot(
    df_ind,
    aes(
      x = ALAN,
      y = LIGHT,
      fill = ALAN)
  ) +

    geom_boxplot() +

    scale_fill_manual(
      values = c(
        "ON_Evening" = "orange",
        "OFF" = "blue",
        "ON_Morning" = "red"
      ),
      labels = c(
        "ON_Evening" = "Allumé soir",
        "OFF" = "Éteint",
        "ON_Morning" = "Allumé matin"
      )
    ) +

    scale_x_discrete(
      labels = c(
        "ON_Evening" = "Allumé soir",
        "OFF" = "Éteint",
        "ON_Morning" = "Allumé matin"
      )
    ) +

    labs(
      title = paste(
        "Distance au lampadaire fixe le plus proche\n",
        animal_name),
      x = "ALAN",
      y = "Distance (m)"
    ) +

    theme_minimal() +

    theme(
      legend.position = "none"
    ) +

    annotate(
      "text",
      x = 2,
      y = y_max * 1.12,
      label = "Δ = différence entre les moyennes",
      fontface = "italic",
      size = 4
    ) +

    annotate(
      "text",
      x = 2,
      y = y_max * 1.05,
      label = diff_label,
      size = 4.5
    ) +

    annotate(
      "text",
      x = 2,
      y = y_max * 0.97,
      label = anova_label,
      size = 3.8
    ) +

    stat_compare_means(
      comparisons = list(
        c("ON_Evening","OFF"),
        c("OFF","ON_Morning")
      ),
      method = "t.test",
      label = "p.signif"
    )

  #########################################################
  # Affichage
  #########################################################

  print(p)

  #########################################################
  # Sauvegarde
  #########################################################

  ggsave(
    filename = paste0(
      "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/",
      animal_name,
      "_DistLamp.png"
    ),
    plot = p,
    width = 10,
    height = 7,
    dpi = 300
  )
}

while (!is.null(dev.list())) dev.off()

