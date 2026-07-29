# Test de colinéarité des variables numériques
# --------------------------------------------

# Pearson 
vars_corr <- STEPS_5min_df.NIGHT %>%
  select(LIGHT, moon_fraction, temp, vent, precip)

cor(vars_corr, use = "complete.obs", method = "pearson")

# Visualize
png("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/col_num.png",
    width = 2000,
    height = 1500,
    res = 300)

corrplot(cor(vars_corr, use = "complete.obs"),
    method = "color",
    type = "upper",
    addCoef.col = "black")

title("Matrice des associations (Pearson) des variables numériques", cex.main = 1.5)

mtext(
  "Pearson :\n < 0.3 weak,\n 0.3–0.5 moderate,\n > 0.5 strong association",
  side = 1,
  line = 0.4,
  adj = 0.3,
  cex = 0.9)

dev.off()

# Test de colinéarité des variables factorielles
# ----------------------------------------------

# sélectionner uniquement les facteurs
vars_fact <- STEPS_5min_df.NIGHT %>%
  select(where(is.factor))

vars_fact <- vars_fact %>%
  select(-diel)

# fonction pour calculer Cramér's V
cramer_matrix <- function(df) {
  n <- ncol(df)
  mat <- matrix(NA, n, n)
  colnames(mat) <- colnames(df)
  rownames(mat) <- colnames(df)
  
  for (i in 1:n) {
    for (j in 1:n) {
      mat[i, j] <- cramerV(df[[i]], df[[j]])
    }
  }
  return(mat)
}

# calcul
cramer_mat <- cramer_matrix(vars_fact)

cramer_mat

# Visualize
png("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/col_fact.png",
    width = 2000,
    height = 1500,
    res = 300)

corrplot(
  cramer_mat,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.cex = 0.8)

title("Matrice des associations (Cramér's V) des variables factorielles", cex.main = 1.5)

mtext(
  "Cramér's V :\n < 0.3 weak,\n 0.3–0.5 moderate,\n > 0.5 strong association",
  side = 1,
  line = 0.4,
  adj = 0.3,
  cex = 0.9)

dev.off()

# Test de colinéarité de toutes les variables
# La colonne step (déplacements) est retenue comme variable réponse

# VIF
mod_vif <- lm(
  step ~ ALAN + LIGHT + temp + vent + precip +
    moon_fraction + SNOW + LAND + veg_height + hour + week_ID,
  data = STEPS_5min_df.NIGHT)

v <- vif(mod_vif)

vif(mod_vif)

vif_df <- as.data.frame(v)

vif_df$variable <- rownames(vif_df)

# Visualize
vif_df$variable <- dplyr::recode(
  vif_df$variable,
  week_ID = "Semaines de l'année",
  LAND = "Utilisation de surface",
  veg_height = "Hauteur de végétation",
  vent = "Vent",
  temp = "Température",
  precip = "Précipitations",
  SNOW = "Couverture neigeuse",
  moon_fraction = "Lune",
  LIGHT = "Distance lampadaire",
  ALAN = "Allumage / Extinction",
  hour = "Heure")

col_plot_fin <- ggplot(vif_df, aes(
  x = reorder(variable, `GVIF^(1/(2*Df))`),
  y = `GVIF^(1/(2*Df))`)) +
  
  geom_col() +
  
  coord_flip() +
  
  geom_hline(yintercept = 3, linetype = "dashed", color = "darkgreen") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "red") +
  
  labs(
    title = "Diagnostic de colinéarité (GVIF)",
    x = "",
    y = "GVIF") +
  
  annotate(
  "label",
    x = 1,                            
    y = max(vif_df$`GVIF^(1/(2*Df))`) * 0.98,
    hjust = 1,
    vjust = 0,
    label = paste0(
        "GVIF :\n",
        "Min = ", round(min(vif_df$`GVIF^(1/(2*Df))`), 2), "\n",
        "Médiane = ", round(median(vif_df$`GVIF^(1/(2*Df))`), 2), "\n",
        "Max = ", round(max(vif_df$`GVIF^(1/(2*Df))`), 2), "\n",
        "Tous les prédicteurs < 5\n→ aucun problème de colinéarité détecté"))

ggsave(
 "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/col_finale.png",
 plot = col_plot_fin,
 width = 10,
 height = 6,
 dpi = 300)
