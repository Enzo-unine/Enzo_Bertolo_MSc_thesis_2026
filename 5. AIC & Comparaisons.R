# Comparaison des AIC pour savoir quelles variables ont un réel impact sur l'activité
#####################################################################################

# Modèle null pour les données nocturnes 
#... or read it
mod30_night <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/mod30_night.rds")

# Résoudre problème de NA suite aux modèles univariés
# => 30 lignes de NA pour toutes les variables sauf végétation
# => Pour mod30 : reprendre les données sans NA des modèles univariés / Utiliser STEPS_5min_df.NIGHT pour AIC de végétation

modHabitat <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modHabitat.rds")

data_common <- modWEEK$data 

mod30_night <- fitHMM(
  data = data_common, # fonctionne pour tous les AIC sauf végétation => STEPS_5min_df.NIGHT
  nbState = nState,
  stateNames = stateNames,
  dist = dist,
  Par0 = Par30)  

# Voir si les AIC sont comparables 
nrow(mod30_night$data)
nrow(modWEEK$data) # Changer selon le mod à vérifier

# Look at parameter estimates
mod30_night

# Save noctural null model
saveRDS(
  mod30_night, 
  file = "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/mod30_night.rds")

#--------------------------------------------------------------------------------------------------------------------------

# Test de la variable Week
# ------------------------
modWEEK <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modWEEK.rds")

# AICs
aic_tab_week <- AIC(mod30_night, modWEEK)

# Comparaison
delta_week <- with(aic_tab_week, AIC[Model == "mod30_night"] - AIC[Model == "modWEEK"])

# Visualisation (delta > 2 ?)
delta_week

#######################################################################

# Test de la variable Habitat
# ---------------------------
modHabitat <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modHabitat.rds")

# AICs
aic_tab_Habitat <- AIC(mod30_night, modHabitat)

# Comparaison
delta_Habitat <- with(aic_tab_Habitat, AIC[Model == "mod30_night"] - AIC[Model == "modHabitat"])

# Visualisation (delta > 2 ?)
delta_Habitat

#---------------------------------------------------------------------

# Test de la variable Hauteur de végétation
# -----------------------------------------
modVeg <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modHeight_Veg.rds")

# AICs
aic_tab_Veg <- AIC(mod30_night, modVeg)

# Comparaison
delta_Veg <- with(aic_tab_Veg, AIC[Model == "mod30_night"] - AIC[Model == "modVeg"])

# Visualisation (delta > 2 ?)
delta_Veg

#########################################################################

# Test de la variable Lune
# ------------------------
modMoon <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modMoon.rds")

# AICs
aic_tab_Moon <- AIC(mod30_night, modMoon)

# Comparaison
delta_Moon <- with(aic_tab_Moon, AIC[Model == "mod30_night"] - AIC[Model == "modMoon"])

# Visualisation (delta > 2 ?)
delta_Moon

#----------------------------------------------------------------

# Test de la variable Neige
# -------------------------
modSnow <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modSnow.rds")

# AICs
aic_tab_Snow <- AIC(mod30_night, modSnow)

# Comparaison
delta_Snow <- with(aic_tab_Snow, AIC[Model == "mod30_night"] - AIC[Model == "modSnow"])

# Visualisation (delta > 2 ?)
delta_Snow

#----------------------------------------------------------------

# Test de la variable Précipitations
# ----------------------------------
modPrecip <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modPrecip.rds")

# AICs
aic_tab_Precip <- AIC(mod30_night, modPrecip)

# Comparaison
delta_Precip <- with(aic_tab_Precip, AIC[Model == "mod30_night"] - AIC[Model == "modPrecip"])

# Visualisation (delta > 2 ?)
delta_Precip

#----------------------------------------------------------------

# Test de la variable Vent
# ------------------------
modVent <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modVent.rds")

# AICs
aic_tab_Vent <- AIC(mod30_night, modVent)

# Comparaison
delta_Vent <- with(aic_tab_Vent, AIC[Model == "mod30_night"] - AIC[Model == "modVent"])

# Visualisation (delta > 2 ?)
delta_Vent

#----------------------------------------------------------------

# Test de la variable Températures
# --------------------------------
modTemp <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modTemp.rds")

# AICs
aic_tab_Temp <- AIC(mod30_night, modTemp)

# Comparaison
delta_Temp <- with(aic_tab_Temp, AIC[Model == "mod30_night"] - AIC[Model == "modTemp"])

# Visualisation (delta > 2 ?)
delta_Temp

##########################################################################

# Test de la variable Distance
# ----------------------------
modLIGHT <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modLIGHT.rds")

# AICs
aic_tab_LIGHT <- AIC(mod30_night, modLIGHT)

# Comparaison
delta_LIGHT <- with(aic_tab_LIGHT, AIC[Model == "mod30_night"] - AIC[Model == "modLIGHT"])

# Visualisation (delta > 2 ?)
delta_LIGHT

#----------------------------------------------------------------

# Test de la variable ALAN (ON/OFF)
# ---------------------------------
modALAN <- readRDS("C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/modALAN.rds")

# AICs
aic_tab_ALAN <- AIC(mod30_night, modALAN)

# Comparaison
delta_ALAN <- with(aic_tab_ALAN, AIC[Model == "mod30_night"] - AIC[Model == "modALAN"])

# Visualisation (delta > 2 ?)
delta_ALAN

######################################################################################
# Résumer le tout en 1 figure
# ---------------------------

# Créer un tableau avec toutes les variables testées
results <- data.frame(
  variable = c("Semaines de l'année", "Utilisation de surface", "Hauteur de végétation", 
                "Vent", "Température", "Précipitations", "Couverture neigeuse", "Lune", 
                "Distance lampadaire", "Allumage / Extinction"),
  delta_AIC = c(delta_week, delta_Habitat, delta_Veg, 
                delta_Vent, delta_Temp, delta_Precip, delta_Snow, delta_Moon, 
                delta_LIGHT, delta_ALAN))

results

AIC_results <- ggplot(
  results, aes(x = reorder(variable, delta_AIC), y = delta_AIC)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_hline(yintercept = 2, linetype = "dashed", color = "red") +
  labs(
    title = "Comparaison des modèles univariés au modèle nul (Δ AIC)",
    x = "Variables testées",
    y = "Δ AIC (modèle nul - modèles univariés)") +
  annotate(
    "label",
    x = 1,                               
    y = max(results$delta_AIC),          
    hjust = 1, vjust = -0.3,
    label = "Δ AIC > 2 => la variable influence\nsignificativement l'ajustement du modèle") +
  theme_minimal()

ggsave(
 "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/Figures analyses/AIC_results.png",
 plot = AIC_results,
 width = 10,
 height = 6,
 dpi = 300)
