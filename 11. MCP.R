# ======================================================================
# MCP 95 % des lièvres par traitement ALAN
# ======================================================================

# ======================================================================
# Import des données
# ======================================================================

# Chargement des données
all_pts <- read.csv(
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/Analyses/raw Hares data/STEPS_5min_dist_lamp.csv",
  stringsAsFactors = FALSE
)

# Fusion des 3 lièvres
all_pts <- bind_rows(hares)

# Conservation des données de nuit uniquement
all_pts <- all_pts %>%
  filter(diel == "night")

# Création d'un identifiant unique (Lièvre + ALAN)
all_pts <- all_pts %>%
  mutate(ID_ALAN = paste(ID, ALAN, sep = "_"))

# Vérification
table(all_pts$ID, all_pts$ALAN)

# ======================================================================
# Conversion en objet spatial
# ======================================================================

pts_sf <- st_as_sf(
  as.data.frame(all_pts),
  coords = c("x", "y"),
  crs = 2056)

pts_sp <- as(pts_sf, "Spatial")

# ======================================================================
# Calcul des MCP à 95 %
# ======================================================================

mcp95 <- mcp(
  pts_sp["ID_ALAN"],
  percent = 95)

mcp95_sf <- st_as_sf(mcp95)

# ======================================================================
# Séparation de l'identifiant
# ======================================================================

mcp95_sf <- mcp95_sf %>%
  mutate(
    ID = sub("_.*", "", id),
    ALAN = sub("^[^_]+_", "", id)
  ) %>%
  select(ID, ALAN, area, geometry)

# Vérification
table(mcp95_sf$ALAN)

# Doit afficher :
# OFF         3
# ON_Evening 3
# ON_Morning 3

# ======================================================================
# Export vers QGIS
# ======================================================================

st_write(
  mcp95_sf,
  "C:/Users/Enzo/Documents/Université/Thèse de Master/Thèse/QGIS/Lièvres/MCP95_final.gpkg",
  delete_dsn = TRUE)
