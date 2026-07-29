# 02.00 PACKAGES
###############

# Spatial data handling (GIS, rasters, vector spatial objects)
# ------------------------------------------------------------
library(terra)        # raster and spatial vector data handling
library(sf)           # simple features for vector spatial data
library(tidyterra)    # tidyverse tools for terra objects
library(ggspatial)    # spatial layers and map annotations for ggplot2

# Movement ecology
# ----------------
library(amt)          # animal movement analysis and SSF/iSSF workflows
library(momentuHMM)   # hidden Markov models for movement behaviour
library(adehabitatHR) # minimal convex polygon

# Data manipulation and time handling
# -----------------------------------
library(tidyverse)    # core tidyverse packages
library(dplyr)        # data wrangling verbs
library(purrr)        # functional programming and iteration
library(lubridate)    # date and time handling
library(zoo)          # rolling functions and indexed time series
library(readxl)       # import Excel files

# Solar, diel, and environmental time variables
# ---------------------------------------------
library(suncalc)      # sunrise, sunset, moon phases, diel categories

# Statistical modelling
# ---------------------
library(lme4)         # linear and generalized mixed-effects models
library(ggeffects)    # marginal effects and model predictions
library(circular)     # circular statistics (angles, directions)
library(rstatix)      # turkey statistics

# Visualization and figure assembly
# ---------------------------------
library(ggplot2)      # plotting system
library(ggnewscale)   # multiple colour/fill scales in ggplot
library(ggpubr)       # publication-ready plots and statistical annotations
library(gridExtra)    # arranging multiple plots
library(grid)         # low-level graphical objects and layouts
library(png)          # save png images

# Reporting and reproducible documents
# ------------------------------------
library(knitr)        # tables and figure formatting in R Markdown

# Collinearity of variables
# -----------------------------------------
library(corrplot)     # calculating the collinearity of numeric variables
library(rcompanion)   # calculating the collinearity of factor variables
library(car)          # calculating the collinearity mixed variables


