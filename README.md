# Enzo_Bertolo_MSc_thesis_2026
Enzo Bertolo. MSc Biodiversity Conservation (2024-2026). Thesis on the impact of light pollution and other environmental factors on the activity of the brown hare. Supervised by Sergio Rasmann (UNINE), Claude Fischer (HEPIA), Vincent Grognuz (Agroscope) and Sandrine Wider (Vogelwarte).

# Data 
Data collected by Enzo Bertolo between 5 December 2025 and 11 May 2026 on three brown hares in the Val-de-Ruz. The data are divided into three files according to the analyses in which they are used.

STEPS_5min: Sorted and categorised data suitable for analysing activity patterns using the Hidden Markov Model (HMM) method.

STEPS_5min_dist_lamp: Sorted and categorised data suitable for spatial analyses relating to fixed public streetlights

STEPS_5min_lm: Sorted and categorised data suitable for spatial analyses relating to mobile streetlights

# Description of each variable
‘ID’: Initial of the hare to which the data belongs

‘step’: distance travelled since the last point

‘angle’: angle since the last point

‘LIGHT_z’: standardised distance (z-score) for each individual (unitless)

‘moon_fraction_z’: standardised percentage of visible moon (z-score) across all individuals (unitless)

‘temp_z’: standardised (z-score) temperature (hourly average) across all individuals (unitless)

‘wind_z’: standardised (z-score) wind (hourly average) across all individuals (unitless)

‘rain_z’: standardised hourly precipitation (z-score) across all individuals (unitless)

‘x’: x-coordinate of the GPS location

‘y’: y-coordinate of the GPS location

‘timestamp_5’: Date (yy-mm-dd) and time (hh-mm-ss) of the GPS location

‘date_local’: Date (yy-mm-dd) of the GPS location

‘point_type’: Observed (recorded by the GPS collar) / Carried-forward: (created on the computer)

‘LAND_arrange’: Land-use category in which the GPS point falls (see thesis, Appendix 5, for the categories and their descriptions)

‘LAND_Height_arrange’: Vegetation height category in which the GPS point falls (0, 5, 10, 15, 20, 100 and 200+ cm)

‘LIGHT’: Distance to the nearest fixed, public streetlight (for the STEPS_5min_lm dataset, distance to the nearest mobile 
streetlight) (metres)

‘diel’: Point recorded during the ‘day’ (between sunrise and sunset) or the ‘night’ (between sunset and sunrise)

‘tod’: timestamp_5 / 60 = hour (without the minute or second at which the data point was recorded)

‘ALAN’: Lighting control: ON_Evening, OFF, ON_Morning

‘moon_fraction’: Moon illumination (%)

‘Snow_cat’: Snow cover category: No snow, light snow, moderate snow or heavy snow

‘temperature_°C Neuchâtel’: Average temperature at the time the GPS point was recorded (°C)

‘wind (km/h) Neuchâtel’: Average wind speed at the time the GPS point was recorded (km/h)

‘precipitation_mm Neuchâtel’: Total precipitation at the time the GPS coordinate was recorded (mm)

‘sunset’: Time of sunset on the day the GPS coordinate was recorded (yy:mm:dd hh:mm:ss)

‘sunrise’: Time of sunrise on the day the GPS coordinate was recorded (yy:mm:dd hh:mm:ss)

# Script
‘1. Packages.R’: 

Packages required to run the following scripts.

‘2. Comportements & modèle null.R’: 

This script prepares the 5-minute GPS step data and fits a three-state Hidden Markov Model (HMM) without covariates. It defines the three behavioural states (resting, foraging, and exploring), sets the initial model parameters based on daytime and nighttime movement patterns, estimates state-specific turning-angle distributions, and fits the HMM using step lengths and turning angles. The script then predicts the most likely behavioural states and their associated probabilities and saves the processed datasets and fitted null model for subsequent analyses.

‘3. HMM Jour & Nuit.R’: 

This script investigates how temporal variables influence the behavioural patterns of brown hares using a three-state Hidden Markov Model. It evaluates the effects of the day–night cycle and different periods of the day on transitions between resting, foraging, and exploring states. The script estimates confidence intervals for transition parameters, calculates the probability of each behavioural state for each temporal category, and generates visualisations of the results.

‘4. HMM Nuit.R’: 

This script investigates how meteorological, lunar, landscape and ALAN variables influence the behavioural patterns of brown hares using a three-state Hidden Markov Model. It evaluates the effects of the differerent variables on transitions between resting, foraging, and exploring states. The script estimates confidence intervals for transition parameters, calculates the probability of each behavioural state for each category or along the gradient of numeric variables, and generates visualisations of the results.

‘5. AIC & Comparaisons.R’:

This script compares univariate HMMs with a null model to assess the influence of different temporal, meteorological, lunar, lanscape and ALAN variables on the activity patterns of brown hares. It calculates the difference in AIC (ΔAIC) between each model and the null model and identifies variables that improve model fit. The results are then summarised and visualised in a single figure to facilitate comparison of the relative importance of the tested variables.

‘6. Colinéarité.R’:

This script assesses collinearity among the numerical, factorial, and combined explanatory variables considered for the brown hare activity models, prior to fitting the multivariate HMMs. Pearson correlations are computed for numerical variables (light distance, moon fraction, temperature, wind, precipitation) and Cramér's V is used for factorial variables, each summarised in a correlation matrix plot. Variance Inflation Factors (VIF/GVIF) are then calculated from a linear model including all variables  to detect any residual multicollinearity across the full variable set. The results are visualised in a bar plot with standard VIF thresholds.

‘7. HMM.R’:

This script fits and compares multivariate Hidden Markov Models (HMMs) of hare nocturnal activity, building on the univariate results from the previous steps. Starting from a full model including all candidate covariates (ALAN × distance to nearest lamp interaction, week, time-of-night class, land use, vegetation height, moon fraction, temperature, precipitation, wind, and snow cover), it iteratively fits a series of reduced models, each dropping one variable at a time (week, land use, vegetation, wind, temperature, precipitation, snow, moon). For each reduced model, stationary state probabilities are predicted across the ALAN × distance-to-lamp gradient and plotted (with confidence ribbons, faceted by behavioural state: resting, foraging, exploring) to visualise how the interactive effect of artificial light changes when a variable is excluded. Model fit is compared to the full model via ΔAIC, quantifying how much each dropped variable contributes to explaining the data. Finally, all ΔAIC values are summarised in a single bar plot, ranking variables by their relative importance and highlighting those whose removal significantly worsens model fit (|ΔAIC| > 2).

‘8. Représentation HMM.R’:

This script produces the final visualisations and coefficient summaries for the full multivariate HMM. It first extracts the model's regression coefficients (with confidence intervals from CIbeta) for all transition probabilities, reshapes them into a long-format table. For each category, the script generates dedicated plots of estimated stationary state probabilities (resting/active/transit) across covariate values, combining bar charts, error bars, and panel layouts (via grid.arrange) into composite figures.

‘9. Analyses distances lampadaires fixes.R’:

A one-way ANOVA compares the mean distance-to-lamp (LIGHT) across the three ALAN categories, followed by pairwise t-tests to identify which specific conditions differ significantly. Group means and their differences (evening-on vs. off, off vs. morning-on) are computed and displayed directly on the resulting boxplot, together with the ANOVA p-value and significance brackets, producing a single population-level figure. The same analysis is then repeated individually for each hare (Boguet, Shy, Giorgia), generating one boxplot per animal to show whether the pattern of avoidance/attraction to streetlamps under different ALAN conditions holds consistently across individuals or varies between them.

‘10. Analyses distances lampadaires mobiles.R’:

Data are subset to nighttime and split into five successive experimental periods per individual (OFF1, ON1, OFF2, ON2, OFF3). Distances from each hare position to the nearest mobile lamp are computed spatially (rasterising the three mobile lamp locations and extracting a distance-to-lamp raster at each GPS fix), and this new distance variable is added to the dataset. A map of distance-to-lamp across the two study sites is produced for visual reference. The core analysis then tests whether distance-to-lamp differs across the five periods, using an ANOVA (with Tukey post-hoc pairwise comparisons) at the population level, reporting mean differences between consecutive periods (e.g., OFF1 vs ON1) directly on a boxplot. As in the fixed-lamp script, the same period-by-period ANOVA and boxplot analysis is then repeated individually for each hare.

‘11. MCP.R’:

This script computes 95% Minimum Convex Polygon (MCP) home ranges for each hare under each ALAN condition (OFF, ON_Evening, ON_Morning).
