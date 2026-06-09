# Libraries required 
library(unmarked)
library(tidyverse)
library(lubridate)
library(mgcv)

# Load and prepare primary detection data
giraffe <- read.csv("data/distance_sampling.csv") %>%
  mutate(
    date = as.Date(date, format = "%d-%b-%y"),
    date_only = date,
    month_year = format(date, "%Y-%m"),
    transect = as.factor(transect),
    siteID = as.factor(siteID),
    dist = sight_distance * sin(angle * pi / 180)
  ) %>% 
  dplyr::select(-sight_distance, -angle)

# Convert zero group sightings into distance na values to preserve effort
giraffe$dist[giraffe$groupsize == 0] <- NA

## Density of giraffes crop raiding (distance sampling line transect)

# Visualize the perpendicular ditances 
hist(giraffe$dist, col = "darkgray", 
     border = "white", xlab = "Raw Distance (m)", main = "")


# Set truncation limits and distance bins
trunc <- 275
distance_bins <- seq(0, trunc, by = 25)

# Visualize truncated distance
all_distances <- giraffe$dist[!is.na(giraffe$dist)]
truncated_data <- all_distances[all_distances <= trunc]
hist(truncated_data, breaks = distance_bins, 
     col = "lightgray", border = "white", xlab = "Truncated Distance (m)", 
     ylab = "Frequency", main = "", xaxt = "n")

axis(1, at = distance_bins, labels = distance_bins, cex.axis = 0.7, las = 0)


# Convert raw perpendicular distances into multinomial site by bin counts per survey day
gira_data <- formatDistData(
  giraffe, 
  distCol = "dist", 
  transectNameCol = "siteID", 
  dist.breaks = distance_bins
)

# Extract site covariates ensuring exactly one row per unique survey day
site_covariates <- giraffe %>%
  group_by(siteID) %>%
  summarise(
    transect = first(transect),
    length_m = mean(length_m, na.rm = TRUE)
  ) %>%
  arrange(siteID)

# Construct the distance sampling frame linking transect location as a site covariate
UMF_gira <- unmarkedFrameDS(
  y = as.matrix(gira_data),
  survey = "line",
  tlength = site_covariates$length_m,
  dist.breaks = distance_bins,
  unitsIn = "m",
  siteCovs = data.frame(transect = site_covariates$transect)
)

# Fit competing models
haz_pooled    <- distsamp(~ 1 ~ 1, UMF_gira, keyfun = "hazard", output = "density", unitsOut = "kmsq")
haz_det_only  <- distsamp(~ transect ~ 1, UMF_gira, keyfun = "hazard", output = "density", unitsOut = "kmsq")
haz_fully_sep <- distsamp(~ transect ~ transect, UMF_gira, keyfun = "hazard", output = "density", unitsOut = "kmsq")

exp_pooled    <- distsamp(~ 1 ~ 1, UMF_gira, keyfun = "exp", output = "density", unitsOut = "kmsq")
exp_det_only  <- distsamp(~ transect ~ 1, UMF_gira, keyfun = "exp", output = "density", unitsOut = "kmsq")
exp_fully_sep <- distsamp(~ transect ~ transect, UMF_gira, keyfun = "exp", output = "density", unitsOut = "kmsq")

# Model selection 
master_selection <- fitList(
  'Hazard: Fully Pooled (~1 ~1)'                = haz_pooled,
  'Hazard: Separate Det / Equal Dens (~trans ~1)' = haz_det_only,
  'Hazard: Fully Separate (~trans ~trans)'       = haz_fully_sep,
  'Exp: Fully Pooled (~1 ~1)'                   = exp_pooled,
  'Exp: Separate Det / Equal Dens (~trans ~1)'    = exp_det_only,
  'Exp: Fully Separate (~trans ~trans)'          = exp_fully_sep
)

# Display model selection matrix
modSel(master_selection)

# Display results from chosen model
print(backTransform(haz_det_only, type = "state"))
print(exp(confint(haz_det_only, type = "state")))

# Extract the fully separate densities to demonstrate lack of biological divergence
site_targets <- data.frame(transect = c("Awarot", "Bouralgy", "Bula salam", "Jarirot"))
final_density_table <- cbind(site_targets, predict(haz_fully_sep, type = "state", newdata = site_targets))
print(final_density_table)

# Extract integrated zero to one average detection probabilities per location from the chosen model
p_matrix <- getP(haz_det_only)
site_probabilities <- site_covariates %>%
  mutate(Actual_p = p_matrix[, 1])

true_detection_table <- site_probabilities %>%
  group_by(transect) %>%
  summarise(
    Mean_Detection_Prob = mean(Actual_p),
    Min_Prob = min(Actual_p),
    Max_Prob = max(Actual_p)
  )

cat("\n Final Average Detection Probabilities per Location\n")
print(true_detection_table)

# Setup distance grid for plotting the hazard rate curves
dist_seq <- seq(0, trunc, length.out = 100)
plot_df <- expand.grid(distance = dist_seq, transect = c("Awarot", "Bouralgy", "Bula salam", "Jarirot"))

# Apply the back transformed shape and scale parameters to generate curves
shape_val <- exp(0.239) 

plot_df <- plot_df %>%
  mutate(
    scale_val = case_when(
      transect == "Awarot"      ~ 8.702,
      transect == "Bouralgy"    ~ 50.329,
      transect == "Bula salam"  ~ 68.720,
      transect == "Jarirot"     ~ 24.922
    ),
    p_detect = 1 - exp(-(distance / scale_val)^(-shape_val))
  )

# Plot the clean detection curves by location
p_detection <- ggplot(plot_df, aes(x = distance, y = p_detect, color = transect)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("#D95F02", "#1B9E77", "#7570B3", "#E7298A")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "",
    x = "Distance (m)",
    y = "Detection Probability (p)",
    color = "Location"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(color = "black", face = "bold"),
    axis.title = element_text(face = "bold")
  )

print(p_detection)

## How precipitation affect number of observed  giraffes (daily)

# Aggregate raw sightings into daily records
daily_totals <- giraffe %>% 
  group_by(date) %>% 
  summarise(
    total_giraffes = sum(groupsize, na.rm = TRUE),
    Precipitation = mean(Precipitation, na.rm = TRUE)
  )


# Fit poisson models for fair aic comparison
glm_linear <- glm(total_giraffes ~ Precipitation, family = poisson, data = daily_totals)
glm_poly   <- glm(total_giraffes ~ Precipitation + I(Precipitation^2), family = poisson, data = daily_totals)
gam_daily  <- gam(total_giraffes ~ s(Precipitation, k = 3), family = poisson, data = daily_totals)

# Model selection using aic
AIC(glm_linear, glm_poly, gam_daily) %>% .[order(.$AIC), ]

summary(gam_daily)

# Generate precipitation range for mapping
precip_range <- seq(min(daily_totals$Precipitation), max(daily_totals$Precipitation), length.out = 200)
newdata <- data.frame(Precipitation = precip_range)

# Calculate model fit predictions on the link scale
gam_pred <- predict(gam_daily, newdata = newdata, se.fit = TRUE, type = "link")

# Transform predictions back to response scale
plot_data <- data.frame(
  Precipitation = precip_range,
  GAMFit   = exp(gam_pred$fit),
  GAMUpper = exp(gam_pred$fit + 1.96 * gam_pred$se.fit),
  GAMLower = exp(gam_pred$fit - 1.96 * gam_pred$se.fit)
)

# Plot model curve with confidence interval ribbon
p <- ggplot(daily_totals, aes(x = Precipitation, y = total_giraffes)) + 
  geom_point(alpha = 0.6) + 
  geom_line(data = plot_data, aes(x = Precipitation, y = GAMFit), color = "#800020", linewidth = 1) + 
  geom_ribbon(data = plot_data, aes(x = Precipitation, ymin = GAMLower, ymax = GAMUpper), fill = "#800020", alpha = 0.2, inherit.aes = FALSE) + 
  labs(title = "", x = "Precipitation (mm)", y = "Giraffes / day") + 
  theme_bw(base_size = 12) + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5),
    axis.text = element_text(face = "bold", size = 14, color = "black"),
    axis.title = element_text(size = 14, face = "bold")
  )

# Display final plot
p

# Save final plot
# ggsave("outputs/group_precipitation.jpg", plot = p, width = 6, height = 4, units = "in", dpi = 300)

# predict
# lowest precipitation and highest
target_points <- data.frame(Precipitation = c(min(daily_totals$Precipitation), 10))

# Predict on the log link scale to keep standard errors mathematically stable
pred_link <- predict(gam_daily, newdata = target_points, se.fit = TRUE, type = "link")

# Calculate fit, lower CI, and upper CI, then transform back with exp()
results_table <- data.frame(
  Precipitation = target_points$Precipitation,
  Predicted_Giraffes = exp(pred_link$fit),
  Lower_95_CI        = exp(pred_link$fit - 1.96 * pred_link$se.fit),
  Upper_95_CI        = exp(pred_link$fit + 1.96 * pred_link$se.fit)
)

# Display the results
print(results_table)
