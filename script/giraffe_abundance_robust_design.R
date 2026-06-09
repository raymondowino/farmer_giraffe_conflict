# libraries required
library(Rcapture) 
library(tidyverse) 

# Load raw capture data
giraffe <- read.csv("data/capture_history.csv")

# Extract numeric matrix by dropping the id column
capture_histories <- as.matrix(giraffe[, -1])
storage.mode(capture_histories) <- "numeric"

# Create a blank matrix matching your five true calendar months
monthly_matrix <- matrix(0, nrow = nrow(capture_histories), ncol = 5)

# Manually pool columns based on your exact disproportionate field calendar
# Pollock's Robust Design framework to accommodate  uneven sampling schedule 
monthly_matrix[, 1] <- as.numeric(rowSums(capture_histories[, 1:5]) > 0)    # Jan: 5 days (cols 1-5)
monthly_matrix[, 2] <- capture_histories[, 6]                              # Feb: 1 day (col 6)
monthly_matrix[, 3] <- as.numeric(rowSums(capture_histories[, 7:22]) > 0)   # Mar: 16 days (cols 7-22)
monthly_matrix[, 4] <- as.numeric(rowSums(capture_histories[, 23:24]) > 0) # Apr: 2 days (cols 23-24)
monthly_matrix[, 5] <- as.numeric(rowSums(capture_histories[, 25:26]) > 0) # May: 2 days (cols 25-26)

# Assign clear monthly calendar headers
colnames(monthly_matrix) <- c("Jan", "Feb", "Mar", "Apr", "May")

# Fit the closed population models using the custom calendar matrix
fit_monthly <- closedp(monthly_matrix)

# Target the time-varying model structure for evaluation
model_row <- "Mt" 
abundance <- fit_monthly$results[model_row, "abundance"]
stderr    <- fit_monthly$results[model_row, "stderr"]

# Calculate proper mark recapture log normal confidence intervals
M_t <- nrow(capture_histories)
f0 <- abundance - M_t
C <- exp(1.96 * sqrt(log(1 + (stderr^2 / f0^2))))
lower_ci <- M_t + (f0 / C)
upper_ci <- M_t + (f0 * C)

# Combine final metrics into a clean summary data frame
final_report_df <- data.frame(
  Metric = c("Observed Individuals", "Estimated Abundance", 
             "Standard Error", "Lower 95% CI", "Upper 95% CI"),
  Value = c(M_t, round(abundance, 1), round(stderr, 1), 
            round(lower_ci, 1), round(upper_ci, 1))
)

# Displayfinal results 
print(final_report_df)

# Explore open model
# Fit an open population Jolly-Seber model to your calendar months
fit_open <- openp(monthly_matrix, dfreq = F)

# Print the open population summary parameters directly to console
print(fit_open)

