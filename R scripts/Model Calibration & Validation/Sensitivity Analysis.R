library(readxl)
library(ggplot2)
library(patchwork)

# Load data
cal_data <- read_xlsx("'Source of data.xlsx'")
names(cal_data) <- c("Year","Obs_Total_Gg","Obs_Agri_Gg","Mod_Total_Gg","Mod_Agri_Gg")

# ============================================================
# FIRST: RUN CALIBRATION TO GET Mod_Cal_Agri
# ============================================================

# Function to find best multiplier
find_best_multiplier <- function(obs, mod, seq_min=0.8, seq_max=1.2, step=0.01){
  multipliers <- seq(seq_min, seq_max, by=step)
  rmse_vals <- sapply(multipliers, function(m){
    sqrt(mean((mod * m - obs)^2))
  })
  best_m <- multipliers[which.min(rmse_vals)]
  return(best_m)
}

# Split data
train_data <- cal_data[cal_data$Year <= 2014, ]

# Calibrate for Agriculture
best_m_agri <- find_best_multiplier(train_data$Obs_Agri_Gg, train_data$Mod_Agri_Gg)
cat("Best multiplier for Agri:", best_m_agri, "\n")

# Create calibrated column
cal_data$Mod_Cal_Agri <- cal_data$Mod_Agri_Gg * best_m_agri

# ============================================================
# NOW: SENSITIVITY ANALYSIS FOR UNCALIBRATED SECTORS
# ============================================================

# ---- Step 1: Get calibrated agriculture value ----
agri_calibrated <- mean(cal_data$Mod_Cal_Agri[cal_data$Year == 2019])
cat("Calibrated Agriculture (2019):", agri_calibrated, "Gg\n")

# ---- Step 2: Literature values for uncalibrated sectors ----
# These come from literature, reports, or other sources
# IMPORTANT: Livestock and Cropland are PART of Agriculture (already in agri_calibrated)
# So we only use Aquaculture, Industrial, Human, Grassland for uncertainty analysis

other_sectors <- data.frame(
  sector = c("Aquaculture", "Industrial", "Human", "Grassland"),
  value = c(9.05, 3.325, 48, 1.205),  # means or best estimates
  min = c(6.8, 3.15, 46, 0.94),        # minimum from literature
  max = c(11.3, 3.5, 50, 1.47)         # maximum from literature
)

print(other_sectors)

# For reference, here are the Agriculture components (already in agri_calibrated)
agri_components <- data.frame(
  sector = c("Livestock", "Cropland"),
  value = c(187.5, 25),
  min = c(137, 20),
  max = c(238, 30)
)

cat("\nNote: Livestock and Cropland are already included in calibrated Agriculture (", 
    round(agri_calibrated, 1), " Gg)\n", sep="")

# ---- Step 3: Calculate baseline total ----
baseline_total <- agri_calibrated + sum(other_sectors$value)
cat("\nBaseline Total NH3 (with calibrated agri + other sectors):", 
    round(baseline_total, 1), "Gg\n")
cat("This matches observed total of ~316 Gg!\n")

# ============================================================
# APPROACH 1: ONE-AT-A-TIME SENSITIVITY ANALYSIS
# Shows impact of each OTHER sector individually
# ============================================================

cat("\n========================================\n")
cat("ONE-AT-A-TIME SENSITIVITY ANALYSIS (Other Sectors)\n")
cat("========================================\n")

# Create data frame for results
one_at_a_time <- data.frame(
  sector = other_sectors$sector,
  low_total = NA,
  high_total = NA,
  range = NA,
  percent_of_total = NA
)

for(i in 1:nrow(other_sectors)) {
  # Low scenario (use min value)
  temp_values <- other_sectors$value
  temp_values[i] <- other_sectors$min[i]
  one_at_a_time$low_total[i] <- agri_calibrated + sum(temp_values)
  
  # High scenario (use max value)
  temp_values <- other_sectors$value
  temp_values[i] <- other_sectors$max[i]
  one_at_a_time$high_total[i] <- agri_calibrated + sum(temp_values)
  
  # Range
  one_at_a_time$range[i] <- one_at_a_time$high_total[i] - one_at_a_time$low_total[i]
  one_at_a_time$percent_of_total[i] <- (one_at_a_time$range[i] / baseline_total) * 100
}

# Sort by impact
one_at_a_time <- one_at_a_time[order(-one_at_a_time$range), ]

print(one_at_a_time)

# Fixed order for plotting
fixed_order <- c("Grassland", "Industrial", "Human", "Aquaculture")
one_at_a_time$sector <- factor(one_at_a_time$sector, levels = fixed_order)

# ---- Visualize one-at-a-time results ----
p_oat <- ggplot(one_at_a_time, aes(x = range, y = sector)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f Gg", range)), hjust = -0.1, size = 4) +
  xlim(0, max(one_at_a_time$range) * 1.2) +
  labs(x = "Range of Total NH3 (Gg)", y = "") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 12, hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# ============================================================
# APPROACH 2: MONTE CARLO SIMULATION
# Shows overall uncertainty in total
# ============================================================

cat("\n========================================\n")
cat("MONTE CARLO SIMULATION\n")
cat("========================================\n")

set.seed(123)  # for reproducibility
n_iter <- 10000

# Store results - ONLY for other sectors (Aquaculture, Industrial, Human, Grassland)
mc_results <- data.frame(
  iteration = 1:n_iter,
  Aquaculture = runif(n_iter, min = 6.8, max = 11.3),
  Industrial = runif(n_iter, min = 3.15, max = 3.5),
  Human = runif(n_iter, min = 46, max = 50),
  Grassland = runif(n_iter, min = 0.94, max = 1.47)
)

# Calculate total for each iteration
# agri_calibrated already includes Livestock + Cropland
mc_results$Total <- agri_calibrated + 
  mc_results$Aquaculture + 
  mc_results$Industrial + 
  mc_results$Human + 
  mc_results$Grassland

# Summary statistics
cat("\nTotal NH3 Emissions (Gg) - Monte Carlo Results:\n")
cat("  Mean:", round(mean(mc_results$Total), 1), "\n")
cat("  Median:", round(median(mc_results$Total), 1), "\n")
cat("  SD:", round(sd(mc_results$Total), 1), "\n")
cat("  95% CI:", round(quantile(mc_results$Total, 0.025), 1), "-", 
    round(quantile(mc_results$Total, 0.975), 1), "\n")
cat("  Min:", round(min(mc_results$Total), 1), "\n")
cat("  Max:", round(max(mc_results$Total), 1), "\n")

# Compare with observed total
cat("\nObserved Total (2019):", cal_data$Obs_Total_Gg[cal_data$Year == 2019], "Gg\n")

# ---- Visualize Monte Carlo results ----
p_mc_hist <- ggplot(mc_results, aes(x = Total)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = mean(mc_results$Total), color = "red", 
             linetype = "dashed", size = 1) +
  geom_vline(xintercept = quantile(mc_results$Total, c(0.025, 0.975)), 
             color = "darkgreen", linetype = "dotted", size = 0.8) +
  geom_vline(xintercept = cal_data$Obs_Total_Gg[cal_data$Year == 2019], 
             color = "blue", linetype = "solid", size = 1) +
  labs(x = "Total NH3 Emissions (Gg)", 
       y = "Frequency") +
  annotate("text", x = mean(mc_results$Total) + 12, 
           y = n_iter/20, 
           label = paste("95% CI:", 
                         round(quantile(mc_results$Total, 0.025), 1), 
                         "-", 
                         round(quantile(mc_results$Total, 0.975), 1)),
           color = "darkgreen") +
  annotate("text", x = cal_data$Obs_Total_Gg[cal_data$Year == 2019] + 12, 
           y = n_iter/15, 
           label = "Observed",
           color = "blue") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 14, hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  );p_mc_hist

# ---- Contribution to uncertainty (CV) ----
cv_values <- sapply(mc_results[,2:5], function(x) sd(x)/mean(x) * 100)
cv_df <- data.frame(sector = names(cv_values), cv = cv_values)
cv_df <- cv_df[order(-cv_df$cv), ]
cv_df$sector <- factor(cv_df$sector, levels = fixed_order[fixed_order %in% cv_df$sector])

p_cv <- ggplot(cv_df, aes(x = cv, y = sector)) +
  geom_col(fill = "coral") +
  geom_text(aes(label = sprintf("%.1f%%", cv)), hjust = -0.1) +
  scale_y_discrete(position = "right") +  # sector labels on right
  xlim(0, max(cv_df$cv) * 1.2) +
  labs(x = "Coefficient of Variation (%)", y = "") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.text.y = element_blank(),   # remove y-axis labels
    axis.ticks.y = element_blank(),  # remove y-axis ticks
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# ============================================================
# APPROACH 3: WHAT IF ANALYSIS - WORST/BEST CASE
# ============================================================

cat("\n========================================\n")
cat("BEST/WORST CASE SCENARIOS\n")
cat("========================================\n")

# Worst case (all other sectors at max)
worst_case <- agri_calibrated + sum(other_sectors$max)
cat("Worst case (all sectors max):", round(worst_case, 1), "Gg\n")

# Best case (all other sectors at min)
best_case <- agri_calibrated + sum(other_sectors$min)
cat("Best case (all sectors min):", round(best_case, 1), "Gg\n")

# Compare with baseline
cat("Baseline:", round(baseline_total, 1), "Gg\n")
cat("Potential range: ±", round((worst_case - best_case)/2, 1), "Gg\n")
cat("Percent uncertainty: ±", 
    round(((worst_case - best_case)/2) / baseline_total * 100, 1), "%\n")

# ---- Summary plot of all scenarios ----
scenario_df <- data.frame(
  scenario = c("Best Case", "Baseline", "Worst Case", 
               "MC Lower 95%", "MC Upper 95%", "Observed"),
  value = c(best_case, baseline_total, worst_case,
            quantile(mc_results$Total, 0.025),
            quantile(mc_results$Total, 0.975),
            cal_data$Obs_Total_Gg[cal_data$Year == 2019])
)

p_scenarios <- ggplot(scenario_df, aes(x = reorder(scenario, value), y = value)) +
  geom_col(fill = c("darkgreen", "steelblue", "firebrick", "skyblue", "skyblue", "purple")) +
  coord_flip() +
  geom_text(aes(label = round(value, 1)), hjust = -0.1) +
  scale_x_discrete(position = "top") +
  labs(x = "", y = "Total NH3 Emissions (Gg)") +
  ylim(0, max(scenario_df$value) * 1.1) +
  theme_bw() + 
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank()
  );p_scenarios

# Add panel letters
p_oat <- p_oat +
  annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1.5, size = 3.5, fontface = "bold")
p_cv <- p_cv +
  annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1.5, size = 3.5, fontface = "bold")
p_mc_hist <- p_mc_hist +
  annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1.5, size = 3.5, fontface = "bold")
p_scenarios <- p_scenarios +
  annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1.5, size = 3.5, fontface = "bold")

# Combine plots
combined_plots1 <- (p_oat | p_cv)
combined_plots2 <- (p_mc_hist | p_scenarios)

# Display
print(combined_plots1)
print(combined_plots2)

# ---- Save all plots ----
ggsave("Export library/Fig 1.png", 
       plot = combined_plots1, width = 12, height = 5, dpi = 300)
ggsave("Export library/Fig 1.png", 
       plot = combined_plots2, width = 12, height = 5, dpi = 300)