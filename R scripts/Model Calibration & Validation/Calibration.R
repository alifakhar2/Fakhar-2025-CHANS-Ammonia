# -----------------------------
# CHANS Calibration 
# -----------------------------
library(readxl)
library(ggplot2)
library(patchwork)

# ---- Load dataset ----
cal_data <- read_xlsx('Source of data.xlsx')
names(cal_data) <- c("Year","Obs_Total_Gg","Obs_Agri_Gg","Mod_Total_Gg","Mod_Agri_Gg")

# ---- Function to find best multiplier by minimizing RMSE ----
find_best_multiplier <- function(obs, mod, seq_min=0.8, seq_max=1.2, step=0.01){
  multipliers <- seq(seq_min, seq_max, by=step)
  rmse_vals <- sapply(multipliers, function(m){
    sqrt(mean((mod * m - obs)^2))
  })
  best_m <- multipliers[which.min(rmse_vals)]
  return(best_m)
}

# ---- Model performance metrics ----
model_performance <- function(obs, mod){
  error <- mod - obs
  rmse <- sqrt(mean(error^2, na.rm = TRUE))
  bias <- mean(error, na.rm = TRUE)
  r2 <- cor(obs, mod, use="complete.obs")^2
  nrmse <- rmse / mean(obs, na.rm = TRUE) * 100
  data.frame(RMSE=rmse, Bias=bias, R2=r2, NRMSE_percent=nrmse)
}

# ---- Split into training and validation ----
train_data <- cal_data[cal_data$Year <= 2014, ]
test_data  <- cal_data[cal_data$Year > 2014, ]

# ---- Calibration for Agricultural NH3 ----
obs_train_agri <- train_data$Obs_Agri_Gg
mod_train_agri <- train_data$Mod_Agri_Gg
best_m_agri <- find_best_multiplier(obs_train_agri, mod_train_agri)
cat("Best multiplier for Agri NH3:", best_m_agri, "\n")

mod_train_cal_agri <- mod_train_agri * best_m_agri
mod_test_cal_agri  <- test_data$Mod_Agri_Gg * best_m_agri

train_stats_agri <- model_performance(obs_train_agri, mod_train_cal_agri)
test_stats_agri  <- model_performance(test_data$Obs_Agri_Gg, mod_test_cal_agri)

cat("Training performance for Agri NH3:\n"); print(train_stats_agri)
cat("Validation performance for Agri NH3:\n"); print(test_stats_agri)

# ---- Calibration for Total NH3 ----
obs_train_total <- train_data$Obs_Total_Gg
mod_train_total <- train_data$Mod_Total_Gg
best_m_total <- find_best_multiplier(obs_train_total, mod_train_total)
cat("Best multiplier for Total NH3:", best_m_total, "\n")

mod_train_cal_total <- mod_train_total * best_m_total
mod_test_cal_total  <- test_data$Mod_Total_Gg * best_m_total

train_stats_total <- model_performance(obs_train_total, mod_train_cal_total)
test_stats_total  <- model_performance(test_data$Obs_Total_Gg, mod_test_cal_total)

cat("Training performance for Total NH3:\n"); print(train_stats_total)
cat("Validation performance for Total NH3:\n"); print(test_stats_total)

# ---- Add calibrated values and period for plotting ----
cal_data$Mod_Cal_Agri  <- cal_data$Mod_Agri_Gg * best_m_agri
cal_data$Mod_Cal_Total <- cal_data$Mod_Total_Gg * best_m_total
cal_data$Period <- ifelse(cal_data$Year <= 2014, "Training", "Validation")

# ---- Plot calibration ----
p1 <- ggplot(cal_data, aes(x = Obs_Agri_Gg, y = Mod_Cal_Agri, color = Period)) +
  geom_point(size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  coord_equal() +
  
  # ---- Axis scales (extend top for strip space) ----
scale_x_continuous(breaks = seq(160, 260, by = 20)) +
  scale_y_continuous(
    breaks = seq(160, 260, by = 20),
    limits = c(158, 270),      # extend upper limit to host the strip
    expand = c(0, 0),
    position = "right" 
  ) +
  
  # ---- Custom strip title ----
annotate("rect",
         xmin = -Inf, xmax = Inf,
         ymin = 260, ymax = 270,
         fill = "grey90") +
  annotate("text",
           x = mean(range(cal_data$Obs_Agri_Gg, na.rm = TRUE)),
           y = 265,
           label = "Agriculture",
           fontface = "bold",
           size = 5) +
  
  labs(
    x = "Observed emissions (Gg)",
    y = "Modeled emissions (Gg)"
  ) +
  
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = c(0, 0.92),
    legend.justification = c("left", "top"),
    legend.background = element_rect(fill = NA, color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2));p1

p2 <- ggplot(cal_data, aes(x = Obs_Total_Gg, y = Mod_Cal_Total, color = Period)) +
  geom_point(size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  
  coord_equal(xlim = c(200, 359), ylim = c(200, 359)) +
  
  scale_x_continuous(breaks = seq(210, 359, by = 30), expand = c(0, 0)) +
  scale_y_continuous(
    breaks = seq(210, 360, by = 25),
    expand = c(0, 0),
    position = "left"   
  ) +
  
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = 345, ymax = 359,
           fill = "grey90") +
  annotate("text",
           x = mean(range(cal_data$Obs_Total_Gg, na.rm = TRUE)),
           y = 352.5,
           label = "Total",
           fontface = "bold",
           size = 5) +
  
  labs(
    x = "Observed emissions (Gg)",
    y = "Modeled emissions (Gg)"
  ) +
  
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = c(0, 0.92),
    legend.justification = c("left", "top"),
    legend.background = element_rect(fill = NA, color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2)); p2


# Arrange side by side
combined <- p2 +p1; combined
 
#Saving plot#
ggsave("Export library/Fig 1.png", 
        plot = combined, 
        width = 12, 
        height = 8, 
        dpi = 300, 
        bg = "white")

