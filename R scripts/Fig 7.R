library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(ggbreak)
library(patchwork)

dat1 <- read_excel('/Users/AU775281/Documents/Msc Ali Fakhar/After Denmark/Manuscript CHANS and Field/Raw Data/Fig 7.xlsx')
colnames(dat1) <- dat1[1, ]  
dat1 <- dat1[-1, ] 

#Making seperate sub set for each plot
fwph <- dat1 %>% select(1:15)
fwnh4  <- dat1 %>% select(17:31)
fwno3 <- dat1 %>% select(33:47)
sph <- dat1 %>% 
  select(49:63) %>% 
  filter(if_all(everything(), ~ !is.na(.)))
snh4  <- dat1 %>% 
  select(65:79) %>% 
  filter(if_all(everything(), ~ !is.na(.)))
sno3 <- dat1 %>% 
  select(81:95 )%>% 
  filter(if_all(everything(), ~ !is.na(.)))

# First, convert relevant columns to numeric (assuming they might be character)
numeric_convert <- c("CNH3", "NH3 SD", "U45NH3", "U45NH3 SD", "U90NH3", "U90NH3 SD", "AS45NH3", "AS45NH3 SD", "AS90NH3", "AS90NH3 SD",
                     "Cont", "U45", "U90", "AS45", "AS90")

fwph <- fwph %>%
  mutate(across(all_of(numeric_convert ), ~ as.numeric(.)))
fwnh4 <- fwnh4 %>%
  mutate(across(all_of(numeric_convert ), ~ as.numeric(.)))
fwno3 <- fwno3 %>%
  mutate(across(all_of(numeric_convert ), ~ as.numeric(.)))
sph <- sph %>%
  mutate(across(all_of(numeric_convert ), ~ as.numeric(.)))
snh4 <- snh4 %>%
  mutate(across(all_of(numeric_convert ), ~ as.numeric(.)))
sno3 <- sno3 %>%
  mutate(across(all_of(numeric_convert ), ~ as.numeric(.)))

# Now pivot longer for pH columns and NH3 emissions
mapping <- tibble(
  treat_col = c("Cont", "U45", "U90", "AS45", "AS90"),
  nh3_col = c("CNH3", "U45NH3", "U90NH3", "AS45NH3", "AS90NH3"),
  sd_col = c("NH3 SD", "U45NH3 SD", "U90NH3 SD", "AS45NH3 SD", "AS90NH3 SD")
)

reshape_for_plot <- function(df) {
  long_df <- purrr::map_dfr(1:nrow(mapping), function(i) {
    df %>%
      transmute(
        value = .data[[mapping$treat_col[i]]],
        NH3 = .data[[mapping$nh3_col[i]]],
        SD = .data[[mapping$sd_col[i]]],
        Treatment = mapping$treat_col[i]
      )
  }) %>%
    mutate(
      Group = case_when(
        Treatment == "Cont" ~ "Con",
        Treatment %in% c("AS45", "AS90") ~ "AS",
        Treatment %in% c("U45", "U90") ~ "U",
        TRUE ~ "Other"
      )
    )
  
  return(long_df)
}
plot_fwph <- reshape_for_plot(fwph)
plot_fwnh4 <- reshape_for_plot(fwnh4) 
plot_fwno3 <- reshape_for_plot(fwno3)
plot_sph <- reshape_for_plot(sph)
plot_snh4 <- reshape_for_plot(snh4) 
plot_sno3 <- reshape_for_plot(sno3)

#Function for quadretic equation 
geteq <- function(data) {
  # Fit quadratic model
  m_quad <- lm(NH3 ~ value + I(value^2), data = data)
  
  # Extract p-value for overall model
  f_stat <- summary(m_quad)$fstatistic
  p_val <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
  
  # Determine significance stars
  p_stars <- function(p) {
    if (p < 0.001) return("***")
    else if (p < 0.01) return("**")
    else if (p < 0.05) return("*")
    else return("ns")
  }
  stars <- p_stars(p_val)
  
  # Create equation label
  coef_quad <- coef(m_quad)
  eq_label <- sprintf(
    "Y = %.2f + %.2f·x + %.2f·x², R² = %.3f %s",
    coef_quad[1], coef_quad[2], coef_quad[3],
    summary(m_quad)$r.squared, stars
  )
  
  return(eq_label)
}

eqlabel <- geteq(plot_fwph)

# Function to generate equation label data frame
create_eq_label_df <- function(data) {
  label <- geteq(data)
  
  data.frame(
    x = max(data$value, na.rm = TRUE) - 0.2,
    y = max(data$NH3, na.rm = TRUE),
    label = label
  )
}
eq_label_df_fwph   <- create_eq_label_df(plot_fwph)
eq_label_df_fwnh4  <- create_eq_label_df(plot_fwnh4)
eq_label_df_fwno3  <- create_eq_label_df(plot_fwno3)
eq_label_df_sph    <- create_eq_label_df(plot_sph)
eq_label_df_snh4   <- create_eq_label_df(plot_snh4)
eq_label_df_sno3   <- create_eq_label_df(plot_sno3)


fwphplot <- ggplot(plot_fwph, aes(x = value, y = NH3, color = Group)) +
  geom_point() +
  geom_ribbon(aes(ymin = NH3 - SD, ymax = NH3 + SD, fill = Group), alpha = 0.2, color = NA) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "black") +
  geom_text(data = eq_label_df_fwph, aes(x = x, y = y, label = label),
            hjust = 1.5, size = 3, color = "black") +
  scale_color_manual(values = c("U" = "#66a61e", "AS" = "#48AAAD", "Con" = "#d95f02")) +
  scale_fill_manual(values = c("U" = "#66a61e", "AS" = "#48AAAD", "Con" = "#d95f02")) +
  scale_y_continuous(breaks = seq(-0.4, 5.4, by = 1), limits = c(-0.4, 5.4), expand = c(0, 0)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 4.8, ymax = 5.4, fill = "grey90") +
  annotate("text", x = mean(range(plot_fwph$value, na.rm = TRUE)), y = 5.1, label = "Floodwater", fontface = "bold", size = 4.5) +
  labs(
    x = "pH",
    y = expression(paste(NH[3], " flux (kg N ha"^-1, ")"))
  ) +
  theme_bw() + 
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank()
  ); fwphplot

sphplot <- ggplot(plot_sph, aes(x = value, y = NH3, color = Group)) +
  geom_point() +
  geom_ribbon(aes(ymin = NH3 - SD, ymax = NH3 + SD, fill = Group), alpha = 0.2, color = NA) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "black") +
  geom_text(data = eq_label_df_sph, aes(x = x, y = y, label = label),
            hjust = 1.5, vjust = -11, size = 3, color = "black") +
  scale_x_continuous(breaks = seq(6.8, 8.2, by = 0.2), limits = c(6.8, 8.2)) +
  scale_color_manual(values = c("U" = "#66a61e", "AS" = "#48AAAD", "Con" = "#d95f02")) +
  scale_fill_manual(values = c("U" = "#66a61e", "AS" = "#48AAAD", "Con" = "#d95f02")) +
  scale_y_continuous(breaks = seq(-0.4, 5.4, by = 1), limits = c(-0.4, 5.4), expand = c(0, 0)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 4.8, ymax = 5.4, fill = "grey90") +
  annotate("text", x = mean(range(plot_sph$value, na.rm = TRUE)), y = 5.1, label = "Soil", fontface = "bold", size = 4.5) +
  labs(
    x = "pH",
    y = expression(paste(NH[3], " flux (kg N ha"^-1, ")"))
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank()
  ); sphplot

fwnhplot <- ggplot(plot_fwnh4, aes(x = value, y = NH3, color = Group)) +
  geom_point() +
  geom_ribbon(aes(ymin = NH3 - SD, ymax = NH3 + SD, fill = Group), alpha = 0.2, color = NA) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "black") +
  geom_text(data = eq_label_df_fwnh4, aes(x = x, y = y, label = label),
            hjust = 1.7, vjust = -5, size = 3, color = "black") +
  scale_color_manual(values = c("U" = "#66a61e", "AS" = "#48AAAD", "Con" = "#d95f02")) +
  scale_fill_manual(values = c("U" = "#66a61e", "AS" = "#48AAAD", "Con" = "#d95f02")) +
  scale_x_continuous(breaks = seq(0, 30, by = 5), limits = c(0, 30)) +
  scale_y_continuous(breaks = seq(-0.4, 5.8, by = 1), limits = c(-0.4, 5.8), expand = c(0, 0)) +
  labs(
    x = expression(NH[4]*"-N"~"(mg L"^{-1}*")"),
    y = expression(paste(NH[3], " flux (kg N ha"^-1, ")"))
  ) +
  theme_bw() + 
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank()
  ); fwnhplot 

snhplot <- ggplot(plot_snh4, aes(x = value, y = NH3, color = Group)) +
  geom_point() +
  geom_ribbon(aes(ymin = NH3 - SD, ymax = NH3 + SD, fill = Group), alpha = 0.2, color = NA) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "black") +
  geom_text(data = eq_label_df_snh4, aes(x = x, y = y, label = label),
            hjust = 1.7, vjust = -15, size = 3, color = "black") +
  scale_color_manual(values = c("U" = "#66a61e", "AS" = "#48AAAD", "Con" = "#d95f02")) +
  scale_fill_manual(values = c("U" = "#66a61e", "AS" = "#48AAAD", "Con" = "#d95f02")) +
  scale_x_continuous(breaks = seq(15, 59, by = 5), limits = c(15, 59)) +
  scale_y_continuous(breaks = seq(-0.4, 5.8, by = 1), limits = c(-0.4, 5.8), expand = c(0, 0)) +
  labs(
    x = expression(NH[4]*"-N"~"(mg L"^{-1}*")"),
    y = expression(paste(NH[3], " flux (kg N ha"^-1, ")"))
  ) +
  theme_bw() + 
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
  ); snhplot 

fwnoplot <- ggplot(plot_fwno3, aes(x = value, y = NH3, color = Group)) +
  geom_point() +
  geom_ribbon(aes(ymin = NH3 - SD, ymax = NH3 + SD, fill = Group), alpha = 0.2, color = NA) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "black") +
  geom_text(data = eq_label_df_fwno3, aes(x = x, y = y, label = label),
            hjust = 1.5, vjust = -3, size = 3, color = "black") +
  scale_color_manual(values = c("U" = "#66a61e", "AS" = "#48AAAD", "Con" = "#d95f02")) +
  scale_fill_manual(values = c("U" = "#66a61e", "AS" = "#48AAAD", "Con" = "#d95f02")) +
  scale_x_continuous(breaks = seq(0.2, 2.3, by = 0.5), limits = c(0.2, 2.3)) +
  scale_y_continuous(breaks = seq(-0.4, 5.2, by = 1), limits = c(-0.4, 5.2), expand = c(0, 0)) +
  labs(
    x = expression(NO[3]*"-N"~"(mg L"^{-1}*")"),
    y = expression(paste(NH[3], " flux (kg N ha"^-1, ")"))
  ) +
  theme_bw() + 
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank()
  ); fwnoplot

snoplot <- ggplot(plot_sno3, aes(x = value, y = NH3, color = Group)) +
  geom_point() +
  geom_ribbon(aes(ymin = NH3 - SD, ymax = NH3 + SD, fill = Group), alpha = 0.2, color = NA) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "black") +
  geom_text(data = eq_label_df_sno3, aes(x = x, y = y, label = label),
            hjust = 1.7, vjust = -15, size = 3, color = "black") +
  scale_color_manual(values = c("U" = "#66a61e", "AS" = "#48AAAD", "Con" = "#d95f02")) +
  scale_fill_manual(values = c("U" = "#66a61e", "AS" = "#48AAAD", "Con" = "#d95f02")) +
  scale_x_continuous(breaks = seq(0.2, 11, by = 2), limits = c(0.2, 11)) +
  scale_y_continuous(breaks = seq(-0.4, 5.2, by = 1), limits = c(-0.4, 5.2), expand = c(0, 0)) +
  labs(
    x = expression(NO[3]*"-N"~"(mg L"^{-1}*")"),
    y = expression(paste(NH[3], " flux (kg N ha"^-1, ")"))
  ) +
  theme_bw() + 
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ); snoplot

#combining plots
ph_label <- ggplot() + 
  theme_void() + 
  annotate("text", x = 0, y = 0, label = "pH", size = 5, hjust = 0) +
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
nh4_label <- ggplot() + 
  theme_void() + 
  annotate("text", x = 0.5, y = 0.5, 
           label = "NH[4]*'-N'~'(mg/L)'", parse = TRUE, 
           size = 5, hjust = 0.5) +
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
no3_label <- ggplot() + 
  theme_void() + 
  annotate("text", x = 0.5, y = 0.5, 
           label = "NO[3]*'-N'~'(mg/L)'", 
           parse = TRUE, size = 5, hjust = 0.5) +
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0))


combined_plot <- 
  (fwphplot | sphplot) / ph_label /
  (fwnhplot | snhplot) / nh4_label /
  (fwnoplot | snoplot) / no3_label +
  plot_layout(heights = c(1, 0.17, 1, 0.17, 1, 0.17));combined_plot

#Saving plot#
ggsave("/Users/AU775281/Documents/Msc Ali Fakhar/After Denmark/Manuscript CHANS and Field/Figures/Fig 7.png", 
plot = combined_plot, 
width = 10, 
height = 12, 
dpi = 300, 
bg = "white")
