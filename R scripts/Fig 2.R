library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(ggbreak)
library(patchwork)

dat1 <- read_excel('/Users/AU775281/Documents/Msc Ali Fakhar/After Denmark/Manuscript CHANS and Field/Raw Data/Fig 2.xlsx')


#Making data for Total, Agri and other NH3 emissions
dat1_long <- dat1 %>%
  pivot_longer(cols = c(`U fertilizer`, `U Manure`, `U other`),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = recode(variable,
                           `U fertilizer` = "Chemical fertilizer",
                           `U Manure` = "Manure",
                           `U other` = "Other"),
         variable = factor(variable, levels = c("Chemical fertilizer", "Manure", "Other")))


p1 <- ggplot(dat1_long, aes(x = year, y = value, color = variable)) + 
  geom_line() + 
  geom_point() + 
  scale_x_continuous(breaks = seq(2001, 2019, by = 2)) + 
  scale_y_continuous(breaks = seq(0, 11.9, by = 2), limits = c(0, 11.9), expand = c(0, 0)) + 
  # Add custom strip-like title
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 10.9, ymax = 11.9, fill = "grey90") +
  annotate("text", x = mean(range(as.numeric(dat1_long$year))), y = 11.4, label = "Upland", fontface = "bold", size = 4.5) +
  labs(
    y = expression(paste(NH[3], "-N emissions (Gg)"))
  ) +
  theme_bw() + 
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = "none",
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = NA, color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) + 
  guides(color = guide_legend(nrow = 3)) + 
  scale_color_manual(values = c(
    "Chemical fertilizer" = "#48AAAD", 
    "Manure" = "#d95f02", 
    "Other" = "#7570b3"
  )); p1

#Making data for Agricultural NH3 emissions
dat2_long <- dat1 %>%
  pivot_longer(cols = c(`CF U urea`, `CF U Others`),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = recode(variable,
                           `CF U urea` = "Urea",
                           `CF U Others` = "Other"),
         variable = factor(variable, levels = c("Urea", "Other")))    

p2 <- ggplot(dat2_long, aes(x = year, y = value, color = variable)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2001, 2019, by = 2)) +
  scale_y_continuous(breaks = seq(0, 10, by = 2), expand = expansion(mult = c(0.05, 0.2))) + 
  labs(
    y = expression(paste(NH[3], " emissions (Gg)")),
    x = "Year",
    color = "Treatment"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),       
    axis.text = element_text(size = 12),      
    plot.title = element_text(size = 16, hjust = 0.5),
    strip.text = element_text(size = 14),       
    legend.text = element_text(size = 12),      
    legend.title = element_blank(),             
    legend.position = "none",
    legend.justification = c("left", "top"),
    legend.background = element_rect(fill = NA, color = NA),
    panel.grid.major = element_blank(),   
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2)) +
  scale_color_manual(values = c(
    "Urea" = "#0492C2",    
    "Other" = "#66a61e"   
  )); p2


#Making data for Total, Agri and other NH3 emissions
dat3_long <- dat1 %>%
  pivot_longer(cols = c(`P fertilizer`, `P Manure`, `P other`),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = recode(variable,
                           `P fertilizer` = "Chemical fertilizer",
                           `P Manure` = "Manure",
                           `P other` = "Other"),
         variable = factor(variable, levels = c("Chemical fertilizer", "Manure", "Other")))


p3 <- ggplot(dat3_long, aes(x = year, y = value, color = variable)) + 
  geom_line() + 
  geom_point() + 
  scale_x_continuous(breaks = seq(2001, 2019, by = 2)) + 
  scale_y_continuous(breaks = seq(2, 24.9, by = 3), limits = c(2, 24.9), expand = c(0, 0), position = "right") +
  # Add custom strip-like title
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 23, ymax = 24.9, fill = "grey90") +
  annotate("text", x = mean(range(as.numeric(dat1_long$year))), y = 23.8, label = "Lowland", fontface = "bold", size = 4.5) +
  labs(
    y = expression(paste(NH[3], "-N emissions (Gg)"))) + 
  theme_bw() + 
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5, color = "gray40"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = c(1, 0.92),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = NA, color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) + 
  guides(color = guide_legend(nrow = 3)) + 
  scale_color_manual(values = c(
    "Chemical fertilizer" = "#48AAAD", 
    "Manure" = "#d95f02", 
    "Other" = "#7570b3"
  )); p3


#Making data for Agricultural NH3 emissions
dat4_long <- dat1 %>%
  pivot_longer(cols = c(`CF P urea`, `CF P Others`),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = recode(variable,
                           `CF P urea` = "Urea",
                           `CF P Others` = "Other"),
         variable = factor(variable, levels = c("Urea", "Other")))    

p4 <- ggplot(dat4_long, aes(x = year, y = value, color = variable)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2001, 2019, by = 2)) +
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    expand = expansion(mult = c(0.05, 0.2)),
    position = "right"
  ) +
  labs(
    y = expression(paste(NH[3], " emissions (Gg)")),
    x = "Year",
    color = "Treatment"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),       
    axis.text = element_text(size = 12),      
    plot.title = element_text(size = 16, hjust = 0.5),
    strip.text = element_text(size = 14),       
    legend.text = element_text(size = 12),      
    legend.title = element_blank(),             
    legend.position = c(1, 0.98),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = NA, color = NA),
    panel.grid.major = element_blank(),   
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2)) +
  scale_color_manual(values = c(
    "Urea" = "#0492C2",    
    "Other" = "#66a61e"   
  )); p4


p1 <- p1 +
  annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1.5, size = 3.5, fontface = "bold")
p2 <- p2 +
  annotate("text", x = -Inf, y = Inf, label = "C", hjust = -0.5, vjust = 1.5, size = 3.5, fontface = "bold")
p3 <- p3 +
  annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1.5, size = 3.5, fontface = "bold")
p4 <- p4 +
  annotate("text", x = -Inf, y = Inf, label = "D", hjust = -0.5, vjust = 1.5, size = 3.5, fontface = "bold")

combined_plot <- (p1 | p3) / (p2 | p4) +
  theme(plot.tag = element_text(size = 10, face = "bold", hjust = 0)); combined_plot


#Saving plot#
ggsave("/Users/AU775281/Documents/Msc Ali Fakhar/After Denmark/Manuscript CHANS and Field/Figures/Fig 2.png", 
plot = combined_plot, 
width = 12, 
height = 10, 
dpi = 300, 
bg = "white")
