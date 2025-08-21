library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(ggbreak)
library(patchwork)

dat1 <- read_excel('Source of data.xlsx')


#Making data for Total, Agri and other NH3 emissions
dat1_long <- dat1 %>%
  pivot_longer(cols = c(total, agri, other),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = recode(variable,
                           total = "Total",
                           agri = "Agriculture",
                           other = "Other"),
         variable = factor(variable, levels = c("Total", "Agriculture", "Other")))


p1 <- ggplot(dat1_long, aes(x = year, y = value, color = variable)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2001, 2019, by = 2)) +
  scale_y_continuous(breaks = seq(30, 380, by = 60)) +
  labs(
    y = expression(paste(NH[3], "-N emissions (Gg)"))
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),       
    axis.text = element_text(size = 12),      
    legend.text = element_text(size = 12),      
    legend.title = element_blank(),             
    legend.position = c(0.01, 0.95),
    legend.justification = c("left", "top"),
    legend.background = element_rect(fill = NA, color = NA),
    panel.grid.major = element_blank(),   
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),        
    axis.text.x = element_blank(),         
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank()        
  ) +
  guides(color = guide_legend(nrow = 3)) + 
  scale_color_manual(values = c(
    "Total" = "#1b9e77",       
    "Agriculture" = "#d95f02",  
    "Other" = "#7570b3"        
  )); p1

#Making data for Agricultural NH3 emissions
dat2_long <- dat1 %>%
  pivot_longer(cols = c(crop, live),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = recode(variable,
                           crop = "Cropland",
                           live = "Livestock"),
         variable = factor(variable, levels = c("Cropland", "Livestock")))    

p2 <- ggplot(dat2_long, aes(x = year, y = value, color = variable)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2001, 2019, by = 2)) +
  scale_y_continuous(breaks = seq(20, 280, by = 35)) +
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
    legend.position = c(0.01, 0.95),
    legend.justification = c("left", "top"),
    legend.background = element_rect(fill = NA, color = NA),
    panel.grid.major = element_blank(),   
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),        
    axis.text.x = element_blank(),      
    axis.ticks.x = element_blank()      
  ) +
  guides(color = guide_legend(nrow = 2)) +
  scale_color_manual(values = c(
    "Cropland" = "#e7298a",    
    "Livestock" = "#66a61e"   
  )); p2


#Making data for Agricultural NH3 emissions
dat3_long <- dat1 %>%
  pivot_longer(cols = c(up, low),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = recode(variable,
                           up = "Upland",
                           low = "Lowland"),
         variable = factor(variable, levels = c("Upland", "Lowland")));p2

p3 <- ggplot(dat3_long, aes(x = year, y = value, color = variable)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2001, 2019, by = 2)) +
  scale_y_continuous(breaks = seq(5, 30, by = 3), expand = expansion(mult = c(0.05, 0.29))) + 
  labs(
    y = expression(paste(NH[3], "-N emissions (Gg)")),
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
    legend.position = c(0.01, 0.98),
    legend.justification = c("left", "top"),
    legend.background = element_rect(fill = NA, color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2))+ 
  scale_color_manual(values = c(
    "Upland" = "#a6761d",       
    "Lowland" = "#666666"   
  )); p3


p1 <- p1 +
  annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1.5, size = 3.5, fontface = "bold")
p2 <- p2 +
  annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1.5, size = 3.5, fontface = "bold")
p3 <- p3 +
  annotate("text", x = -Inf, y = Inf, label = "C", hjust = -0.5, vjust = 1.5, size = 3.5, fontface = "bold")


combined_plot <- (p1 / p2 / p3) + 
  plot_layout(heights = c(1, 1, 1));combined_plot


#Saving plot#
#ggsave("Export library/Fig 1.png", 
       plot = combined_plot, 
       width = 8, 
       height = 10, 
       dpi = 300, 
       bg = "white")
