library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(ggbreak)
library(patchwork)

dat1 <- read_excel('/Users/AU775281/Documents/Msc Ali Fakhar/After Denmark/Manuscript CHANS and Field/Raw Data/Fig 3.xlsx')

#Seperating Mean and SD values for plotting for Urea and AS treatments 
u.mean <- dat1 %>%
  select(DAT, cont, u45, u90,) %>%
  pivot_longer(-DAT, names_to = "variable", values_to = "value")
as.mean <- dat1 %>%
  select(DAT, cont, as45, as90) %>%
  pivot_longer(-DAT, names_to = "variable", values_to = "value")

u.sd <- dat1 %>%
  select(DAT, `con SD`, `u45 SD`, `u90 SD`) %>%
  rename(
    cont = `con SD`,
    u45 = `u45 SD`,
    u90 = `u90 SD`
  ) %>%
  pivot_longer(-DAT, names_to = "variable", values_to = "sd")

as.sd <- dat1 %>%
  select(DAT, `con SD`, `as45 SD`, `as90 SD`) %>%
  rename(
    cont = `con SD`,
    as45 = `as45 SD`,
    as90 = `as90 SD`
  ) %>%
  pivot_longer(-DAT, names_to = "variable", values_to = "sd")

# Combine value and sd data frames
uplot.dat <- left_join(u.mean, u.sd, by = c("DAT", "variable"))
asplot.dat <- left_join(as.mean, as.sd, by = c("DAT", "variable"))

#Set factor levels to control legend order ===
uplot.dat$variable <- factor(uplot.dat$variable, levels = c("cont", "u45", "u90"))
asplot.dat$variable <- factor(asplot.dat$variable, levels = c("cont", "as45", "as90"))

#Defining the pH selected point
x_line <- data.frame(
  xintercept = c(1, 14, 45),
  color = c("#BC544B", "#3B9AB2", "#E1AF00")
)



p1 <- ggplot(uplot.dat, aes(x = DAT, y = value, color = variable, group = variable)) + 
  geom_line() + 
  geom_point() + 
  geom_ribbon(aes(ymin = value - sd, ymax = value + sd, fill = variable), alpha = 0.2, color = NA) +
  geom_vline(xintercept = 1, color = "#BC544B", linetype = "dashed", size = 0.5, alpha = 0.7) +
  geom_vline(xintercept = 14, color = "#3B9AB2", linetype = "dashed", size = 0.5, alpha = 0.7) +
  geom_vline(xintercept = 45, color = "#E1AF00", linetype = "dashed", size = 0.5, alpha = 0.7) +
  scale_x_continuous(breaks = seq(0, 135, by = 20)) + 
  scale_y_continuous(breaks = seq(0, 5, by = 1), limits = c(0, 5.5), expand = c(0, 0)) +
  # Add custom strip-like title
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 5.1, ymax = 5.5, fill = "grey90") +
  annotate("text", x = mean(range(uplot.dat$DAT)), y = 5.3, label = "Urea", fontface = "bold", size = 4.5) +
  labs(
    y = expression(paste(NH[3], " flux (kg N ha"^-1, " day"^-1, ")"))
  ) +
  theme_bw() + 
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  guides(color = guide_legend(nrow = 3)) + scale_color_manual(
    values = c(
      "cont" = "#d95f02", 
      "u45" = "#48AAAD", 
      "u90" = "#66a61e"
    ),
    labels = c(
      "cont" = expression("0 kg ha"^-1),
      "u45" = expression("45 kg ha"^-1),
      "u90" = expression("90 kg ha"^-1)
    )
  ) +
  scale_fill_manual(
    values = c(
      "cont" = "#d95f02", 
      "u45" = "#48AAAD", 
      "u90" = "#66a61e"
    ),
    labels = c(
      "cont" = expression("0 kg ha"^-1),
      "u45" = expression("45 kg ha"^-1),
      "u90" = expression("90 kg ha"^-1)
    )); p1


p2 <- ggplot(asplot.dat, aes(x = DAT, y = value, color = variable, group = variable)) + 
  geom_line() + 
  geom_point() + 
  geom_ribbon(aes(ymin = value - sd, ymax = value + sd, fill = variable), alpha = 0.2, color = NA) +
  geom_vline(xintercept = 1, color = "#BC544B", linetype = "dashed", size = 0.5, alpha = 0.7) +
  geom_vline(xintercept = 14, color = "#3B9AB2", linetype = "dashed", size = 0.5, alpha = 0.7) +
  geom_vline(xintercept = 45, color = "#E1AF00", linetype = "dashed", size = 0.5, alpha = 0.7) +
  scale_color_manual(
    values = c(
      "cont" = "#d95f02", 
      "as45" = "#48AAAD", 
      "as90" = "#66a61e"
    ),
    labels = c(
      "cont" = expression("0 kg ha"^-1),
      "as45" = expression("45 kg ha"^-1),
      "as90" = expression("90 kg ha"^-1)
    )
  ) +
  scale_fill_manual(
    values = c(
      "cont" = "#d95f02", 
      "as45" = "#48AAAD", 
      "as90" = "#66a61e"
    ),
    labels = c(
      "cont" = expression("0 kg ha"^-1),
      "as45" = expression("45 kg ha"^-1),
      "as90" = expression("90 kg ha"^-1)
    )
  ) +
  scale_x_continuous(breaks = seq(0, 135, by = 20)) + 
  scale_y_continuous(breaks = seq(0, 5, by = 2), limits = c(0, 5.5), expand = c(0, 0)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 5.1, ymax = 5.5, fill = "grey90") +
  annotate("text", x = mean(range(uplot.dat$DAT)), y = 5.3, label = "Ammonium Sulfate", fontface = "bold", size = 4.5) +
  labs(
    y = expression(paste(NH[3], " flux (kg N ha"^-1, " day"^-1, ")"))
  ) +
  theme_bw() + 
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = c(0.98, 0.93),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = NA, color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  guides(color = guide_legend(nrow = 3)); p2


p1 <- p1 +
  annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 4.2, size = 4, fontface = "bold")

p2 <- p2 +
  annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 4.2, size = 4, fontface = "bold")
combined_plot <- p1 + p2; combined_plot

#Saving plot#
ggsave("/Users/AU775281/Documents/Msc Ali Fakhar/After Denmark/Manuscript CHANS and Field/Figures/Fig 3.png", 
       plot = combined_plot, 
       width = 12, 
       height = 5, 
       dpi = 300, 
       bg = "white")
