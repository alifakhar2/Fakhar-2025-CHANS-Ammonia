library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(ggbreak)
library(patchwork)

dat1 <- read_excel('Source of data.xlsx')
colnames(dat1) <- dat1[1, ]  
dat1 <- dat1[-1, ] 



#Making seperate sub set for each plot
sph <- dat1 %>% select(1:11)
snh4  <- dat1 %>% select(1, 13:22)
sno3 <- dat1 %>% select(1, 24:33)

#Seperating Mean and SD values for plotting for Urea and AS treatments 
sph.mean <- sph %>%
  select(DAT, Cont, U45, U90, AS45, AS90) %>%
  pivot_longer(-DAT, names_to = "variable", values_to = "value")
snh4.mean <- snh4 %>%
  select(DAT, Cont, U45, U90, AS45, AS90) %>%
  pivot_longer(-DAT, names_to = "variable", values_to = "value")
sno3.mean <- sno3 %>%
  select(DAT, Cont, U45, U90, AS45, AS90) %>%
  pivot_longer(-DAT, names_to = "variable", values_to = "value")


sph.sd <- sph %>%
  select(DAT,`Cont SD`, `U45 SD`, `AS45 SD`,`U90 SD`, `AS90 SD`) %>%
  rename(
    U45 = `U45 SD`,
    AS45 = `AS45 SD`,
    U90 = `U90 SD`,
    AS90 = `AS90 SD`,
    Cont = `Cont SD`
  ) %>%
  pivot_longer(-DAT, names_to = "variable", values_to = "sd")
snh4.sd <- snh4 %>%
  select(DAT, `Cont SD`, `U45 SD`, `AS45 SD`,`U90 SD`, `AS90 SD`) %>%
  rename(
    U45 = `U45 SD`,
    AS45 = `AS45 SD`,
    U90 = `U90 SD`,
    AS90 = `AS90 SD`,
    Cont = `Cont SD`
  ) %>%
  pivot_longer(-DAT, names_to = "variable", values_to = "sd")
sno3.sd <- sno3 %>%
  select(DAT, `Cont SD`, `U45 SD`, `AS45 SD`,`U90 SD`, `AS90 SD`) %>%
  rename(
    U45 = `U45 SD`,
    AS45 = `AS45 SD`,
    U90 = `U90 SD`,
    AS90 = `AS90 SD`,
    Cont = `Cont SD`
  ) %>%
  pivot_longer(-DAT, names_to = "variable", values_to = "sd")

#Combine value and sd data frames
sph.dat <- left_join(sph.mean, sph.sd, by = c("DAT", "variable"))
snh4.dat <- left_join(snh4.mean, snh4.sd, by = c("DAT", "variable"))
sno3.dat <- left_join(sno3.mean, sno3.sd, by = c("DAT", "variable"))

#Subcategorization of longer format datasets
sph_AS <- sph.dat %>% 
  filter(variable %in% c("Cont", "AS45", "AS90"))
sph_U <- sph.dat %>% 
  filter(variable %in% c("Cont", "U45", "U90"))
snh4_AS <- snh4.dat %>% 
  filter(variable %in% c("Cont", "AS45", "AS90"))
snh4_U <- snh4.dat %>% 
  filter(variable %in% c("Cont", "U45", "U90"))
sno3_AS <- sno3.dat %>% 
  filter(variable %in% c("Cont", "AS45", "AS90"))
sno3_U <- sno3.dat %>% 
  filter(variable %in% c("Cont", "U45", "U90"))

#Set factor levels to control legend order ===
sph_AS$variable <- factor(sph_AS$variable, levels = c("Cont", "AS45", "AS90"))


#Convert to numeric 
sph_AS <- sph_AS %>%
  mutate(
    DAT = as.numeric(DAT),
    value = as.numeric(value),
    sd = as.numeric(sd)
  )
sph_U <- sph_U %>%
  mutate(
    DAT = as.numeric(DAT),
    value = as.numeric(value),
    sd = as.numeric(sd)
  )
snh4_AS <- snh4_AS %>%
  mutate(
    DAT = as.numeric(DAT),
    value = as.numeric(value),
    sd = as.numeric(sd)
  )
snh4_U <- snh4_U %>%
  mutate(
    DAT = as.numeric(DAT),
    value = as.numeric(value),
    sd = as.numeric(sd)
  )
sno3_AS <- sno3_AS %>%
  mutate(
    DAT = as.numeric(DAT),
    value = as.numeric(value),
    sd = as.numeric(sd)
  )
sno3_U <- sno3_U %>%
  mutate(
    DAT = as.numeric(DAT),
    value = as.numeric(value),
    sd = as.numeric(sd)
  )

#color and fill manual scales
U_scales <- list(
  scale_color_manual(
    values = c("Cont" = "#d95f02", "U45" = "#48AAAD", "U90" = "#66a61e"),
    labels = c(
      "Cont" = expression("0 kg ha"^-1),
      "U45" = expression("45 kg ha"^-1),
      "U90" = expression("90 kg ha"^-1)
    )
  ),
  scale_fill_manual(
    values = c("Cont" = "#d95f02", "U45" = "#48AAAD", "U90" = "#66a61e"),
    labels = c(
      "Cont" = expression("0 kg ha"^-1),
      "U45" = expression("45 kg ha"^-1),
      "U90" = expression("90 kg ha"^-1)
    )
  )
)

AS_scales <- list(
  scale_color_manual(
    values = c("Cont" = "#d95f02", "AS45" = "#48AAAD", "AS90" = "#66a61e"),
    labels = c(
      "Cont" = expression("0 kg ha"^-1),
      "AS45" = expression("45 kg ha"^-1),
      "AS90" = expression("90 kg ha"^-1)
    )
  ),
  scale_fill_manual(
    values = c("Cont" = "#d95f02", "AS45" = "#48AAAD", "AS90" = "#66a61e"),
    labels = c(
      "Cont" = expression("0 kg ha"^-1),
      "AS45" = expression("45 kg ha"^-1),
      "AS90" = expression("90 kg ha"^-1)
    )
  )
)

#Plotting
phu <- ggplot(sph_U, aes(x = DAT, y = value, color = variable, group = variable)) + 
  geom_line() + 
  geom_point() + 
  geom_ribbon(aes(ymin = value - sd, ymax = value + sd, fill = variable), alpha = 0.2, color = NA) +
  scale_x_continuous(breaks = seq(0, 135, by = 20)) + 
  scale_y_continuous(breaks = seq(6.7, 8.3, by = 0.3), limits = c(6.7, 8.3), expand = c(0, 0)) +
  # Add custom strip-like title
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 8.2, ymax = 8.3, fill = "grey90") +
  annotate("text", x = mean(range(as.numeric(sph_AS$DAT))), y = 8.25, label = "Urea", fontface = "bold", size = 4.5) +
  labs(
    y = expression(pH~"(1:5 H"["2"]*"O)")
  ) +
  theme_bw() + 
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) + 
  guides(color = guide_legend(nrow = 3)) +
  U_scales; phu

phas <- ggplot(sph_AS, aes(x = DAT, y = value, color = variable, group = variable)) + 
  geom_line() + 
  geom_point() + 
  geom_ribbon(aes(ymin = value - sd, ymax = value + sd, fill = variable), alpha = 0.2, color = NA) +
  scale_x_continuous(breaks = seq(0, 135, by = 20)) + 
  scale_y_continuous(breaks = seq(6.7, 8.3, by = 0.3), limits = c(6.7, 8.3), expand = c(0, 0)) +
  # Add custom strip-like title
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 8.2, ymax = 8.3, fill = "grey90") +
  annotate("text", x = mean(range(as.numeric(sph_AS$DAT))), y = 8.25, label = "Ammonium Sulfate", fontface = "bold", size = 4.5) +
  labs(
    y = expression(pH~"(1:5 H"["2"]*"O)")
  ) +
  theme_bw() + 
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = c(1, 0.93),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = NA, color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) + 
  guides(color = guide_legend(nrow = 3)) +
  AS_scales; phas

nh4u <- ggplot(snh4_U, aes(x = DAT, y = value, color = variable, group = variable)) + 
  geom_line() + 
  geom_point() + 
  geom_ribbon(aes(ymin = value - sd, ymax = value + sd, fill = variable), alpha = 0.2, color = NA) +
  scale_x_continuous(breaks = seq(0, 135, by = 20)) + 
  scale_y_continuous(breaks = seq(13, 59, by = 5), limits = c(13, 59), expand = c(0, 0)) +
  labs(
    y = expression(NH[4]*"-N"~"(mg L"^{-1}*")")
  ) +
  theme_bw() + 
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) + 
  guides(color = guide_legend(nrow = 3)) +
  U_scales; nh4u
nh4as <- ggplot(snh4_AS, aes(x = DAT, y = value, color = variable, group = variable)) + 
  geom_line() + 
  geom_point() + 
  geom_ribbon(aes(ymin = value - sd, ymax = value + sd, fill = variable), alpha = 0.2, color = NA) +
  scale_x_continuous(breaks = seq(0, 135, by = 20)) + 
  scale_y_continuous(breaks = seq(13, 59, by = 5), limits = c(13, 59), expand = c(0, 0)) +
  labs(
    y = expression(NH[4]*"-N"~"(mg L"^{-1}*")")
  ) +
  theme_bw() + 
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) + 
  guides(color = guide_legend(nrow = 3)) +
  AS_scales; nh4as


no3u <- ggplot(sno3_U, aes(x = DAT, y = value, color = variable, group = variable)) + 
  geom_line() + 
  geom_point() + 
  geom_ribbon(aes(ymin = value - sd, ymax = value + sd, fill = variable), alpha = 0.2, color = NA) +
  scale_x_continuous(breaks = seq(0, 135, by = 20)) + 
  scale_y_continuous(breaks = seq(0, 12, by = 2), limits = c(0, 12), expand = c(0, 0)) +
  labs(
    y = expression(NO[3]*"-N"~"(mg L"^{-1}*")")
  ) +
  theme_bw() + 
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  ) + 
  guides(color = guide_legend(nrow = 3)) +
  U_scales; no3u
no3as <- ggplot(sno3_AS, aes(x = DAT, y = value, color = variable, group = variable)) + 
  geom_line() + 
  geom_point() + 
  geom_ribbon(aes(ymin = value - sd, ymax = value + sd, fill = variable), alpha = 0.2, color = NA) +
  scale_x_continuous(breaks = seq(0, 135, by = 20)) + 
  scale_y_continuous(breaks = seq(0, 12, by = 2), limits = c(0, 12), expand = c(0, 0)) +
  labs(
    y = expression(NO[3]*"-N"~"(mg L"^{-1}*")")
  ) +
  theme_bw() + 
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) + 
  guides(color = guide_legend(nrow = 3)) +
  AS_scales; no3as

phu <- phu +
  annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 3.8, size = 4, fontface = "bold")
phas <- phas+
  annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 3.8, size = 4, fontface = "bold")
nh4u <- nh4u +
  annotate("text", x = -Inf, y = Inf, label = "C", hjust = -0.5, vjust = 2, size = 4, fontface = "bold")
nh4as <- nh4as +
  annotate("text", x = -Inf, y = Inf, label = "D", hjust = -0.5, vjust = 2, size = 4, fontface = "bold")
no3u <- no3u +
  annotate("text", x = -Inf, y = Inf, label = "E", hjust = -0.5, vjust = 2, size = 4, fontface = "bold")
no3as <- no3as+
  annotate("text", x = -Inf, y = Inf, label = "F", hjust = -0.5, vjust = 2, size = 4, fontface = "bold")


combined_plot <- (phu | phas) / 
  (nh4u | nh4as) / 
  (no3u | no3as);combined_plot


#Saving plot#
ggsave("Export library/Fig 6.png", 
plot = combined_plot, 
width = 10, 
height = 12, 
dpi = 300, 
bg = "white")
