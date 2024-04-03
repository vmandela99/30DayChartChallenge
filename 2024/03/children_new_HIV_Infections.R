## packages
library(tidyverse)
library(ggtext)
library(ggrepel)
library(patchwork)
library(systemfonts)
library(camcorder)


gg_record(dir = here::here("2024/03"), device = "png", 
          width = 1500 * 2, height = 1350 * 2, units = "px", dpi = 320)


theme_set(theme_minimal(base_size = 19, base_family = "Girassol"))

theme_update(
  text = element_text(color = "grey12"),
  axis.title = element_blank(),
  axis.text.x = element_text(family = "Iosevka Curly"),
  axis.text.y = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor = element_blank(),
  plot.margin = margin(20, 5, 10, 10),
  plot.subtitle = element_textbox_simple(family = "Roboto Condensed", size = 14,
                                         lineheight = 1.6),
  plot.title.position = "plot",
  plot.caption = element_text(family = "Iosevka Curly", color = "#b40059", hjust = .5,
                              size = 10, margin = margin(35, 0, 0, 0))
)

# read the data
df <- read_csv("2024/data/new_infected_data.csv")

#prep data
df_prep <- df %>%
  pivot_longer(cols = c(-Newly_infected_with_HIV)) %>% 
  rename(year = name, 
         n = value) %>% 
  group_by(Newly_infected_with_HIV) %>% 
  mutate(
    total = sum(n),
    current = n[which(year == 2021)]
  ) %>% 
  ungroup() %>%
  mutate(
    Newly_infected_with_HIV = fct_reorder(Newly_infected_with_HIV, total),
    Newly_infected_with_HIV = fct_relevel(Newly_infected_with_HIV, "Young_people_(ages 15-24)")
  )



df_sum <-
  df_prep %>% 
  filter(year <= 2021) %>% 
  group_by(year) %>% 
  summarize(n = sum(n))

# plot
p1 <- 
  df_sum %>% 
  ggplot(aes(year, n)) +
  geom_col(aes(fill = factor(year)), width = .85) +
  geom_col(
    data = df_prep %>% filter(Newly_infected_with_HIV == "Children_(ages 0-14)" & year <= 2021),
    aes(alpha = year == 2021),
    fill = "blue", width = .5
  ) +
  geom_text(
    data = df_sum %>% 
      mutate(n_lab = if_else(year %in% c(2012, 2021), paste0(n, "\nTotal"), as.character(n))),
    aes(label = n_lab), 
    family = "Iosevka Curly", size = 3.3, lineheight = .8, 
    nudge_y = 12, vjust = 0, color = "black", fontface = "bold"
  ) +
  geom_text(
    data = df_prep %>% filter(Newly_infected_with_HIV == "Children_(ages 0-14)" & year <= 2021) %>% 
      mutate(n_lab = if_else(year %in% c(2012, 2021), paste0(n, "\nChildren"), as.character(n))), 
    aes(label = n_lab), 
    family = "Iosevka Curly",
    color = "white", lineheight = .8, size = 3.0, 
    nudge_y = 12, vjust = 0, fontface = "bold"
  ) +
  geom_text(
    data = df_prep %>% filter(Newly_infected_with_HIV == "Children_(ages 0-14)" & year <= 2021),
    aes(y = -15, label = year, color = factor(year)), 
    family = "Iosevka Curly", size = 6, hjust = .5, vjust = 1
  ) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(limits = c(-15, NA)) +
  scale_color_manual(values = c(rep("black", 9), "#b40059", "grey70"), guide = "none") +
  scale_fill_manual(values = c(rep("purple", 9), "#b40059", "yellow"), guide = "none") +
  scale_alpha_manual(values = c(.25, .4), guide = "none") +
  labs(title = "<br><span style='font-size:20pt'>New HIV infections among children in Kenya decreased by 13% in the <b style='color:#b40059'>Pandemic Year</b>",
       subtitle = "<br><span style='font-size:14pt'>From 2019 to 2021, there was a 1.13% decrease in new HIV infections overall in Kenya, and a 16.13% decrease in new infections among children aged 0 to 14.",
       caption = paste("<br><span style='font-size:6pt'><b style='color:black'>Source: World Bank  ", "      ", "\nGraphics: Victor Mandela")) +
  theme(
    plot.title = element_markdown(size = 28, margin = margin(5, 35, 25, 35), color = "black"),
    plot.subtitle = element_textbox_simple(margin = margin(5, 35, 15, 35)),
    panel.grid.major = element_blank(),
    axis.text.x = element_blank(),
    plot.caption = element_markdown(hjust = 0.6, size = 9, lineheight = 0.8,
                                    family = "Charm", face = "bold",
                                    margin = margin(t = 8))
  )

p1


