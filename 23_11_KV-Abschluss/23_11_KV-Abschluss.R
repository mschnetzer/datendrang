library(tidyverse)
library(patchwork)

data <- tribble(~saison, ~branche, ~inflation, ~abschluss,
                "Herbstlohnrunde 2022", "Metall", 6.3, 7.4,
                "Herbstlohnrunde 2022", "Sozialdienste", 6.3, 8.0,
                "Herbstlohnrunde 2022", "Handel", 6.9, 7.3,
                "Herbstlohnrunde 2022", "Öffentlicher Dienst", 6.9, 7.3,
                "Herbstlohnrunde 2022", "Reinigung", 7.5, 8.6,
                "Herbstlohnrunde 2022", "Bewachung", 7.5, 10.3,
                "Herbstlohnrunde 2022", "Eisenbahn", 7.5, 8.0,
                "Frühjahrslohnrunde 2023", "Tourismus", 8.0, 9.3,
                "Frühjahrslohnrunde 2023", "Holzindustrie", 9.0, 9.8,
                "Frühjahrslohnrunde 2023", "Banken", 9.0, 9.5,
                "Frühjahrslohnrunde 2023", "Bau", 9.5, 9.5,
                "Frühjahrslohnrunde 2023", "Textilindustrie", 9.5, 9.8,
                "Frühjahrslohnrunde 2023", "Chemie", 9.7, 9.9,
                "Frühjahrslohnrunde 2023", "Elektro", 9.5, 10.5) |> 
  mutate(saison = fct_inorder(saison),
         branche = fct_inorder(branche))


filtered_data <- data |> mutate(diff = abschluss - inflation) |> 
  filter(branche %in% c("Metall", "Sozialdienste", "Handel", "Reinigung",
                        "Tourismus", "Bau", "Banken", "Elektro"))


plot <- filtered_data |> 
  ggplot(aes(x = branche)) +
  geom_segment(aes(y = inflation, yend = abschluss, xend = branche), 
               color = "firebrick", linewidth = 10) +
  geom_segment(aes(y = 0, yend = inflation, xend = branche), 
               color = "navyblue", linewidth = 10, alpha = 0.3) +
  geom_text(aes(y = abschluss-diff/2, label = paste0(sprintf("%.1f", diff))),
            color = "white", size = 2.5, family = "Roboto Condensed", hjust = 0.5, vjust = 0.5,
            data = filtered_data |> filter(!branche == "Bau")) +
  geom_text(aes(y = inflation, label = sprintf("%.1f", inflation)), nudge_y = -.2,
            color = "navyblue", size = 2.5, family = "Roboto Condensed", hjust = 0.5, vjust = 1) +
  geom_text(aes(y = abschluss, label = paste0(sprintf("%.1f", abschluss),"%")), nudge_y = .2,
            color = "black", size = 2.8, family = "Roboto Condensed", hjust = 0.5, vjust = 0,
            fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     breaks = c(0,5), limits = c(0,13), expand = c(0, 0)) +
  scale_x_discrete(labels = scales::label_wrap(5)) +
  facet_wrap(~saison, ncol = 2, scales = "free_x", strip.position = "bottom") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.2),
        panel.spacing.x = unit(1, units = "lines"),
        legend.position = "bottom",
        legend.text = element_text(margin = margin(r = 2, unit = "lines")),
        strip.placement = "outside",
        strip.text.x.bottom = element_text(hjust = 0.5, size = 11, 
                                           margin = margin(t = 1, unit = "lines")))

legend <- ggplot() +
  geom_segment(aes(y = 0.5, yend = 1, x = 1, xend = 1), 
               color = "firebrick", linewidth = 5) +
  geom_segment(aes(y = 0, yend = 0.5, x = 1, xend = 1), 
               color = "navyblue", linewidth = 5, alpha = 0.3) +
  geom_text(aes(y = 1.1, x = 1), label = "%", hjust = 0.5, vjust = 0, fontface = "bold",
            color = "black", size = 3, family = "Roboto Condensed") +
  geom_text(aes(y = 1.1, x = 1.02), label = "Abschluss", hjust = 0, vjust = 0,
            color = "black", size = 2.6, family = "Roboto Condensed") +
  geom_text(aes(x = 1, y = 0.25), label = "Rollierende\nInflation", color = "navyblue",
            nudge_x = 0.02, hjust = 0, size = 2.6, family = "Roboto Condensed", lineheight = 0.8) +
  geom_text(aes(x = 1, y = 0.75), label = "Durchschnittliche\nReallohnerhöhung", color = "firebrick",
            nudge_x = 0.02, hjust = 0, size = 2.6, family = "Roboto Condensed", lineheight = 0.8) +
  scale_x_continuous(expand = c(0.1,0.1)) +
  scale_y_continuous(expand = c(0.1,0.1)) +
  theme_void(base_family = "Roboto Condensed") +
  theme(panel.background = element_rect(fill = "white", color = NA))


plot + inset_element(legend, left = 0, bottom = 0.7, right = 0.25, top = 1,
                     align_to = "panel", on_top = T)

ggsave("23_11_KV-Abschluss.png", width = 8, height = 4, dpi = 320, bg = "white")

