library(tidyverse)
library(waffle)

data <- tribble(~gruppe, ~`nicht leistbar`, ~`nur mit reduziertem\nVerbrauch leistbar`, ~`ohne Einschränkung\nleistbar`, ~`keine Kosten`,
                "Gesamtbevölkerung", 5, 46, 48, 1,
                "Von Arbeitslosigkeit\nbetroffene Haushalte", 13, 57, 28, 2,
                "Ein-Eltern-Haushalte", 17, 52, 28, 3)

labdat <- tribble(~gruppe, ~x, ~y, ~lab,
                  factor("Gesamtbevölkerung"), 0, 0.5, 4.9,
                  factor("Von Arbeitslosigkeit\nbetroffene Haushalte"), 0, 0.5, 13.2,
                  factor("Ein-Eltern-Haushalte"), 0, 0.5, 17.1)

plotdata <- data |> pivot_longer(-gruppe, names_to = "cat", values_to = "vals") |> 
  mutate(gruppe = fct_relevel(gruppe, "Ein-Eltern-Haushalte", after = 2),
         cat = fct_inorder(cat)) 

plotdata |> 
  ggplot() +
  geom_text(data = labdat, aes(x = x, y = y), 
            label = scales::percent(labdat$lab, accuracy = 1, scale = 1), 
            color = "#831818", size = 3.5, family = "Roboto Condensed",
            hjust = 1, vjust = 0, nudge_x = 0.65, nudge_y = 0.15) +
  geom_pictogram(aes(color = cat, values = vals, label = cat), size = 3.5, 
                 n_rows = 10, flip = F, family = "Font Awesome 5 Free Solid", 
                 angle = 90, inherit.aes = F) +
  scale_color_manual(name = NULL, values = MetBrewer::met.brewer("Paquin")[c(1,3,8,6)], 
                     guide = guide_legend(label.position = "right",
                                          label.theme = element_text(size = 7, lineheight = 0.9,
                                                                     family = "Roboto Condensed", 
                                                                     margin = margin(r = 1.5, unit = "lines")),
                                          label.hjust = 0.5, nrow = 1, 
                                          override.aes = list(size = 6, angle = 90))) +
  scale_label_pictogram(name = NULL,
                        values = c("battery-empty", "battery-half", "battery-full", "ban")) +
  coord_fixed(expand = F, xlim = c(-1, NA), ratio = 1.3) +
  facet_wrap(~gruppe, strip.position = "top") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme_enhance_waffle() +
  labs(title = toupper("Leistbarkeit von Haushaltsenergie"),
       caption = "Anm.: Haushaltsenergie umfasst Heizen, Warmwasser, Kochen, Kühlung, Licht oder Haushaltsgeräte.\nQuelle: Statistik Austria, Daten für Q4/2022.") +
  theme(legend.position = "top", 
        legend.spacing.x = unit(0.2, unit = "lines"),
        legend.margin = margin(t = 1, unit = "lines"),
        plot.title = element_text(hjust = 0.5, family = "Cabinet Grotesk"),
        plot.title.position = "plot",
        plot.caption = element_text(size = 6, margin = margin(t = 2, unit = "lines")),
        panel.spacing.x = unit(1.5, "lines"))

ggsave(filename = "12_23_Energiearmut.png", width = 7, height = 4, dpi = 320, bg = "white")