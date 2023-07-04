librarian::shelf(tidyverse, sf, scales)

load("07_22_Hitzetage.RData")

decadedat <- heatdays |> count(year, name, decade, .drop = FALSE) |> 
  summarise(n = mean(n), .by = c(name, decade))

plotdat <- blmap |> left_join(decadedat)

plotdat |> ggplot(aes(fill = n)) +
  facet_wrap(~decade, nrow = 3) +
  geom_sf(linewidth = 0.05, color = "black") +
  geom_text(data = plotdat |> slice_max(name, n = 1),
            aes(label = decade), x = 10, y = 48, size = 5, hjust = 0, family = "Roboto Condensed Light") +
  scale_fill_distiller(palette = "Reds", direction = 1, 
                       name = "Durchschnittliche Hitzetage (≥30°C) pro Jahr",
                       limits = c(0, 30), breaks = pretty_breaks(n = 3, 0:28)) +
  guides(fill = guide_colorbar(direction = "horizontal", barheight = 0.5, barwidth = 15, 
                               label.position = "bottom", title.position = "top", title.hjust = 0.5)) +
  labs(title = "HUNDSTAGE",
       subtitle = NULL,
       caption = "Anm.: Messdaten der jeweiligen Landeshauptstadt. Quelle: ZAMG") +
  theme_minimal(base_family = "Roboto Condensed Light") +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20, family = "Roboto Condensed", 
                                  margin = margin(b=3, unit="lines")),
        plot.title.position = "plot",
        plot.caption = element_text(size = 8),
        legend.title = element_text(size = 10),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = c(0.5,1.11))

ggsave("07_22_Hitzetage.png", width = 8, height = 5, dpi = 320, bg = "white") 
