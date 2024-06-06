library(tidyverse)
library(ggtext)
library(readxl)
library(giscoR)
library(countrycode)

load("04_24_Steuerflucht.RData")

world_res10 <- gisco_get_countries(year = "2020", resolution = "10", 
                                   epsg = "3035")

ctry <- codelist |> filter(continent == "Europe") |> pull(iso3c)


plotdat <- data |> 
  filter(counterpart == "EU_Havens", 
         indicator == "tax_loss_share_of_CITrev",
         year == 2020) |> 
  mutate(cat = cut(value, breaks = c(0, 0.04, 0.08, 0.12, 0.16, 1), 
                   labels = c("0-4", "4-8", "8-12", "12-16", ">16")))

# Grenzen EU-Staaten
eu_res10 <- world_res10 |> 
  filter(ISO3_CODE %in% ctry, ISO3_CODE != "RUS") |> 
  left_join(plotdat |> select(iso3, value, cat), by = c("ISO3_CODE" = "iso3")) |> 
  mutate(cat = fct_na_value_to_level(cat, "Profiteure"))


eu_res10 |> 
  ggplot() +
  geom_sf(data = world_res10, fill = "gray90", color = "NA") +
  geom_sf(aes(fill = cat), linewidth = 0.1, color = "black") +
  scale_fill_manual(name = "Entgangene Gewinnsteuern in %<br>aller Einnahmen aus Gewinnsteuern, 2020",
                    values = c(RColorBrewer::brewer.pal(5, "Reds"), "goldenrod1"),
                    guide = guide_legend(title.position = "top", 
                                         label.position = "bottom",
                                         keywidth = 1.5, keyheight = 0.5,
                                         nrow = 1,
                                         override.aes = list(color = NA))) +
  annotate("richtext", x = 1900000, y = 4300000, 
           label = "Steuerausfall durch<br>Gewinnverschiebungen<br>in EU-Steuersümpfe",
           family = "Alfa Slab One", size = 5, hjust = 0.5, 
           fill = NA, label.color = NA, lineheight = 1.3) +
  annotate("text", x = 950000, y = 1430000, 
           label = "Quelle: Atlas of the Offshore World. Grafik: @matschnetzer",
           family = "Barlow Condensed",
           size = 2.5, hjust = 0) +
  coord_sf(xlim = c(900000, 6600000), ylim = c(1380000, 5428510), expand=F) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = "Barlow Condensed") +
  theme(panel.background = element_rect(fill = "aliceblue", color = NA),
        axis.text = element_blank(),
        panel.grid.major = element_line(linetype = "longdash", linewidth = 0.3),
        legend.position = "inside",
        legend.position.inside = c(0.17, 0.54),
        legend.title = element_markdown(lineheight = 1.1, hjust = 0.5, size = 10),
        legend.text = element_text(size = 9))

ggsave("04_24_Steuerflucht.png", width = 8, height = 5.9, dpi = 320, bg = "white")

