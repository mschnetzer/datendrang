library(tidyverse)
library(giscoR)
library(ggflags)
library(sf)
library(ggtext)
library(countrycode)

# Datenquelle: https://www.statistik.at/fileadmin/announcement/2025/03/20250307Aussenhandel2024.pdf
data <- tribble(~country, ~euro, ~share,
                "Deutschland", 56.76, 29.7,
                "Vereinigte Staaten", 16.23, 8.5,
                "Italien", 11.70, 6.1,
                "Schweiz", 9.48, 5.0,
                "Polen", 7.34, 3.8,
                "Frankreich", 6.94, 3.6,
                "Ungarn", 6.84, 3.6,
                "Tschechien", 6.75, 3.5,
                "China", 5.30, 2.8,
                "Vereinigtes Königreich", 4.93, 2.6)

eumap <- gisco_get_countries(epsg = "3035", year = "2020", 
                             resolution = "20" , region = "Europe")  

austria <- eumap |> filter(NAME_GERM == "Österreich") |> 
  st_point_on_surface() |> 
  st_coordinates() |> 
  as_tibble() |> 
  mutate(iso2c = "at")

partners <- data |> 
  left_join(eumap, by = c("country" = "NAME_GERM")) |> 
  st_as_sf() |> 
  st_point_on_surface() |> 
  mutate(rank = dense_rank(desc(euro)),
         iso2c = tolower(countrycode(country, origin = "country.name.de", 
                                     destination = "iso2c")),
         geometry = case_match(iso2c, 
                               "de" ~ st_as_sfc("POINT (4327023 3208977)", crs = 3035),
                               "gb" ~ st_as_sfc("POINT (3524941 3266226)", crs = 3035),
                               "us" ~ st_as_sfc("POINT (2647988 2949289)", crs = 3035),
                               "cn" ~ st_as_sfc("POINT (5139684 1564535)", crs = 3035),
                               .default = geometry),
         lon = st_coordinates(geometry)[,1],
         lat = st_coordinates(geometry)[,2],
         atx = austria$X,
         aty = austria$Y) 


eumap |> ggplot() + 
  geom_sf(fill = "gray20", linewidth = 0.07, alpha = 0.2) +
  geom_curve(aes(x = atx, y = aty, xend = lon, yend = lat, linewidth = share),
             curvature = 0.2, data = partners) +
  geom_flag(aes(x = X, y = Y, country = iso2c), size = 9, data = austria) +
  geom_flag(aes(x = lon, y = lat, country = iso2c), size = 7, data = partners) +
  geom_label(data = partners |> filter(iso2c %in% c("us","gb","de","cz","pl")),
             aes(x = lon, y = lat, 
                 label = glue::glue("{rank}. {toupper(iso2c)}\n({share}%)")), 
             size = 3, lineheight = 0.9, 
             family = "Barlow Condensed", vjust = 0.5, nudge_y = 1.6e5) +
  geom_label(data = partners |> filter(iso2c %in% c("fr","ch","it","cn","hu")),
             aes(x = lon, y = lat, 
                 label = glue::glue("{rank}. {toupper(iso2c)}\n({share}%)")), 
             size = 3, lineheight = 0.9, 
             family = "Barlow Condensed", vjust = 0.5, nudge_y = - 1.6e5) +
  annotate("richtext", x = 3277294, y = 1653597, hjust = 0.5, vjust = 0.5,
           label = "<span style='font-size:12pt;color:firebrick;font-family:\"Roboto Condensed\";'>**TOP 10 EXPORT-ZIELE ÖSTERREICHS**</span><br><br><span style='font-size:9pt;font-family:\"Barlow Condensed\";'>Rund zwei Drittel der Warenexporte Österreichs gingen 2024 an<br>EU-Länder. Nach Deutschland (29,7%) waren die USA mit 8,5%<br>Exportanteil aber der zweitwichtigste Exportpartner.</span><br><span style='font-size:7pt;font-family:\"Barlow Condensed\";'>Daten: Statistik Austria. Grafik: @matschnetzer</span>", lineheight = 1, label.padding=unit(0.5, "lines"),
           fill = alpha("white", 0.7)) +
  scale_linewidth_continuous(range = c(0.5, 6),
                        breaks = c(2, 5, 10, 20), limits = c(2, 30)) +
  coord_sf(xlim = c(2577294, 5500000), ylim = c(1413597, 3828510)) +
  theme_void() +
  theme(plot.background = element_rect(color = NA, fill = "aliceblue"),
        legend.position = "none")

ggsave("25_03_Exporte.png", width = 6, height = 5, dpi = 320, bg = "white")
