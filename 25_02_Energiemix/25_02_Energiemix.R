library(tidyverse)
library(ggstream)
library(MetBrewer)
library(colorspace)

# Data from https://oesterreichsenergie.at/fakten/energiegrafiken/detailseite/bruttostromerzeugung-in-oesterreich-1

raw <- read_csv2("25_02_Energiemix.csv") |> 
  filter(`Titel der Werte-Spalten` == "GWh") |> 
  mutate(Werte = as.numeric(Werte),
         Jahr = ymd(Jahr, truncated = 2L),
         Primärenergieträger = factor(Primärenergieträger, levels = rev(c("Kohle", "Erdöl", "Erdgas","Biogene Brennstoffe", "Speicherkraftwerke", "Laufkraftwerke", "Wind", "Photovoltaik")))) |> 
  mutate(Anteil = Werte/sum(Werte, na.rm =T)*100, .by = Jahr)

pal <- met.brewer("Lakota")[c(2,1,5,3)]
cols <- c(rbind(lighten(pal, 0.1), darken(pal, 0.2)))

raw |> filter(!is.na(Primärenergieträger)) |> 
  ggplot(aes(x = Jahr, y = Werte, fill = Primärenergieträger)) +
  geom_stream(type = "mirror") +
  scale_fill_manual(values = cols, name = "", 
                    guide = guide_legend(keywidth = 0.3, keyheight = 1)) +
  scale_x_date(position = "bottom") +
  labs(x = NULL, y = NULL, caption = "Quelle: E-Control. Grafik: @matschnetzer",
       title = "Bruttostromerzeugung in Österreich (in GWh)") +
  coord_cartesian(expand = F) +
  theme_minimal(base_family = "Futura") +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 6),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(size = 6),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.2),
        legend.text = element_text(size = 8),
        panel.grid.major.y = element_blank())

ggsave("25_02_Energiemix.png", dpi = 320, width = 8, height = 3.5, bg = "white")
