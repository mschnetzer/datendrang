library(tidyverse)
library(readxl)
library(eurostat)

raw <- read.csv("25_05_Prognose.csv")
gdp <- get_eurostat("tec00115", filters = list(geo = "AT", unit = "CLV_PCH_PRE"))


findat <- 
  raw |> filter(Institution == "Wifo", Land == "Österreich", Variable == "BIP real") |>
  select(Zieljahr, Prognosedatum, Wert) |> 
  arrange(Prognosedatum) |> 
  mutate(Prognosejahr = year(Prognosedatum),
         Wert = Wert * 100) |> 
  mutate(Quartal = factor(row_number()), .by = c(Prognosejahr, Zieljahr)) |> 
  left_join(gdp |> add_row(time = as.Date("2025-01-01"), values = NA) |> 
              mutate(Zieljahr = year(time), Lastreal = lag(values)) |> 
              select(Zieljahr, Real = values, Lastreal)) 

seldat <- findat |> filter(Zieljahr >= 2013) |> 
  filter(Zieljahr == Prognosejahr +1) |> 
  add_row(Zieljahr = 2013, Real = -0.3)

seldat |> 
  ggplot(aes(x = Zieljahr)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_segment(aes(x = Prognosejahr, xend = Zieljahr, y = Lastreal, yend = Wert,
                   color = Quartal), inherit.aes = F, linewidth = 0.6,
               arrow = arrow(type = "closed", ends = "last", length = unit(.01, "npc"))) +
  geom_line(data = seldat |> filter(!is.na(Real)),
            aes(y = Real), linewidth = 1, color = "black") +
  geom_point(aes(y = Real), color = "black", size = 1.5) +
  annotate("text", x = 2020.9, y = -4, hjust = 0, family = "Barlow Condensed", 
           size = 3.5, lineheight = 0.9, color = "black",
           label = "Tatsächliche\nBIP-Entwicklung") +
  annotate("text", x = 2014, y = -1, hjust = 0, family = "Barlow Condensed", 
           size = 3.5, lineheight = 0.9, color = "#902A57",
           label = str_wrap("WIFO-Prognose vom Winter 2014 für das BIP-Wachstum 2015.", 
                            30)) +
  annotate("curve", x = 2014.3, y = -0.4, xend = 2014.6, yend = 0.5, 
               linewidth = 0.2, color = "#902A57", curvature = -0.1) +
  annotate("curve", x = 2020.3, y = -3.8, xend = 2020.8, yend = -4, 
           linewidth = 0.2, color = "black", curvature = 0.1) +
  scale_color_manual(values = futurevisions::futurevisions("mars"),
                     labels = c("Frühjahr", "Sommer", "Herbst", "Winter"),
                     name = "Prognose",
                    na.translate = F) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_continuous(limits = c(2013,NA),
                     breaks = seq(2013, 2025, 2)) +
  labs(x = NULL, y = "Veränderung des realen BIP",
       title = "Prognostiziertes und tatsächliches Wirtschaftswachstum in Österreich",
       caption = "Quellen: WIFO, Eurostat") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(size = 16, 
                                  margin = margin(b = 0.8, unit = "lines")))

ggsave("25_05_Prognose.png", width = 8, height = 4, dpi = 320, bg = "white")
