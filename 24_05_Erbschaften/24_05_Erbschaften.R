library(tidyverse)
library(readxl)
library(ggtext)
library(geomtextpath)
library(patchwork)
library(ggchicklet)


load("24_05_Erbschaften.RData")

pens <-
erbdist |> ggplot(aes(x = Perzentil, y = `Durchschnittliche Erbschaft`)) +
  geom_bar(stat = "identity",
           aes(fill = case_when(Perzentil == 100 ~ "#7c291e", 
                                Perzentil %in% 91:99 ~ "#cf5e4e",
                                Perzentil %in% 51:90 ~ "#f1af3a",
                                TRUE ~ "#003967"))) +
  geom_segment(aes(y = 3e5, yend = `Durchschnittliche Erbschaft`+ 0.5e5,
                   xend = Perzentil), 
               linewidth = 0.3, arrow = arrow(length = unit(2, "pt"), type = "closed"),
               data = ~. |> filter(Perzentil %in% c(25, 50, 75))) +
  geom_label(aes(label = scales::number(after_stat(y), accuracy = 1000, 
                                        big.mark = ".", decimal.mark = ",", suffix=" €")),
             data = ~. |> filter(Perzentil %in% c(25, 50, 75)), 
             size = 2.3, family = "Roboto Condensed",
             label.padding = unit(0.15, "lines"), nudge_y = 3e5) +
  geom_label(aes(label = scales::number(after_stat(y)/1e6, suffix = " Mio. €",
                                        decimal.mark = ",", accuracy = 0.1)),
             data = ~. |> filter(Perzentil == 100), 
             size = 2.3, family = "Roboto Condensed",
             label.padding = unit(0.15, "lines"), nudge_y = 0.2e6) +
  scale_color_manual(values = c("#003967", "#f1af3a", "#cf5e4e", "#7c291e")) + 
  scale_y_continuous(labels = scales::number_format(scale = 1/1e6,suffix = " Mio. €")) +
  scale_x_continuous(expand = c(0.04,0.05)) +
  scale_fill_identity() +
  labs(x = "Durchschnittliche Erbschaft nach Perzentilen", y = NULL) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(legend.position = "none", 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.15),
        axis.text = element_text(size = 7, color = "gray60"),
        axis.title = element_text(size = 8))


anteile <- 
  erbdist |> mutate(Gruppe = cut(Perzentil, breaks = c(0,50,90,99,100), 
                               labels = c("Untere 50%", "Mittlere 40%", 
                                          "Nächste 9%", "Top 1%"))) |> 
  summarise(groupsum = sum(Erbschaftsvolumen), .by = Gruppe) |> 
  reframe(Gruppe, Anteil = groupsum/sum(groupsum)*100)

chicklet <- 
anteile |> mutate(Gruppe = fct_rev(Gruppe)) |> 
  ggplot(aes(x = 1, y = Anteil, fill = Gruppe)) +
  geom_chicklet(position = "stack", width = 1) +
  geom_text(aes(label = round(Anteil,0)), 
            family = "Roboto Condensed", color = "white", fontface = "bold",
            size = 4, position = position_stack(vjust = 0.5)) +
  geom_text(aes(x = 0.2, label = Gruppe, color = Gruppe),
                position = position_stack(vjust = 0.5),
                family = "Roboto Condensed", size = 3) +
  scale_fill_manual(values = c("#7c291e", "#cf5e4e", "#f1af3a", "#003967")) + 
  scale_color_manual(values = c("#7c291e", "#cf5e4e", "#f1af3a", "#003967")) +
  scale_x_continuous(expand = c(0.1, 0.1)) +
  coord_flip() + 
  labs(title = "Anteil am gesamten Erbvolumen in %") +
  theme_void(base_family = "Roboto Condensed") +
  theme(legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5, size = 10),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"),
        plot.background = element_rect(fill = "white", color = "gray30", 
                                       linewidth = 0))

pens + inset_element(chicklet, 
                     left = 0.2, bottom = 0.48, right = 0.9, top = 0.7, on_top = T) +
  plot_annotation(title = "Wie sind die Erbschaften in Österreich verteilt?") &
  theme(plot.title = element_text(family = "Roboto Condensed"))

ggsave("24_05_Erbschaften.png", width = 7, height = 4, dpi = 320, bg = "white")
