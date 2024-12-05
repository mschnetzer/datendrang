library(tidyverse)
library(ggsankey)
library(ggtext)
library(colorspace)

raw <- readxl::read_xlsx("endgueltiges_Ergebnis_Beschluss_Bundeswahlbehoerde_16102024.xlsx")

# Bevölkerungszahl (ink. U16) für Q1/24 von Statistik Austria
data <- raw |> filter(Gebietsname == "Österreich") |> 
  mutate(SONST = MFG+BGE+LMP+GAZA+KEINE) |> 
  select(Wahlberechtigt = Wahlberechtigte, `Wähler:innen` = Abgegebene, 
         Ungültig = Ungültige, Gültig = Gültige, 
         FPÖ, ÖVP, SPÖ, NEOS, GRÜNE, KPÖ, BIER, SONST) |> 
  mutate(across(everything(), as.numeric),
         Bevölkerung = 9158750,
         `Unter 16` = 1403981,
         `Nicht wahlberechtigt` = Bevölkerung - Wahlberechtigt - `Unter 16`,
         `Nichtwähler:innen` = Wahlberechtigt - `Wähler:innen`) |> 
  pivot_longer(everything(), names_to = "Kategorie", values_to = "Anzahl") |> 
  mutate(Anteil = Anzahl/9158750*100,
         Kategorie = factor(Kategorie))


df <- 
  tribble(~Insgesamt, ~Wahlberechtigt, ~Wähler, ~Gültig, ~Partei, ~Anteil,
  "Bevölkerung", "Unter 16", NA, NA, NA, data[[14,3]],
  "Bevölkerung", "Nicht wahlberechtigt", NA, NA, NA, data[[15,3]],
  "Bevölkerung", "Wahlberechtigt", "Nichtwähler:innen", NA, NA, data[[16,3]],
  "Bevölkerung", "Wahlberechtigt", "Wähler:innen", "Ungültig", NA, data[[3,3]],
  "Bevölkerung", "Wahlberechtigt", "Wähler:innen", "Gültig", "SPÖ", data[[7,3]],
  "Bevölkerung", "Wahlberechtigt", "Wähler:innen", "Gültig", "ÖVP", data[[6,3]],
  "Bevölkerung", "Wahlberechtigt", "Wähler:innen", "Gültig", "FPÖ", data[[5,3]],
  "Bevölkerung", "Wahlberechtigt", "Wähler:innen", "Gültig", "GRÜNE", data[[9,3]],
  "Bevölkerung", "Wahlberechtigt", "Wähler:innen", "Gültig", "NEOS", data[[8,3]],
  "Bevölkerung", "Wahlberechtigt", "Wähler:innen", "Gültig", "BIER", data[[11,3]],
  "Bevölkerung", "Wahlberechtigt", "Wähler:innen", "Gültig", "SONST", data[[12,3]],
  "Bevölkerung", "Wahlberechtigt", "Wähler:innen", "Gültig", "KPÖ", data[[10,3]])

dflong <- df |> make_long(Partei, Gültig, Wähler, Wahlberechtigt, Insgesamt, 
                        value = Anteil) |> 
  mutate(node = factor(node, levels = c("Bevölkerung", "Wahlberechtigt", "Unter 16", "Nicht wahlberechtigt", "Wähler:innen", "Nichtwähler:innen", "Gültig", "Ungültig","FPÖ","ÖVP", "SPÖ", "NEOS","GRÜNE","KPÖ","BIER","SONST")))

dflong <- dflong |> left_join(data, by = c("node" = "Kategorie"))

options(OutDec= ",")

dflong |> ggplot(aes(x = x, next_x = next_x, node = node, value = value, 
                 next_node = next_node, group = node)) +
  geom_sankey(aes(fill = node), flow.alpha = 1, node.alpha = 1, width = 0.15,
              smooth = 7, space = 8, type = "alluvial", linewidth = 0) +
  # WEISSE LABELS
  geom_sankey_label(aes(label = glue::glue("{node}\n{round(Anzahl/1e6,1)} Mio.\n{round(Anteil,1)}%"),
                        color = ifelse(node %in% c("Gültig", "Wähler:innen", "Wahlberechtigt"), 
                                       "white", "transparent")), 
                   y = 2, type = "alluvial", space = 8, hjust = 0, vjust = 0,
                   family = "Barlow Condensed", lineheight = 0.9,
                   position = position_nudge(x = -0.05),
                   fill = NA, label.size = 0, size = 3.5,
                   data = dflong |> filter(!x %in% c("Insgesamt","Partei")) |> drop_na()) +
  # UNTER 16 + NICHT WAHLBERECHTIGT
  geom_sankey_label(aes(y = node, label = glue::glue("{node}\n{round(Anzahl/1e6,1)} Mio.\n{round(Anteil,1)}%"),
                        color = ifelse(node %in% c("Unter 16", "Nicht wahlberechtigt"), "black", "transparent")), 
                    type = "alluvial", space = 8, hjust = 0, vjust = 1,
                    family = "Barlow Condensed", lineheight = 0.9,
                    position = position_nudge(x = -0.07, y = -8),
                    fill = NA, label.size = 0, size = 3,
                    data = dflong |> filter(x == "Wahlberechtigt") |> drop_na()) +
  # NICHT-WÄHLER:INNEN
  geom_sankey_label(aes(y = node, label = glue::glue("{node}\n{round(Anzahl/1e6,1)} Mio.\n{round(Anteil,1)}%"),
                        color = ifelse(node == "Nichtwähler:innen", "black", "transparent")), 
                    type = "alluvial", space = 8, hjust = 0, vjust = 1,
                    family = "Barlow Condensed", lineheight = 0.9,
                    position = position_nudge(x = -0.07, y = -9),
                    fill = NA, label.size = 0, size = 3,
                    data = dflong |> filter(x == "Wähler") |> drop_na()) +
  # UNGÜLTIG
  geom_sankey_label(aes(y = node, label = glue::glue("{node}\n{round(Anzahl/1e3,0)} Tsd.\n{round(Anteil,1)}%"),
                        color = ifelse(node == "Ungültig", "black", "transparent")), 
                    type = "alluvial", space = 8, hjust = 0, vjust = 1,
                    family = "Barlow Condensed", lineheight = 0.9,
                    position = position_nudge(x = -0.07, y = -1),
                    fill = NA, label.size = 0, size = 3,
                    data = dflong |> filter(x == "Gültig") |> drop_na()) +
  # PARTEIEN
  geom_sankey_label(aes(y = node, label = glue::glue("{node}\n{scales::label_number(scale=1/1e3, accuracy=1, big.mark='.', suffix='T')(Anzahl)}\n{round(Anteil,1)}%")), 
                    type = "alluvial", space = 8, vjust = 1,
                    family = "Barlow Condensed", color = "black", lineheight = 0.9,
                    position = position_nudge(x = -0.1),
                    fill = NA, label.size = 0, size = 3,
                    data = dflong |> filter(x == "Partei") |> drop_na()) +
  # BEVÖLKERUNG
  geom_sankey_label(aes(label = glue::glue("{node} in Österreich: {round(Anzahl/1e6,2)} Mio.")), 
                    y = 2, type = "alluvial", space = 8, hjust = 0,
                    family = "Barlow Condensed", color = "white",
                    fill = NA, label.size = 0, size = 3.5,
                    data = dflong |> filter(x == "Insgesamt")) +
  geom_linerange(aes(xmin = x, xmax = x, ymin = 0, ymax = Inf), 
               color = "white", linewidth = 1.5, position = position_nudge(x = -0.07),
               data = dflong |> filter(x != "Partei")) +
  # TITEL
  annotate("text", x = 2.35, y = 112, size = 6, color = "black", hjust = .5, vjust = 0,
           family = "Barlow Condensed", label = "WAHL 2024") +
  annotate("text", x = 2.3, y = 112, size = 4, color = "black", hjust = .5, vjust = 1,
           family = "Barlow Condensed", lineheight = 0.9,
           label = "Endgültiges Endergebnis\n laut Bundeswahlbehörde") +
  annotate("text", x = 2.05, y = 112, size = 3, color = "black", hjust = .5, vjust = 1,
           family = "Barlow Condensed", lineheight = 0.9,
           label = "Daten: BMI. Grafik: @matschnetzer") +
  scale_color_identity() +
  scale_fill_manual(values = c("ÖVP" = "#63c3d0", "SPÖ" = "#ff0000", 
                               "FPÖ" = "#0066ff", "GRÜNE" = "#92d050",
                               "NEOS" = "#e84188", "BIER" = "#ffed00",
                               "KPÖ" = "#e60000", "SONST" = "#a1a1a1",
                               "Bevölkerung" = darken("deepskyblue3", 0.4),
                               "Wahlberechtigt" = darken("deepskyblue3", 0.2),
                               "Unter 16" = "gray50",
                               "Nicht wahlberechtigt" = "gray50",
                               "Wähler:innen" = "deepskyblue3",
                               "Nichtwähler:innen" = "gray70",
                               "Gültig" = lighten("deepskyblue3", 0.2),
                               "Ungültig" = "gray90"),
                    na.value = "transparent") +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_discrete(expand = c(0.07, 0.07)) +
  labs(x = NULL, y = NULL) +
  coord_flip() +
  theme_minimal(base_family = "Barlow Condensed") +
  theme(panel.grid = element_blank(),
        plot.margin = margin(l = 0.1, t = -1, unit = "cm"),
        legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 20, family = "Fira Sans",
                                  hjust = .5,
                                  margin = margin(b = 1, unit = "lines")),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

ggsave("25_01_Demokratie.png", width = 5, height = 9, dpi = 320, bg = "white")
 
