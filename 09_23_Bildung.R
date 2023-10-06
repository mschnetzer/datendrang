library(tidyverse)
library(MetBrewer)
library(ggalluvial)

raw <- tribble(~Eltern, ~Kinder, ~Anteil, ~Gesamt,
              "Pflichtschule", "Pflichtschule", 32.6, 409900,
              "Pflichtschule", "Lehre/BMS", 46.1, 409900,
              "Pflichtschule", "AHS/BHS", 14.4, 409900,
              "Pflichtschule", "Universität", 6.9, 409900,
              "Lehre/BMS", "Pflichtschule", 7.5, 1241600,
              "Lehre/BMS", "Lehre/BMS", 59.3, 1241600,
              "Lehre/BMS", "AHS/BHS", 17.3, 1241600,
              "Lehre/BMS", "Universität", 15.9, 1241600,
              "AHS/BHS", "Pflichtschule", 6.0, 294100,
              "AHS/BHS", "Lehre/BMS", 18.6, 294100,
              "AHS/BHS", "AHS/BHS", 41.9, 294100,
              "AHS/BHS", "Universität", 33.6, 294100,
              "Universität", "Pflichtschule", 3.6, 361200,
              "Universität", "Lehre/BMS", 15.6, 361200,
              "Universität", "AHS/BHS", 23.5, 361200,
              "Universität", "Universität", 57.3, 361200
              )

df <- raw |> mutate(Flow = 1:n()) |> 
  pivot_longer(cols = c(Eltern, Kinder), names_to = "Generation", values_to = "Bildung") |> 
  mutate(freq = Gesamt*Anteil/100,
         Bildung = factor(Bildung, levels = rev(c("Pflichtschule","Lehre/BMS","AHS/BHS","Universität"))),
         Generation = factor(Generation, levels = c("Kinder", "Eltern")))


df |> ggplot(aes(x = Generation, y = freq, stratum = Bildung, alluvium = Flow)) +
  geom_flow(aes(fill = Bildung), alpha = 0.8, width = 1/40, linewidth = 0.1, color = "black",
            aes.flow = "backward") +
  geom_stratum(aes(fill = Bildung), width = 0.05, color = "black", linewidth = 0.1, reverse = T) +
  geom_text(stat = "stratum", reverse = T, aes(label = after_stat(stratum), color = Bildung), 
            size = 4, family = "Roboto Condensed", nudge_x = 0.07,
            data = df |> filter(Generation == "Eltern")) +
  geom_text(stat = "stratum", reverse = T, aes(label = after_stat(stratum), color = Bildung), 
            size = 4, family = "Roboto Condensed", nudge_x = -0.07,
            data = df |> filter(Generation == "Kinder")) +
  scale_fill_manual(values = met.brewer("Egypt")) +
  scale_color_manual(values = met.brewer("Egypt")) +
  scale_x_discrete(expand = c(0.05,0.05)) +
  labs(x=NULL, y=NULL, caption = "Anm.: Höchste abgeschlossene Bildung von 25-44-Jährigen und deren Eltern\nQuelle: Bildung in Zahlen 2021/22, Statistik Austria",
       title = "Bildungspersistenz in Österreich") +
  coord_flip() +
  theme_minimal(base_family = "Roboto Condensed", base_size = 10) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_text(size = 18, margin = margin(b=1, unit = "lines")),
        plot.title.position = "plot",
        plot.caption = element_text(margin = margin(t=1, unit = "lines")),
        axis.text = element_text(size = 14),
        axis.text.x = element_blank())

ggsave(file = "09_23_Bildung.png", width = 7, height = 4, dpi=320, bg="white")
