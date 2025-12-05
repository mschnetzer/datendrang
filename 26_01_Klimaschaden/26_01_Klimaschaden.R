library(tidyverse)
library(ggtext)
library(ggchicklet)

# Daten: https://www.eea.europa.eu/en/analysis/indicators/economic-losses-from-climate-related
daten <- read_csv("26_01_Klimaschaden.csv", 
                  skip = 11, n_max = 29, col_names = F) |> 
  bind_rows(read_csv("26_01_Klimaschaden.csv", 
                     skip = 41, col_select = 2:5, col_names = F) |> 
              rename("X1" = "X2", "X2" = "X3", "X3" = "X4", "X4" = "X5"))

final <- daten |> 
  rename("Klimatologisch" = "X1",
         "Hydrologisch" = "X2",
         "Meteorologisch" = "X3",
         "Jahr" = "X4") |> 
  pivot_longer(cols = !Jahr, names_to = "Art", values_to = "Schaden") |> 
  mutate(Art = fct_inorder(Art))

Summe <- final |> summarise(Schaden = sum(Schaden), .by = Art)

final |> 
  ggplot(aes(x = Jahr, y = Schaden, fill = Art)) +
  geom_chicklet(radius = unit(2, "pt")) +
  #  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Klimatologisch" = "#DB3A2F",
                               "Hydrologisch" = "#275D8E",
                               "Meteorologisch" = "#EAB33A")) +
  annotate("text", label = str_wrap("Gesamtschaden 1980-2024 durch Hitzewellen, Dürren, etc.: 213 Mrd. €", 25), x = 1985, y = 50, size = 3.5, color = "#DB3A2F", lineheight = 0.9) +
  annotate("text", label = str_wrap("Schaden durch Hochwasser, Überflutung, etc.: 386 Mrd. €", 30), x = 1999, y = 47, size = 3.5, color = "#275D8E", lineheight = 0.9) +
  annotate("text", label = str_wrap("Schaden durch Sturm, Hagel, etc.: 224 Mrd. €", 25), x = 2013, y = 43, size = 3.5, color = "#EAB33A", lineheight = 0.9) +
  scale_y_continuous(labels = scales::label_number(suffix = " Mrd. €"),
                     breaks = seq(20,60,20),
                     expand = c(0.01,0)) +
  scale_x_continuous(expand = c(0.01,0)) +
  labs(x = NULL, y = NULL,
       title = "Ökonomischer Schaden in der EU durch die Klimakrise",
       subtitle = "Schaden durch <span style='color:#DB3A2F'>**klimatologische**</span>, <span style='color:#275D8E'>**hydrologische**</span> und <span style='color:#EAB33A'>**meteorologische**</span> Ereignisse",
       caption = "Daten: Europäische Umweltagentur") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(legend.position = "none",
        plot.title.position = "plot",
        plot.subtitle = element_markdown(),
        plot.caption = element_text(size = 9),
        panel.grid.major.y = element_line(linewidth = 0.2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

ggsave("26_01_Klimaschaden.png", width = 8, height = 6, dpi = 320, bg = "white")
