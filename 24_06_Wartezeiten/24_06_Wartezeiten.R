library(tidyverse)
library(ggtext)

data <- tribble(~arzt, ~anzahl, ~icon, ~`2012`, ~`2024`,
                "HNO", 50, "hno", 2, 10, 
                "Urologie", 44, "uro", 9, 29,
                "Gynäkologie und Geburtsheilkunde", 50, "gyn", 8, 32,
                "Innere Medizin", 51, "inner", 12, 33,
                "Augenheilkunde", 50, "auge", 9, 44,
                "Neurologie", 27, "neuro", 33, 45,
                "Radiologie", 11, "radio", 32, 57) |> 
  mutate(arzt = fct_inorder(arzt))

fin <- data |> 
  pivot_longer(cols = 4:5, names_to = "jahr", values_to = "wartezeit")

fin |> 
  ggplot(aes(y = arzt, x = wartezeit)) +
  geom_line(aes(group = arzt), color = "gray90", linewidth = 5.5) +
  geom_point(shape = 15, size = 5, aes(color = jahr)) +
  geom_text(aes(label = wartezeit), size = 2.5, color = "white") +
  geom_richtext(aes(label = glue::glue("<img src='png/{icon}.png' height=20 /> {arzt}")),
                hjust = .5, label.size = 0, fill = "transparent", nudge_y = 0.2,
            family = "Barlow Condensed", size = 3.3, color = "black",
            data = fin |> 
              reframe(anzahl, icon, wartezeit = mean(wartezeit), .by = arzt) |> 
              slice_head(n = 1, by = arzt)) + 
  geom_text(aes(label = jahr, color = jahr), hjust = 0.5, nudge_y = 0.4,
            family = "Barlow Condensed", size = 3.3, fontface = "bold",
            data = fin |> filter(arzt == "Radiologie")) +
  scale_color_manual(values = c("#275D8E", "#DB3A2F")) +
  scale_x_continuous(breaks = seq(10,50,10),
                     labels = scales::label_number(suffix = " Tage")) +
  labs(x = NULL, y = NULL,
       title = "Wartezeiten auf Arzttermine in Wien sind <span style='color:#DB3A2F'>2024</span> deutlich länger als <span style='color:#275D8E'>2012</span>",
       subtitle = "Medianwartezeit in ausgewählten Fachrichtungen in Tagen",
       caption = "Daten: Ärztekammer Wien/Peter Hajek. Icons: Flaticon.com. Grafik: @matschnetzer") + 
  theme_minimal(base_family = "Barlow Condensed") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, color = "gray20"),
        axis.ticks.x = element_line(linewidth = 0.1, color = "gray20"),
        plot.title = element_markdown(face = "bold"),
        plot.caption = element_text(size = 7, color = "gray20",
                                    margin = margin(t = 0.3, unit = "cm")),
        legend.position = "none")

ggsave("wartezeiten.png", width = 8, height = 3.5, dpi = 640, bg = "white")
