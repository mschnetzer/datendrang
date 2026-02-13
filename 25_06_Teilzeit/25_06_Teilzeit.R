library(tidyverse)
library(MetBrewer)

# Daten von https://www.statistik.at/fileadmin/pages/318/KTH-Monitoringbericht_2023-24_Web-barrierefrei.pdf Seite 35 

df <- tribble(
  ~Geschlecht, ~Status, ~"<1", ~"1", ~"2", ~"3", ~"4", ~"5", ~"6-9", ~"10-14",
  "Väter", "Vollzeit", 85.5, 84.7, 84.4, 83.5, 83.4, 84.5, 84.9, 85.9,
  "Mütter", "Vollzeit", 9.4, 11.6, 9.8, 15.2, 15.8, 13.8, 21.6, 29.3,
  "Väter", "Teilzeit", 6.8, 8.8, 7.9, 7.5, 9.0, 8.6, 7.4, 5.9,
  "Mütter", "Teilzeit", 12.1, 32.7, 58.1, 60.2, 65.1, 66.5, 61.2, 54.3,
  "Väter", "Nicht erwerbstätig", 6.9, 5.3, 7.6, 9.0, 7.7, 6.9, 7.6, 8.2,
  "Mütter", "Nicht erwerbstätig", 27.7, 29.2, 27.7, 24.6, 19.0, 19.6, 17.2, 16.4,
) 

plotdf <- df |>
  bind_rows(df |> reframe(Status = "Karenz", across(where(is.numeric), ~100-sum(.)), 
                          .by = Geschlecht)) |> 
  pivot_longer(-c(Geschlecht, Status), names_to = "Alter", values_to = "Wert") |> 
  mutate(Alter = fct_inorder(Alter),
         Status = factor(Status, levels=c("Nicht erwerbstätig","Karenz","Teilzeit","Vollzeit")))

plotdf |> ggplot(aes(x=Alter, y=Wert, fill=Status, group=Status)) + 
  geom_area() +
  facet_wrap(~Geschlecht, strip.position = "top", dir = "h", scales = "fixed") +
  scale_fill_manual(name = NULL, values = met.brewer("Lakota"),
                    guide = guide_legend(keywidth = 0.5)) +
  theme_minimal(base_family = "Barlow Condensed", base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 9,
                                   margin = margin(t=-3, unit = "pt")),
        plot.caption.position = "plot",
        axis.title.x = element_text(hjust = 0.05, size = 12,
                                    margin = margin(t=7, unit = "pt"))) + 
  labs(y = NULL, x = "Alter des Kindes", caption="Quelle: Statistik Austria.")

ggsave("25_06_Eltern.png", dpi=320, width = 8, height = 4, bg = "white")
 


