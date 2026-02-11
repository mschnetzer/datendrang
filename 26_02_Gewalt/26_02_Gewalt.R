library(tidyverse)
library(ggtextures)
library(magick)
library(ggimage)


# Daten unter https://www.statistik.at/statistiken/bevoelkerung-und-soziales/kriminalitaet-und-sicherheit/gewalt-gegen-frauen
gbv <- read_csv2("26_02_Gewalt.csv", n_max = 5)

mapgif <- image_read("isotype_00010.gif")
img <- image_read("isotype_00578.gif") |> 
  image_map(mapgif) |> image_crop(geometry = "100x390+0+0") |> image_flop()
backgr <- image_read("isotype_00578.gif") |> 
  image_flatten("Modulate") |> 
  image_fill(point = "+30+80", color = "gray80", fuzz = 50) |> 
  image_crop(geometry = "100x390+0+0") |> image_flop()


plotdf <- gbv |> 
  select(Merkmal, Anteil = last_col()) |>
  mutate(Merkmal = fct_inorder(Merkmal),
    Anteil_10 = Anteil / 10,
    label = glue::glue("{round(Anteil,0)}% = Jede {round(100/Anteil,0)}. Frau"), 
    bg = 10,
    image = list(img),
    bgimg = list(backgr))

plotdf |> 
  ggplot(aes(x = fct_rev(Merkmal))) +
  geom_isotype_col(aes(y = bg, image = bgimg),
                  img_width = grid::unit(1, "native"),
                  img_height = grid::unit(.9, "native"),
                  ncol = NA, nrow = 1,
                  hjust = 0, vjust = 0.5) +
  geom_isotype_col(aes(y = Anteil_10, image = image),
                  img_width = grid::unit(1, "native"),
                  img_height = grid::unit(.9, "native"),
                  ncol = NA, nrow = 1, 
                  hjust = 0, vjust = 0.5) +
  geom_text(aes(label = label), y = 10.5, color = "gray20",
    hjust = 0, family = "Roboto Condensed", size = 4.5) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_y_continuous(limits = c(NA, 15)) +
  labs(title = "Gewalterfahrungen von Frauen",
    caption = "Quelle: Statistik Austria",
    x = NULL, y = NULL) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Roboto Condensed", size = 14, color = "gray20"),
        plot.title = element_text(family = "Cabinet Grotesk", hjust = 0.5, size = 20),
        plot.title.position = "plot",
        plot.caption = element_text(size = 9),
        panel.grid = element_blank())

ggsave("26_02_Gewalt.png", width = 6.5, height = 6, dpi = 320, unit = "in", bg = "white")
