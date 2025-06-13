library(tidyverse)
library(readODS)
library(treemapify)
library(ggtext)
library(showtext)

# Load Fonts
font_add("fa-solid", "fontawesome/otfs/Font Awesome 6 Free-Solid-900.otf")
font_add_google("Barlow Condensed", family = "Barlow Condensed")
showtext_auto()
showtext_opts(dpi = 320)

# Read ODS file
ods <- "https://www.statistik.at/fileadmin/pages/233/Staatsausgaben_nach_Abteilungen_Prozentaufteilung_2021-2024_Maerz_2025.ods"
tf <- tempfile()
download.file(ods, dest = tf)
raw <- read_ods(tf, skip = 1, n_max = 12)
unlink(tf)


# Read and edit PDF file
fintab <- raw |> 
  select(cofog = `COFOG-Abteilungen`, share = `2024`) |> 
  drop_na() |> 
  mutate(class = case_when(
    str_starts(cofog, "01|02|03") ~ "Verwaltung, Sicherheit, Zinsen",
    str_starts(cofog, "04|05") ~ "Wirtschaft, Infrastruktur und Umwelt",
    str_starts(cofog, "06|10") ~ "Soziale Sicherung, Familie und Wohnen",
    str_starts(cofog, "07") ~ "Gesundheit",
    str_starts(cofog, "08|09") ~ "Bildung, Forschung, Kunst und Kultur"
  )) |> 
  summarise(share = sum(share), .by = class) |> 
  mutate(class = fct_reorder(class, share, .desc = T)) |> 
  arrange(class)


# Specify icons
icon_dat <- fintab |> 
  treemapify(area = "share", start = "topleft") |> 
  mutate(logo = c(
    "<span style='font-family:fa-solid'>&#xf4c4;</span><span style='font-size:15pt'> </span><span style='font-family:fa-solid'>&#xe537;</span>",
    "<span style='font-family:fa-solid'>&#xe05c;</span>",
    "<span style='font-family:fa-solid'>&#xf19c;</span>",
    "<span style='font-family:fa-solid'>&#xe5b4;</span>",
    "<span style='font-family:fa-solid'>&#xf19d;</span>"
  ))

# Final plot
fintab |>
  ggplot(aes(area = share, fill = class)) +
  geom_treemap(colour = "white", size = 5, start = "topleft") +
  geom_treemap_text(aes(label = glue::glue("{class}\n({round(share,1)}%)")),
                    family = "Barlow Condensed", size = 20, color = "white", reflow = T,
                    padding.x = unit(3, "mm"), padding.y = unit(3, "mm"), 
                    place = "topleft", start = "topleft") +
  geom_richtext(data = icon_dat, aes(x = xmax, y = ymin, label = logo),
                size = 30, hjust = 1, vjust = 0, nudge_x = -0.01, alpha = 0.3,
                inherit.aes = F, fill = NA, label.size = NA, color = "white") +
  scale_fill_manual(values = MetBrewer::met.brewer("Wissing", direction = -1)) +
  scale_x_continuous(limits = c(0, 1.0001), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none")

ggsave("staatsausgaben.png", width = 12, height = 6, dpi = 320, bg = "white")
