librarian::shelf(tidyverse, janitor, ggbump, MetBrewer)

# Load inflation data
load("06_23_Inflation.RData")

# Set local language for month names
Sys.setlocale(locale = "de_AT")

# Data wrangling and renaming
plotdat <- inflation |>
  drop_na() |> 
  rename(Datum=`Zeitraum der Erhebung`) |> 
  mutate(Datum = str_replace_all(Datum, "Jän", "Jan"),
         Datum = dmy(paste("01", Datum), locale = "de_DE")) |> 
  pivot_wider(names_from = "Coicop 5-Steller", values_from = "Einfluss Vorjahresmonat") |> 
  clean_names() |> 
  mutate(`Wohnen &\n Haushalt` = x04_wohnung_wasser_energie - x04_5_aufwand_fur_energie +
           x05_hausrat_instandhaltung_des_hauses) |> 
  select(Datum = datum,
         Nahrungsmittel = x01_1_nahrungsmittel,
         `Wohnen &\n Haushalt`,
         Energie = x04_5_aufwand_fur_energie,
         Verkehr = x07_verkehr,
         `Freizeit & Kultur` = x09_freizeit_und_kultur,    
         `Restaurants &\n Hotels` = x11_restaurants_und_hotels,
         `Alkholische Getränke & Tabak` = x02_alkoholische_getranke_und_tabak,
         `Bekleidung & Schuhe` = x03_bekleidung_und_schuhe,
         `Gesundheitspflege` = x06_gesundheitspflege,
         `Nachrichtenübermittlung` = x08_nachrichtenubermittlung,
         `Erziehung & Unterricht` = x10_erziehung_und_unterricht,
         `Verschiedene Waren, Dienstleistungen` = x12_verschiedene_waren_dienstleistungen
  )  |> 
  mutate(Inflation = rowSums(across(where(is.numeric)))) |> 
  pivot_longer(cols = -Datum, names_to = "Coicop", values_to = "Beitrag")

# Define data limits
firstdate <- as.Date("2022-04-01")
lastdate <- max(plotdat$Datum)

# Select and rank COICOP groups for each month
findat <- plotdat |> filter(!Coicop == "Inflation", 
                            !Coicop == "Verschiedene Waren, Dienstleistungen",
                            Datum >= firstdate) |> 
  mutate(rank = rank(-Beitrag, ties.method = "random"), .by = Datum) |> 
  mutate(intop = any(rank <= 5), .by = Coicop) |> 
  filter(intop == T)

# Plot data
findat |>  
  ggplot(aes(x = Datum, y = rank, color = Coicop)) +
  geom_point(aes(size = 2.5)) + 
  geom_text(data = findat |> slice_min(Datum, n=1), family = "Raleway",
            aes(x = Datum - days(10), label = Coicop), size = 3, hjust = 1) +
  geom_text(data = findat |> slice_max(Datum, n=1), family = "Raleway",
            aes(x = Datum + days(10), label = Coicop), size = 3, hjust = 0) +
  geom_bump(linewidth = 2, smooth = 8) +
  geom_point(data = findat |> slice_min(Datum, n = 1), size = 8) +
  geom_text(data = findat |> slice_min(Datum, n = 1), size = 3, color = "white", hjust = 0.5,
            aes(label = paste0(round(Beitrag/9.4*100, 0),"%")), family = "Roboto Condensed") +
  geom_point(data = findat |> slice_max(Datum, n = 1), size = 8) +
  geom_text(data = findat |> slice_max(Datum, n = 1), size = 3, color = "white", hjust = 0.5,
            aes(label = paste0(round(Beitrag/9.2*100, 0),"%")), family = "Roboto Condensed") +
  scale_color_manual(values = met.brewer("Lakota")) +
  scale_size_continuous(range = c(2, 6)) +
  scale_y_reverse(expand = c(0.1,0)) +
  scale_x_date(limits = c(as.Date("2022-02-20"), as.Date("2023-04-15")),
               breaks = seq(firstdate, lastdate, "1 month"), 
               date_labels = "%b '%y") +
  labs(x = NULL, y = NULL,
       title = "Was trägt zur hohen Inflation bei?",
       subtitle = "Anteile ausgewählter Gruppen an der Gesamtinflation",
       caption = "Daten: Statistik Austria") +
  theme_minimal(base_family = "Raleway") +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 5, unit = "pt"), size = 8),
        plot.title = element_text(margin = margin(b = 5, unit = "pt"), size = 15),
        plot.subtitle = element_text(margin = margin(b = 10, unit = "pt"), size = 11),
        plot.caption = element_text(margin = margin(t = 10, unit = "pt"), size = 5),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid = element_blank())

ggsave("06_23_Inflation.png", width = 8, height = 3.5, dpi = 320)
