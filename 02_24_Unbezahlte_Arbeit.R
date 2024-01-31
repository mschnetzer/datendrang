library(tidyverse)
library(readODS)
library(ggchicklet)
library(colorspace)
library(patchwork)

# Load data into temp file
url <- "https://www.statistik.at/fileadmin/pages/298/Durchschnittliche_Zeitverwendung_2021-22.ods"
tempf <- tempfile()
download.file(url, dest = tempf, mode = "wb")

# Read data for working days and prepare variables
rawdata <- 
  read_ods(tempf, sheet = "Tabelle_2", range = "A5:J12", col_names = F, 
           col_types = "ctcttcttct") |> 
  janitor::clean_names() |> 
  mutate(across(c(x3, x6, x9), ~as.numeric(str_replace_all(., ",", "."))),
         x1 = case_match(x1, 
                         "Essen und andere persönliche Tätigkeiten" ~ "Essen",
                         "Sorgearbeit in Haushalt und Familie" ~ "Sorgearbeit",
                         "Soziale Kontakte und Freizeit" ~ "Freizeit",
                         "Nicht näher bestimmte Zeitverwendung" ~ "Anderes",
                         .default = x1),
         x1 = fct_inorder(x1))

# Select variables and add shares variable 
plotdata <- rawdata |> 
  select(activity = x1, female = x2, male = x5) |> 
  mutate(across(.cols = -activity, 
                .fns = ~period_to_seconds(hms(.))/period_to_seconds(days(1))*100, 
                .names = "{.col}_perc"))

# Aggregate activities and convert time variable into period
reduceddata <- plotdata |> 
  mutate(cat = case_when(
    activity %in% c("Schlafen", "Essen") ~ "Schlafen, Essen, u.a.",
    activity %in% c("Freiwilligentätigkeiten", "Aus- und Weiterbildung", "Freizeit", "Anderes") ~ "Freizeit, Ausbildung, u.a.",
    TRUE ~ activity),
  cat = fct_relevel(fct_inorder(cat), "Sorgearbeit", after = 2)) |> 
  summarise(across(!where(is.factor), sum), .by = "cat") |> 
  mutate(labpos = c("3 hours", "9 hours", "15 hours", "21 hours")) |> 
  mutate(across(c(female, male, labpos), as.period))

# Create chart
reduceddata |> pivot_longer(cols = c(female, male), 
                            names_to = "gender", values_to = "values") |>
  ggplot(aes(x = gender, y = values, fill = cat)) +
  geom_chicklet(width = 0.7, radius = unit(5, "pt"),
                position = position_stack(reverse = T)) +
  geom_text(aes(label = format(parse_date_time(values, "HMS"), "%H:%M")), 
            family = "Digital-7", color = "white", size = 6.5,
            position = position_stack(vjust = 0.5, reverse = T)) + 
  scale_fill_manual(values = c("#802417", "#CCA560", "#646e3b", "#508ea2")) +
  scale_y_time(labels = scales::label_timespan(unit = "hours", locale = "de_DE"), 
               breaks = scales::date_breaks("4 hours"),
               expand = c(0.03, 0.05)) +
  scale_x_discrete(expand = c(0.2,0.2), labels = c("Frauen", "Männer")) +
  guides(fill = guide_legend(title = NULL, nrow = 1,
                             label.position = "top", label.vjust = -13, 
                             keywidth = unit(7, "lines"), 
                             keyheight = unit(3, "lines"),
                             label.theme = element_text(size = 12, lineheight = 0.8,
                                                        color = "white", 
                                                        margin = margin(l = 4, r = 4),
                                                        family = "Roboto Condensed"))) +
  coord_flip() +
  labs(x = NULL, y = NULL,
       title = "Unbezahlte Sorgearbeit ist weiblich",
       subtitle = "Durchschnittliche Zeitverwendung pro Werktag für zusammengefasste Hauptaktivitäten",
       caption = "Quelle: Zeitverwendungserhebung 2021/22, Statistik Austria.") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.3),
        plot.title.position = "plot",
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 12,
                                     margin = margin(b = -0.7, unit = "lines")),
        plot.caption = element_text(size = 9, margin = margin(t = 2, unit = "lines")),
        axis.text.y = element_text(size = 16, color = "grey10"),
        legend.position = "top",
        legend.margin = margin(b = -.3, unit = "cm"))

ggsave("02_24_Unbezahlte_Arbeit.png", width = 8, height = 3.5, dpi = 320, bg = "white")