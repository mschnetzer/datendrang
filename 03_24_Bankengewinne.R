library(tidyverse)
library(oenb)
library(colorspace)

data <- oenb_data(id = "3104", pos = "VDBKICBD66EL210") |> 
  select(period, value) |> 
  as_tibble()

aggquart <- data |> filter(str_detect(period, "-")) |> 
  separate(period, into = c("year", "quarter"), sep = "-") |> 
  mutate(quarter = str_remove(quarter, "Q"),
         across(c(year, quarter), as.numeric)) |> 
  filter(!year == 2014)

aggquart |> ggplot(aes(x = quarter, y = value, color = factor(year))) +
  geom_line(aes(group = year), linewidth = 2) +
  gghighlight::gghighlight(year %in% 2022:2023, line_label_type = "text_path",
                           label_params = list(size = 4, family = "Roboto Condensed",
                                               hjust = 0.85, vjust = -0.2),
                           unhighlighted_params = list(color = "grey80",linewidth = 1)) +
  geom_segment(x = 3, xend = 3, y = 5500, yend = 11500, linetype = "longdash",
               linewidth = 0.3) +
  geom_point(data = aggquart |> filter(year %in% 2022:2023, quarter %in% 3:4), 
             size = 9) +
  geom_point(data = aggquart |> filter(year %in% 2022:2023), size = 3) +
  geom_text(data = aggquart |> filter(year %in% 2022:2023, quarter %in% 3:4), 
            aes(label = scales::number(value, scale = 1/1e3, decimal.mark=",")), 
            size = 3, color = "white") +
  annotate("label", x = 3, y = 9000, hjust = 0.5, angle = 0,
           label = "+110%", size = 3.6, family = "Roboto Condensed") +
  annotate("text", x = 4.05, y = 4000, hjust = 1, angle = 270, color = "grey60", 
           label = "2015–2021", size = 4, family = "Roboto Condensed") +
  scale_color_manual(values = c("navyblue", "firebrick"),
                     guide = guide_none()) +
  scale_x_continuous(labels = scales::number_format(suffix = ". Quartal"),
                     expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::number_format(scale = 1/1000, suffix = " Mrd €")) +
  labs(x = NULL, y = NULL,
       title = "Rekordgewinne im Bankensektor",
       subtitle = "Aggregierte Gewinne (nach Steuern und Minderheitenanteilen) im Quartalsverlauf",
       caption = "Quelle: Oesterreichische Nationalbank. Grafik: @matschnetzer") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2),
        plot.title.position = "plot",
        plot.title = element_text(family = "Cabinet Grotesk", size = 18),
        plot.subtitle = element_text(family = "Cabinet Grotesk", size = 12, 
                                  margin = margin(b = 1, unit = "lines")),
        plot.caption = element_text(family = "Cabinet Grotesk", size = 8, 
                                    margin = margin(t = 2, unit = "lines")))

ggsave("03_24_Bankengewinne.png", width = 8, height = 4.5, dpi = 320, bg = "white")
