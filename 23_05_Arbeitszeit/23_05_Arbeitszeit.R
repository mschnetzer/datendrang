library(tidyverse)
library(patchwork)
library(janitor)
library(colorspace)
library(ggtext)

# Load data
load("23_05_Arbeitszeit.RData")

# Plot 1: Donut chart with relative shares
plot1 <- data1 |> 
  ggplot(aes(x = 3, y = share, fill = status)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(y = share, label = paste(round(share,0),"%", sep = "")), fontface = "bold",
            col = "white", size = 2.7, position = position_stack(vjust = 0.5), 
            family = "Roboto Condensed") +
  scale_fill_manual(name = NULL, values = c("#275D8E", "#EAB33A", "#DB3A2F"),
          guide = guide_legend(keywidth = 0.3, keyheight = 0.6, nrow = 3, reverse = T), 
          labels = c("Wunsch zur Mehrarbeit",
                     "Kein Änderungswunsch",
                     "Wunsch zur Arbeitszeitverkürzung")) +
  coord_polar(theta="y") +
  facet_wrap(~gender, nrow = 2) +
  xlim(1.7,3.5) +
  theme_void() +
  theme(legend.position = c(0.5,-0.05),
        legend.text = element_text(size = 7, family = "Roboto Condensed", color = "gray40"),
        panel.spacing.y = unit(1.5, "lines"), 
        strip.text = element_text(family = "Roboto Condensed", color = "gray40", size = 16))


# Data manipulation for dumbbell chart
dumbbell <- data2 |> mutate(labpos = (meanst + meanwu)/2, labdiff = meanwu - meanst)

facetlab <- c("Personen mit Wunsch<br><span style='color:#DB3A2F'><b>zur Arbeitszeitverkürzung</b></span>",
        "Personen <br><span style='color:#EAB33A'><b>ohne Veränderungswunsch</b></span>",
        "Personen mit Wunsch<br><span style='color:#275D8E'><b>zur Mehrarbeit</b></span>")
names(facetlab) <- c("decr","keep","incr")

description <- tribble(~status, ~gender, ~xpos, ~ypos, ~label,
      factor("decr"), factor("Männer"), 28, 4, "Kreise zeigen die\ngewünschte Arbeitszeit",
      factor("incr"), factor("Männer"), 29, 3, "Quadrate zeigen die\ntatsächliche Arbeitszeit",
      factor("keep"), factor("Männer"), 35, 2, "Hier sind gewünschte und\ntatsächliche Arbeitszeit gleich")


# Plot2: Work time preferences by gender and age of children
plot2 <- data2 |> pivot_longer(meanst:meanwu, names_to = "concept", values_to = "values")  |> 
  ggplot(aes(x = values, y = kindcat)) +
  geom_segment(data = dumbbell, aes(x = 0, xend = meanst, y = kindcat, yend = kindcat),
               color = "grey93", linewidth = 6, alpha = 0.4) +
  geom_segment(data = dumbbell, aes(x = meanwu, xend = meanst, y = kindcat, yend = kindcat),
               color = "grey70", linewidth = 6) +
  geom_segment(data = dumbbell, aes(x = meanst, xend = meanst + 5*sign(meanwu-meanst), 
                            y = kindcat, yend = kindcat), color = "white", linewidth = 0,
               arrow = arrow(length = unit(9, "pt"), type="closed")) +
  geom_text(data = description, aes(x = xpos, y = ypos, label = label), 
            size = 2.3, color = "black", family = "Roboto Condensed", hjust = 1, 
            lineheight = 0.9) +
  geom_point(aes(color = factor(status), shape = concept, size = concept)) + 
  geom_text(aes(label = round(values,0)), size = 2.2, color = "white", fontface = "bold",
            family = "Roboto Condensed") + 
  scale_color_manual(name = NULL, values = c("#DB3A2F", "#EAB33A", "#275D8E")) +
  scale_shape_manual(values = c(15,19)) +
  scale_size_manual(values = c(5.8, 5.5)) +
  scale_x_continuous(limits = c(0, 47), breaks = seq(0, 50, 10), 
                     labels = scales::number_format(suffix="h")) +
  facet_grid(gender~status, labeller = labeller(status = facetlab)) +
  labs(x= "Durchschnittliche Arbeitszeit in Wochenstunden ➔", y=NULL) +
  theme_minimal() +
  theme(strip.text.y = element_blank(),
        strip.text.x = element_markdown(size = 9, hjust = 0.5, family = "Roboto Condensed",
                        lineheight = 1.1, margin = margin(b=3, unit="pt")),
        axis.text = element_text(size = 7, family = "Roboto Condensed"),
        axis.title.y=element_text(angle=0, vjust=1, hjust=1, size = 7,
                                  family = "Roboto Condensed", color = "gray30",
                                  margin = margin(r = -30, b = 10, unit = "pt")),
        axis.title.x = element_text(size = 8, family = "Roboto Condensed", color = "gray30",
                                  margin = margin(t=7, unit="pt"), hjust = 0),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.2),
        panel.grid.minor.x = element_blank(),
        panel.spacing.y = unit(1.5, "lines"), 
        legend.position = "none")


total <- plot1 + plot2 + plot_layout(widths = c(0.25, 0.75)) +
  plot_annotation(caption = "Daten: Mikrozensus 2022, Statistik Austria. Grafik: @matschnetzer") & 
  theme(plot.caption = element_text(size = 5, color = "gray40", family = "Robtoto Condensed"))

ggsave(total, file = "23_05_Arbeitszeit.png", width = 8.5, height = 4, dpi = 600)
