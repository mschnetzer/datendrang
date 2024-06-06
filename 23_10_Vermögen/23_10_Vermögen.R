library(tidyverse)
library(sf)
library(waffle)
library(MetBrewer)
sf_use_s2(FALSE)

geodat <- st_read("https://raw.githubusercontent.com/ginseng666/GeoJSON-TopoJSON-Austria/master/2021/simplified-99.5/gemeinden_995_geo.json", quiet=TRUE, stringsAsFactors=FALSE) |> 
  mutate(
    center = map(geometry, st_centroid),
    centercoord = map(center, st_coordinates),
    ccordx = map_dbl(centercoord, 1),
    ccordy = map_dbl(centercoord, 2)
  ) |> 
  mutate(iso = as.numeric(iso))

geodat$area <- st_area(geodat)
total <- sum(geodat$area)
arbreaks <- total*c(0, 0.389, 0.657, 0.972, 1)

ardf <- geodat |> arrange(ccordx) |> mutate(csarea = cumsum(area)) |>  
  mutate(gruppe = cut(csarea, breaks=arbreaks))

## COLOR

ardf |> ggplot() +
  geom_sf(aes(fill=gruppe), color="black", linewidth=0.06) +
  geom_sf(fill="transparent", color="black", linewidth = 0.3, 
          data = ardf |> group_by(gruppe) |> summarise()) +
  coord_sf(datum=NA) +
  expand_limits(x=19) + 
  annotate("text",label="Dem Top 1% würden fast\n 40% der Fläche gehören", size=2.5, hjust=0.5, x=11, y=48.3) +
  annotate("text",label="Die reichsten 2-10% würden rund\n ein Viertel der Fläche besitzen", size=2.5, hjust=0.5, x=13, y=49.2) +
  annotate("text",label="Die nächsten 40% hätten\n etwa ein Drittel der Fläche", size=2.5, hjust=0.5, x=16, y=46.2) +
  annotate("text",label="Die ärmere Hälfte\n der Bevölkerung\n teilt sich 2,8% der Fläche", size=2.5, hjust=0.5, x=17.6, y=47) +
  geom_curve(aes(x=11.5,xend=12, y=48.1,yend=47.7), curvature = -0.1, ncp=8, linewidth=0.1, arrow=arrow(length=unit(0.01, "npc"), type="closed")) +
  geom_curve(aes(x=14,xend=14.5, y=49,yend=48.7), curvature = -0.1, ncp=8, linewidth=0.1, arrow=arrow(length=unit(0.01, "npc"), type="closed")) +
  geom_curve(aes(x=15.5,xend=15.7, y=46.4,yend=46.65), curvature = 0.1, ncp=8, linewidth=0.1, arrow=arrow(length=unit(0.01, "npc"), type="closed")) +
  geom_curve(aes(x=17.5,xend=17.2, y=47.3,yend=47.9), curvature = 0.1, ncp=8, linewidth=0.1, arrow=arrow(length=unit(0.01, "npc"), type="closed")) +
  scale_fill_manual(values = met.brewer("Egypt", direction = -1), 
        labels = c("Top 1%","Reichste 2-10%","Nächste 40%","Untere 50%")) +
  theme_minimal(base_family = "Roboto Condensed") +
  labs(title="Wenn Österreich so verteilt wäre wie Vermögen...",
       caption = "Quelle: HFCS 2017, Heck u.a. (2020)", x=NULL, y=NULL) +
  theme(legend.title = element_blank(), 
        legend.position = "top",
        legend.key.size = unit(0.3 , "cm"),
        plot.title = element_text(margin = margin(b=0.5,unit="cm"), family = "Playfair Display", 
                                  size = 14, hjust = 0.5),
        plot.caption = element_text(size = 5),
        legend.text = element_text(size = 8))

ggsave("10_23_Vermögen.png", dpi=320, width = 6.5, height = 4, bg = "white")



### Waffles
faclevel <- c("Untere 50%","Nächste 40%","Reichste 2-10%","Top 1%")

tibble(labels=factor(faclevel,levels=rev(faclevel)),
       'Bevölkerungsanteil'=c(50,40,9,1),
       'Vermögensanteil'=c(3,31,27,39)) -> dfpar
plotdf <- dfpar |> pivot_longer(cols = 2:3, names_to = "cat", values_to = "value")

ggplot(plotdf, aes(fill = labels, values = value)) +
  geom_waffle(color = "white", size = 1.125, n_rows = 5, na.rm = T) +
  facet_wrap(~cat, ncol = 1) +
  scale_fill_manual(values = met.brewer("Johnson")[c(1,3,4,5)]) +
  coord_equal(expand = FALSE) +
  labs(
    title = "Verteilung der Nettovermögen in Österreich",
    caption="Quelle: HFCS 2017, Heck u.a. (2020)"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(strip.text.x = element_text(size = 12, margin=margin(b=5, t=5), hjust = 0),
        plot.caption = element_text(margin = margin(t=4)),
        plot.title = element_text(margin = margin(b=6), family = "Playfair Display"),
        legend.title = element_blank()) +
  theme_enhance_waffle()
ggsave("23_10_Vermögen.png", dpi=320, width = 8, height = 4, bg = "white")

