library(dplyr)
library(readr)
library(ggplot2)
library(ggthemes)
library(scales)
library(maps)
library(mapproj)
library(sp)
library(rgdal)
library(spatialEco)
library(tidyverse)
library(sf)
library(plotly)
library(Cairo)
library(ggmap)
library(leaflet)
library(crosstalk)
library(ggplot2)
library(ggthemes)
library(scales)
library(maps)
library(mapproj)
library(sp)
library(rgdal)
library(spatialEco)
library(tidyverse)
library(sf)
library(plotly)
library(Cairo)
library(ggspatial)
library(leaflet)
install.packages("ggspatial")



fires_wfu<-readOGR("C:/ncstate/NPS/southflorida/allfires/all_fires_cost.shp")

everWFMI<-read.csv("C:/ncstate/NPS/southflorida/adddates/ever_dates.csv", stringsAsFactors = F)
bicyWFMI<-read.csv("C:/ncstate/NPS/southflorida/adddates/BICY_dates.csv", stringsAsFactors = F)

WFMI<-rbind(everWFMI, bicyWFMI)


fires<-merge(fires, WFMI, by.x = "FireID", by.y = "FireId", all.x= T)


fires$StartDate<-as.character(fires$StartDate)
fires$ContrDate<-as.character(fires$ContrDate)
fires$StartDate<-as.Date(fires$StartDate, format="%Y%m%d", origin="1970-01-01")
fires$ContrDate<-as.Date(fires$ContrDate, format="%Y%m%d", origin="1970-01-01")
fires$duration<-difftime(fires$ContrDate, fires$StartDate, units="days")
fires$plot_date<-as.Date(fires$StartDate, format = "%Y%m%d")
fires$plot_date<-as.Date(format(fires$plot_date,"2017-%m-%d"))

fires<-subset(fires, GISAcres>1000)
fires<-spTransform(fires, '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

unique(fires$NEWCAT)

#leaflet try
pal<-colorFactor(c("#56B4E9", "#ffff00", "#FF5733", "#d397fc"), fires$NEWCAT)

fires<-st_as_sf(fires)

share<-SharedData$new(fires)
bscols(
leaflet(share) %>%
  addProviderTiles(providers$Esri.WorldImagery, options=providerTileOptions(opacity = 0.5)) %>%
  addPolygons(fillColor = ~pal(NEWCAT), weight = 1, smoothFactor = 0.5,
                          opacity = 0.7, fillOpacity = 0.5, color='gray') %>% 
  highlight(dynamic = TRUE, persistent = TRUE, color=c('#e98756', '#ff00ff', '#33ffc2', '#fce897')),
ggplotly(ggplot(share, aes(y=FIRE_YEAR)) +
           geom_hline(yintercept = seq(1992, 2015, by = 1), color = "gray", size = 0.04) +
           scale_size_area(max_size = 15, guide = FALSE) +
           scale_x_date(date_breaks = "months", date_labels = "%b") +
           scale_y_reverse(limits = c(2015,1992), breaks = c(2010,2005,2000,1995)) +
           xlab("") +
           ylab("") +
           theme(plot.background = element_rect(fill='gray20'), 
                 panel.background = element_rect(fill='gray20'), 
                 axis.text.y = element_text(colour='gray98', size=16),
                 axis.text.x = element_text(colour='gray98', size=10),
                 panel.grid.major = element_blank(),   
                 panel.grid.minor = element_blank(),
                 panel.grid.major.y = element_line(size=1.2, color="gray"),
                 legend.position='none',
                 axis.ticks.x = element_line(colour='gray98')) +
           geom_point(aes(size = GISAcres*2, x = plot_date, color = NEWCAT, group=1, text=paste('Name:', IncidentNa, '<br>Type:', NEWCAT, '<br>Date:', format(StartDate, "%b %d %Y"), '<br>Duration:', duration, "days")), alpha=0.5) +
           scale_color_manual(values=c("#56B4E9", "#ffff00", "#FF5733", "#d397fc")), tooltip="text") %>%
  highlight("plotly_selected", persistent = TRUE, color=c('#e98756', '#ff00ff', '#33ffc2', '#fce897')),
widths=c(6,6))
  



ggplot(fires, aes(y=FIRE_YEAR)) +
           geom_hline(yintercept = seq(1992, 2015, by = 1), color = "gray", size = 0.04) +
scale_size_area(max_size = 15, guide = FALSE) +
scale_x_date(date_breaks = "months", date_labels = "%b") +
scale_y_reverse(limits = c(2015,1992), breaks = c(2010,2005,2000,1995)) +
xlab("") +
ylab("") +
theme(plot.background = element_rect(fill='gray20'), 
panel.background = element_rect(fill='gray20'), 
axis.text.y = element_text(colour='gray98', size=16),
axis.text.x = element_text(colour='gray98', size=10),
panel.grid.major = element_blank(),   
panel.grid.minor = element_blank(),
panel.grid.major.y = element_line(size=1.2, color="gray"),
legend.position='none',
axis.ticks.x = element_line(colour='gray98')) +
geom_point(aes(size = GISAcres*2, x = plot_date, color = NEWCAT, group=1, text=paste('Name:', IncidentNa, '<br>Type:', NEWCAT, '<br>Date:', format(StartDate, "%b %d %Y"), '<br>Duration:', duration, "days")), alpha=0.5) +
scale_color_manual(values=c("#56B4E9", "#ffff00", "#FF5733", "#d397fc"))


bscols(
  ggplotly(
    ggplot(share) + 
      geom_sf(aes(fill=NEWCAT, group=1, text=paste('Name:', IncidentNa, '<br>Type:', NEWCAT, '<br>Date:', format(StartDate, "%b %d %Y"), '<br>Duration:', duration, "days")), alpha=0.5) +
      xlab("") +
      ylab("") +
      theme(plot.background = element_rect(fill='gray20'), 
            panel.background = element_rect(fill='gray20'), 
            axis.text = element_blank(),
            panel.grid.major = element_line(colour = "gray20"),   
            panel.grid.minor = element_blank(),
            legend.position='none') +
      scale_fill_manual(values=c("#56B4E9", "#ffff00", "#FF5733", "#d397fc")), tooltip="text") %>%
    highlight(dynamic = TRUE, persistent = TRUE, color=c('#e98756', '#ff00ff', '#33ffc2', '#fce897')),
  ggplotly(ggplot(share, aes(y=FIRE_YEAR)) +
             geom_hline(yintercept = seq(1992, 2015, by = 1), color = "gray", size = 0.04) +
             scale_size_area(max_size = 15, guide = FALSE) +
             scale_x_date(date_breaks = "months", date_labels = "%b") +
             scale_y_reverse(limits = c(2015,1992), breaks = c(2010,2005,2000,1995)) +
             xlab("") +
             ylab("") +
             theme(plot.background = element_rect(fill='gray20'), 
                   panel.background = element_rect(fill='gray20'), 
                   axis.text.y = element_text(colour='gray98', size=16),
                   axis.text.x = element_text(colour='gray98', size=10),
                   panel.grid.major = element_blank(),   
                   panel.grid.minor = element_blank(),
                   panel.grid.major.y = element_line(size=1.2, color="gray"),
                   legend.position='none',
                   axis.ticks.x = element_line(colour='gray98')) +
             geom_point(aes(size = GISAcres*2, x = plot_date, color = NEWCAT, group=1, text=paste('Name:', IncidentNa, '<br>Type:', NEWCAT, '<br>Date:', format(StartDate, "%b %d %Y"), '<br>Duration:', duration, "days")), alpha=0.5) +
             scale_color_manual(values=c("#56B4E9", "#ffff00", "#FF5733", "#d397fc")), tooltip="text") %>%
    highlight("plotly_selected", persistent = FALSE, dynamic=TRUE, color=c('#e98756', '#ff00ff', '#33ffc2', '#fce897')),
  widths=c(6,6))




summary









firessf<-st_as_sf(fires)

register_google(key='REDACTED')


bbox<-st_bbox(firessf)
bbox

lat <- c(bbox[[2]], bbox[[4]])                  
lon <- c(bbox[[1]], bbox[[3]])               
center = c(mean(lon), mean(lat)) 

map <- get_map(location = center, maptype =  "satellite",  zoom=9, source = "google")

  ggmap(map, darken = c(0.6, "gray20")) + 
  geom_sf(data=firessf, aes(fill=NEWCAT), inherit.aes = F, alpha=0.5) + 
  coord_sf(datum = NA)+
  xlab("") +
  ylab("") +
  theme(plot.background = element_rect(fill='gray20'), 
        panel.background = element_rect(fill='gray20'), 
        axis.text = element_blank(),
        panel.grid.major = element_line(colour = "gray20"),   
        panel.grid.minor = element_blank()) +
  scale_fill_manual(values=c("#56B4E9", "#ffff00", "#FF5733", "#d397fc"))


#This doesn't work because geommaptiles types isn't in plotly yet
ggplotly(ggplot(firessf) +
  annotation_map_tile(cachedir = system.file("rosm.cache", package = "ggspatial")) +
  geom_sf(aes(fill=NEWCAT)))

  

bbox<-st_bbox(firessf)
bbox[[1]] 
 library(crosstalk)

share<-SharedData$new(firessf)
bscols(ggplotly(ggplot(share) + 
    geom_sf(aes(fill=NEWCAT), alpha=0.5) +
    xlab("") +
    ylab("") +
    theme(plot.background = element_rect(fill='gray20'), 
          panel.background = element_rect(fill='gray20'), 
          axis.text = element_blank(),
          panel.grid.major = element_line(colour = "gray20"),   
          panel.grid.minor = element_blank(),
          legend.position='none') +
    scale_fill_manual(values=c("#ffff00", "#FF5733", "#56B4E9", "#d397fc"))) %>%
    highlight(dynamic = TRUE, persistent = TRUE),
    

  ggplotly(ggplot(share, aes(y=FIRE_YEAR)) +
    geom_hline(yintercept = seq(1992, 2015, by = 1), color = "gray", size = 0.04) +
    scale_size_area(max_size = 15, guide = FALSE) +
    scale_x_date(date_breaks = "months", date_labels = "%b") +
    scale_y_reverse(limits = c(2015,1992), breaks = c(2010,2005,2000,1995)) +
    xlab("") +
    ylab("") +
    theme(plot.background = element_rect(fill='gray20'), 
          panel.background = element_rect(fill='gray20'), 
          axis.text = element_text(colour='gray98', size=16),
          panel.grid.major = element_blank(),   
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(size=1.2, color="gray"),
          legend.position='none') +
    geom_point(aes(size = GISAcres*2, x = plot_date, color = NEWCAT), alpha=0.5) +
    scale_color_manual(values=c("#ffff00", "#ff5733", "#56b4b9", "#d397fc"))) %>%
    highlight("plotly_selected", persistent = FALSE))






bubble
demo("sf-plotly-3D-globe", package = "plotly")
DEMO CODE
library(plotly)

> library(dplyr)

> # latitude, longitude, and altitiude of tropical storms
  > storms <- sf::st_read(system.file("shape/storms_xyz.shp", package = "sf"), quiet = TRUE)

> # even grid of lat/lons spanning the globe (for creating the globe surface)
  > nlat <- 200

> nlon <- 100

> lat <- seq(-180, 180, length.out = nlat)

> lon <- seq(-90, 90, length.out = nlon)

> lat <- matrix(rep(lat, nlon), nrow = nlat)

> lon <- matrix(rep(lon, each = nlat), nrow = nlat)

> # helper function for converting polar (lat/lon) -> cartesian (x/y/z)
  > degrees2radians <- function(degree) degree * pi / 180 

> # show as little as possible when hovering over surface
  > empty_axis <- list(
    +   showgrid = FALSE, 
    +   zeroline = FALSE,
    +   showticklabels = FALSE,
    +   showspikes = FALSE,
    +   spikesides = FALSE,
    +   title = ""
    + )

> # for centering camera/lighting on the center of the storm paths
  > xyzmean <- list(x = .41, y = -.71, z = 0.57)

> # A 3D globe implemented with 3D lines and a spherical surface
  > # Note that the globe has a radius of 1, but project the lines with 
  > # a radius of 1.001 so that we appear on top of the surface
  > globe <- plot_ly(height = 500) %>%
  +   add_sf(
    +     data = sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE)), 
    +     x = ~ 1.001 * cos(degrees2radians(x)) * cos(degrees2radians(y)),
    +     y = ~ 1.001 * sin(degrees2radians(x)) * cos(degrees2radians(y)),
    +     z = ~ 1.001 * sin(degrees2radians(y)),
    +     color = I("black"), size = I(1),
    +     hoverinfo = "none"
    +   ) %>%
  +   add_sf(
    +     data = highlight_key(storms, group = "storm paths"),
    +     name = "storm paths",
    +     x = ~ 1.001 * cos(degrees2radians(x)) * cos(degrees2radians(y)),
    +     y = ~ 1.001 * sin(degrees2radians(x)) * cos(degrees2radians(y)),
    +     z = ~ 1.001 * sin(degrees2radians(y)),
    +     color = ~z, size = I(6),
    +     text = ~paste("Latitude:", y, "<br>", "Longitude:", x, "<br>", "Altitude:", z),
    +     hoverinfo = "text"
    +   ) %>%
  +   add_surface(
    +     x = cos(degrees2radians(lon)) * cos(degrees2radians(lat)),
    +     y = sin(degrees2radians(lon)) * cos(degrees2radians(lat)),
    +     z = sin(degrees2radians(lat)),
    +     # NOTE: you can map a value to surfacecolor to encode, say air temp
      +     # for an example, see https://github.com/cpsievert/Weather_Stuff/blob/master/radiation-plot-3D.R
      +     # But here is a trick to set the surface color to a constant white
      +     surfacecolor = matrix(NA, nrow = nlat, ncol = nlon),
    +     showscale = FALSE, hoverinfo = "skip",
    +     lightposition = xyzmean, 
    +     contours = list(
      +       x = list(highlight = FALSE), 
      +       y = list(highlight = FALSE), 
      +       z = list(highlight = FALSE)
      +     )
    +   ) %>%
  +   layout(
    +     showlegend = FALSE,
    +     scene = list(
      +       xaxis = empty_axis,
      +       yaxis = empty_axis,
      +       zaxis = empty_axis,
      +       aspectratio = list(x = 1, y = 1, z = 1),
      +       camera = list(eye = xyzmean)
      +     )
    +   )

> # spherical distance between the first point and every other point
  > # https://en.wikipedia.org/wiki/Great-circle_distance
  > arc_dist <- function(lon, lat) {
    +   lon <- degrees2radians(lon)
    +   lat <- degrees2radians(lat)
    +   lon0 <- lon[1]
    +   lat0 <- lat[1]
    +   delta <- cos(abs(lon - lon0))
    +   acos(sin(lat0) * sin(lat) + cos(lat0) * cos(lat) * delta)
    + }

> # plot altitude of each storm versus the distance it has traveled
  > distanceByAlt <- storms %>%
  +   sf::st_coordinates() %>%
  +   as.data.frame() %>%
  +   group_by(L1) %>%
  +   mutate(dist = arc_dist(X, Y)) %>%
  +   rename(altitude = Z) %>%
  +   highlight_key(~L1, group = "storm paths") %>%
  +   plot_ly(x = ~dist, y = ~altitude, height = 400) %>%
  +   # plotly.js doesn't support color gradient along *2D* lines
  +   add_lines(color = I("gray")) %>%
  +   add_markers(
    +     color = ~altitude, hoverinfo = "text",
    +     text = ~paste("Distance:", round(dist, 2), "<br>", "Altitude:", altitude, "<br>", "Storm:", L1)
    +   ) %>%
  +   layout(
    +     showlegend = FALSE,
    +     title = "Tropical storm altitude by distance \n (click to highlight storm)",
    +     font = list(size = 15, family = "Balta"),
    +     margin = list(t = 60)
    +   )

> # force persistent selection
  > # TODO: persistence via shift should work with two separate graphs!!
  > options(persistent = TRUE)

> library(htmltools)

> browsable(tagList(globe, distanceByAlt))








firessf$StartDate<-as.Date(firessf$StartDate, format="%Y%m%d", origin="1970-01-01")
fires$ContrDate<-as.Date(fires$ContrDate, format="%Y%m%d", origin="1970-01-01")
fires$duration<-difftime(fires$ContrDate, fires$StartDate, units="days")
fires$plot_date<-as.Date(fires$StartDate, format = "%Y%m%d")

firessf$newcause<-=case_when(firessf$FireTypePr == 1  ~ "Natural",
                             firessf$FireTypePr== 13 | is.na(stat_cause_code) ~ "Unknown",
                             firessf$FireTypePr>= 2 | stat_cause_code <= 12 ~ "Human")







summary(fires$plot_date)

#Don't really understand dates in R or why this next step is necessary, but Buzzfeed did it and it made the dates work on
fires$plot_date<-as.Date(format(fires$plot_date,"2017-%m-%d"))

#First static plot
# color palette for major fire causes
cause_pal <- c("#ffff00","#d397fc","#ffffff")

fires<-fortify(fires, region="FireID")
names(fires)

p<- ggplot(data=fires, aes(y=fires$FIRE_YEAR)) +
  geom_hline(yintercept = seq(1980, 2017, by = 1), color = "gray", size = 0.05) +
  scale_size_area(max_size = 10, guide = FALSE) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_y_reverse(limits = c(2017,1980), breaks = c(2010,2000,1990,1980)) +
  xlab("") +
  ylab("") +
  theme_hc(style = "darkunica", base_size = 20, base_family = "ProximaNova-Semibold") +
  theme(axis.text = element_text(color = "#ffffff")) +
  geom_point(data=fires, aes(x=plot_date, y=FIRE_YEAR))

fires$

summary(fires$plot_date)

summary(fires$FIRE_YEAR)
length(fires$plot_date)
fires$plot_date

length(fires)
length(fires$GISAcres)
length(fires$FIRE_YEAR)
length(fires$plot_date)

nrow(fires)
