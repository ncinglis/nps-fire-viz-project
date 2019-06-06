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


#Read in WFMI 
everWFMI<-read.csv("data/ever.csv", stringsAsFactors = F)
bicyWFMI<-read.csv("data/bicy.csv", stringsAsFactors = F)
fires<-rbind(everWFMI, bicyWFMI)
names(fires)

#Format dates

fires$StartDate<-as.character(fires$StartDate)
fires$ContrDate<-as.character(fires$ControlDate)
fires$StartDate<-as.Date(fires$StartDate, format="%Y%m%d", origin="1970-01-01")
fires$ContrDate<-as.Date(fires$ContrDate, format="%Y%m%d", origin="1970-01-01")

#Calculate duration
fires$duration<-difftime(fires$ContrDate, fires$StartDate, units="days")

#Add plotting date (maintain month and day, static year (2017))
fires$plot_date<-as.Date(fires$StartDate, format = "%Y%m%d")
fires$plot_date<-as.Date(format(fires$plot_date,"2017-%m-%d"))


#Calculate NEWCAT 
fires$NEWCAT<-case_when(fires$FireTypePr == 48 ~ "Prescribed Fire",
                        fires$FireTypePr == 15 | fires$FireTypePr == 16 ~ "Mutual aid",
                        fires$FireTypePr != 48 & fires$CauseCategory == 'Human' & fires$FireTypePr != 15 & fires$FireTypePr !=16 ~ "Human-caused wildfire",
                        fires$CauseCategory == 'Natural' & fires$FireTypePr != 15 & fires$FireTypePr !=16 ~ "Natural wildfire")



#Make base chart, all fires, colored by Cause


p<-ggplot(fires, aes(y=CalendarYear)) +
           geom_hline(yintercept = seq(1971, 2015, by = 1), color = "gray", size = 0.04) +
           scale_size_area(max_size = 15, guide = FALSE) +
           scale_x_date(date_breaks = "months", date_labels = "%b") +
           scale_y_reverse(limits = c(2015,1971), breaks = c(2010,2005,2000,1995,1990,1985,1980,1975)) +
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
           geom_point(aes(size = ControlAcres, x = plot_date, color = NEWCAT, group=1, text=paste('Name:', FireName, '<br>Type:', NEWCAT, '<br>Date:', format(StartDate, "%b %d %Y"), '<br>Duration:', duration, "days")), alpha=0.5) +
           scale_color_manual(values=c("#56B4E9", "#ffff00", "#FF5733", "#d397fc"))



p



#leaflet try
pal<-colorFactor(c("#56B4E9", "#ffff00", "#FF5733", "#d397fc"), fires$NEWCAT)



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
