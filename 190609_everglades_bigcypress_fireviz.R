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
library(ggspatial)
library(leaflet)
library(lubridate)
library(gridExtra)
devtools::install_github("ropensci/plotly")

#Read in WFMI 
everWFMI<-read.csv("data/ever.csv", stringsAsFactors = F)
bicyWFMI<-read.csv("data/bicy.csv", stringsAsFactors = F)
fires<-rbind(everWFMI, bicyWFMI)
names(fires)

#Format dates

fires$StartDate<-as.character(fires$StartDate)
fires$ContrDate<-as.character(fires$ControlDate)
fires$StartDate<-as.Date(fires$StartDate, format="%m/%d/%Y", origin="1970-01-01")
fires$ContrDate<-as.Date(fires$ContrDate, format="%m/%d/%Y", origin="1970-01-01")

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
fires$week <- floor_date(fires$plot_date, "week")



#Make base chart, all fires, colored by Cause

pal<-c("#FF5733", "#d397fc", "#ffff00", "#56B4E9")
n <- highlight_key(fires, ~NEWCAT)
p<-ggplot(n, aes(y=CalendarYear)) +
  geom_hline(yintercept = seq(1971, 2018, by = 1), color = "gray", size = 0.04) +
  scale_size_area(max_size = 15, guide = FALSE) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_y_reverse(limits = c(2018,1971), breaks = c(2015,2010,2005,2000,1995,1990,1985,1980,1975)) +
  xlab("") +
  ylab("") +
  theme(plot.background = element_rect(fill='gray20'), 
        panel.background = element_rect(fill='gray20'), 
        axis.text.y = element_text(colour='gray98', size=16),
        axis.text.x = element_text(colour='gray98', size=10),
        panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(size=1.2, color="gray"),
        legend.background = element_rect(fill="gray20"),
        legend.title = element_blank(),
        legend.text = element_text(colour="gray98"),
        axis.ticks.x = element_line(colour='gray98')) +
  
  geom_point(aes(size = ControlAcres*2, x = plot_date, color = NEWCAT, text=paste('Name:', FireName, '<br>Type:', NEWCAT, '<br>Date:', format(StartDate, "%b %d %Y"), '<br>Duration:', duration, "days")), alpha=0.5) +
  scale_color_manual(values=c("#FF5733", "#d397fc", "#ffff00", "#56B4E9")) +
  guides(colour = guide_legend(override.aes = list(size=10)))


p2<- ggplot(n, aes(x=plot_date, fill = NEWCAT)) + 
  geom_histogram(alpha=0.7, color=NA, bins=52, position="identity") +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_y_continuous(position = "right") +
  theme(plot.background = element_rect(fill='gray20'), 
        panel.background = element_rect(fill='gray20'), 
        axis.text.y = element_text(colour='gray98', size=14),
        axis.text.x = element_text(colour='gray98', size=10),
        panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(size=1.2, color="gray"),
        axis.ticks.x = element_line(colour='gray98'),
        plot.margin=unit(c(0,2,0,0), 'cm'),
        legend.position='none',
        axis.title.y = element_text(colour='gray98', size=14),
        plot.title  = element_text(colour='gray98', size=20))+ 
  xlab("") +
  ylab("Fire occurences") +
  scale_fill_manual(values=pal)



p3<-ggplot(n, aes(x=plot_date, fill = NEWCAT)) + 
  geom_col(aes(x=week, y=ControlAcres), alpha=0.7, color=NA, position = "identity") +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(plot.background = element_rect(fill='gray20'), 
        panel.background = element_rect(fill='gray20'), 
        axis.text.y = element_text(colour='gray98', size=14),
        axis.text.x = element_text(colour='gray98', size=10),
        panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(size=1.2, color="gray"),
        axis.ticks.x = element_line(colour='gray98'),
        plot.margin=unit(c(0,2,0,0), 'cm'),
        legend.position='none',
        axis.title.y = element_text(colour='gray98', size=14)) +
  xlab("") +
  ylab("Acres burned") +
  scale_fill_manual(values=pal)

pg<-ggplotly(p, tooltip="text")
p2g<-ggplotly(p2, tooltip=FALSE)
p3g<-ggplotly(p3, tooltip=FALSE)

subplot(pg, subplot(p2g,p3g, margin=c(.02,.1,.1,.1), titleY=T), nrows = 2, heights=c(.7,.3), margin=c(0,0,0,0.1),titleY=T) %>% highlight("plotly_click") %>% highlight("plotly_click")


#Static plots of cost over time
dualpal<-c("#56B4E9","#ffff00")
cost<-read.csv("data/costs.csv", stringsAsFactors = F) 

names(cost)<-c("park", "type", "year", "cost")

costbicy<-cost[cost$park=="bicy",]
costever<-cost[cost$park=="ever",]


c<-ggplot(costbicy, aes(x=year)) + 
  geom_col(aes(x=year, y=cost, fill=type, colour=dualpal), alpha=0.7, color=NA, position = "dodge") +
  scale_x_continuous(breaks=seq(2013,2018,1))+
  scale_y_continuous(breaks=seq(0, max(costbicy$cost), 500000), 
                     minor_breaks = seq(0, max(costbicy$cost), 250000),
                     labels=function(x) format(x, big.mark = ",", scientific = FALSE),
                     position='right') +
  theme(plot.background = element_rect(fill='gray20'), 
        panel.background = element_rect(fill='gray20'), 
        axis.text.y = element_text(colour='gray98', size=12),
        axis.text.x = element_text(colour='gray98', size=12, vjust = 5),
        panel.grid.major.x = element_blank(), 
        axis.ticks.x.bottom = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line(colour = "gray98", size=0.3),
        panel.grid.major.y = element_line(size=1.2, color="gray98"),
        legend.background = element_rect(fill="gray20"),
        legend.position="bottom",
        plot.title =element_text(colour='gray98', size=16, face='bold'),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(colour="gray98", size=10),
        axis.ticks.x = element_line(colour='gray98'),
        plot.margin=unit(c(0,2,0,0), 'cm'),
        axis.title.y = element_text(colour='gray98', size=14)) +
  ggtitle("BIG CYPRESS NATIONAL PRESERVE") +
  xlab("") +
  ylab("Annual cost") +
  scale_fill_manual(values=dualpal, labels=c("Prescribed fire", "Wildfire"))


c2<-ggplot(costever, aes(x=year)) + 
  geom_col(aes(x=year, y=cost, fill=type, colour=dualpal), alpha=0.7, color=NA, position = "dodge") +
  scale_x_continuous(breaks=seq(2013,2018,1))+
  scale_y_continuous(breaks=seq(0, max(costever$cost), 200000), 
                     minor_breaks = seq(0, max(costever$cost), 100000),
                     labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(plot.background = element_rect(fill='gray20'), 
        panel.background = element_rect(fill='gray20'), 
        axis.text.y = element_text(colour='gray98', size=12),
        axis.text.x = element_text(colour='gray98', size=12, vjust = 5),
        axis.ticks.x.bottom = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line(colour = "gray98", size=0.3),
        panel.grid.major.y = element_line(size=1.2, color="gray98"),
        legend.background = element_rect(fill="gray20"),
        legend.position="bottom",
        plot.title =element_text(colour='gray98', size=16, face='bold'),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(colour="gray98", size=10),
        axis.ticks.x = element_line(colour='gray98'),
        plot.margin=unit(c(0,2,0,0), 'cm'),
        axis.title.y = element_text(colour='gray98', size=14)) +
  ggtitle("EVERGLADES NATIONAL PARK") +
  xlab("") +
  ylab("Annual cost") +
  scale_fill_manual(values=dualpal, labels=c("Prescribed fire", "Wildfire"))


grid.arrange(c2, c, nrow=1)


#Plotly of individual wildfire costs (size = size, color = cost)
WFMIcosts<-read.csv("C:/ncstate/NPS/southflorida/2010-2018_code_ID_combo.csv")

justcost<-merge(fires,WFMIcosts, by.x="FireId", by.y="fireID")

nrow(justcost)

c3<-ggplot(justcost, aes(y=CalendarYear)) +
  geom_hline(yintercept = seq(2010, 2018, by = 1), color = "gray", size = 0.04) +
  scale_size_area(max_size = 15) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_y_reverse(limits = c(2018,2010), breaks = c(2015,2010)) +
  xlab("") +
  ylab("") +
  theme(plot.background = element_rect(fill='gray20'), 
        panel.background = element_rect(fill='gray20'), 
        axis.text.y = element_text(colour='gray98', size=16),
        axis.text.x = element_text(colour='gray98', size=10),
        panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(size=1.2, color="gray"),
        legend.background = element_rect(fill="gray20"),
        legend.title = element_blank(),
        legend.text = element_text(colour="gray98"),
        axis.ticks.x = element_line(colour='gray98')) +
  geom_point(aes(size = ControlAcres^(1/2), x = plot_date, color = cost, text=paste('Name:', FireName, '<br>Cost:', dollar(justcost$cost), '<br>Date:', format(StartDate, "%b %d %Y"), '<br>Duration:', duration, "days")), alpha=0.5) +
  scale_color_gradientn(colors=brewer.pal(9, 'YlOrRd')) +
  guides(colour = guide_legend(override.aes = list(size=10)))
ggplotly(c3, tooltip="text")

dollar(justcost$cost)

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
