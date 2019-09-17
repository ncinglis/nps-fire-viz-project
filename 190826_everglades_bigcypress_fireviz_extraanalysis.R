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
library(data.table)
library(raster)

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

firesdt<-as.data.table(fires)
firesdt$timeperiod <- ifelse(firesdt$CalendarYear >= 2000 & firesdt$CalendarYear <=2009, 1,
                             ifelse(firesdt$CalendarYear>=2010 & firesdt$CalendarYear <=2018 , 2,0))




nat_fires<- firesdt[NEWCAT=="Natural wildfire",.(number=.N, acres=sum(ControlAcres), avg=mean(ControlAcres)) , by=timeperiod]
human_fires<-firesdt[NEWCAT=="Human-caused wildfire",.(number=.N, acres=sum(ControlAcres), avg = mean(ControlAcres)) , by=timeperiod]
rx_fires<-firesdt[NEWCAT=="Prescribed Fire",.(number=.N, acres=sum(na.omit(ControlAcres)), avg = mean(na.omit(ControlAcres))) , by=timeperiod]
ma<-firesdt[NEWCAT=="Mutual aid",.(number=.N, acres=sum(na.omit(ControlAcres)), avg = mean(na.omit(ControlAcres))) , by=timeperiod]

nat_fires
human_fires
rx_fires
ma





#Number
((nat_fires[3,2] - nat_fires[2,2])/nat_fires[2,2])*100
#Acres
((nat_fires[3,3] - nat_fires[2,3])/nat_fires[2,3])*100
#Avg
((nat_fires[3,4] - nat_fires[2,4])/nat_fires[2,4])*100


#Number
((human_fires[3,2] - human_fires[2,2])/human_fires[2,2])*100
#Acres
((human_fires[3,3] - human_fires[2,3])/human_fires[2,3])*100
#Avg
((human_fires[3,4] - human_fires[2,4])/human_fires[2,4])*100

((rx_fires[3,2] - rx_fires[2,2])/rx_fires[2,2])*100
((rx_fires[3,3] - rx_fires[2,3])/rx_fires[2,3])*100
((rx_fires[3,4] - rx_fires[2,4])/rx_fires[2,4])*100


((ma[2,2] - ma[1,2])/ma[1,2])*100
((ma[2,3] - ma[1,3])/ma[1,3])*100
((ma[2,4] - ma[1,4])/ma[1,4])*100




#Make fires spatial
#Spatial join into FMUs


fmu<-readOGR("C:/ncstate/NPS/southflorida/FMUs.shp", stringsAsFactors = F)
refco<-crs(fmu)

names(fires)


xy <- fires[,c(42,43)]

xy[is.na(xy)] <- -999

sd <- SpatialPointsDataFrame(coords = xy, data = fires,
                             proj4string = refco)
library(spatialEco)

new<-point.in.poly(sd, fmu)

fmusdt<-as.data.table(new)

table(is.na(fmusdt$Label))




###Check mutual aids to see if 15 and 16s are different
ma <-fmusdt[NEWCAT == 'Mutual aid' & is.na(NAME)==T]

write.csv(fmusdt, "data/all_data_fmus.csv")

Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1Ijoibmlra2lpIiwiYSI6ImNqdWxhcHJqMTB1eXk0ZG80dDR2NjVkZnMifQ.jCTe3UM5cUDd67MWTlAp1A')
plot_mapbox(ma, x=~LongitudeDD, y=~LatitudeDD, color=~FireTypePr,
            alpha=0.7, size=~(ControlAcres^(1/2)), 
            mode="scattermapbox", 
            text=~paste('Name:', FireName, '<br>Park:', ReportingUnitName, '<br>Type:', NEWCAT, '<br>Date:', format(StartDate, "%b %d %Y"), '<br>Duration:', duration, "days", '<br>WFU?:', ifelse(FireTypePr==14 | FireTypePr==49, "Yes", "No"), "<br>Size class:", SizeClass), hoverinfo="text") %>% layout(plot_bgcolor = '#333333', paper_bgcolor = '#333333', mapbox = list(style = 'dark', zoom=7, center =list(lat =25.683891,lon = -80.872782)))

#Divide up by time period
#Then divide up by FMU

fmusdt$timeperiod <- ifelse(fmusdt$CalendarYear >= 1979 & fmusdt$CalendarYear <=1999, 1,
                            ifelse(fmusdt$CalendarYear>=2000 & fmusdt$CalendarYear <=2018 , 2,0))



nat_fmu <- fmusdt[NEWCAT=="Natural wildfire", .(number=.N, acres=sum(ControlAcres), avg=mean(ControlAcres)), by=.(timeperiod, NAME, UNITNAME)]
human_fmu <- fmusdt[NEWCAT=="Human-caused wildfire",.(park = UNITNAME, number=.N, acres=sum(ControlAcres), avg=mean(ControlAcres)), by=.(timeperiod, NAME, UNITNAME)]
rx_fmu <- fmusdt[NEWCAT=="Prescribed Fire",.(park=UNITNAME, number=.N, acres=sum(na.omit(ControlAcres)), avg=mean(na.omit(ControlAcres))), by=.(timeperiod, NAME, UNITNAME)]
ma_fmu <- fmusdt[NEWCAT=="Mutual aid",.(park=UNITNAME, number=.N, acres=sum(na.omit(ControlAcres)), avg=mean(na.omit(ControlAcres))), by=.(timeperiod, NAME,UNITNAME)]

perc<-function(x,y) {
  ((y - x)/x)*100
}

options(scipen = 999)

nat_fmu_diff <- nat_fmu[timeperiod==1 | timeperiod==2, 
                        .(number=perc(number[timeperiod==1], number[timeperiod==2]) , 
                          acres = perc(acres[timeperiod==1], acres[timeperiod==2]) , 
                          avg = perc(avg[timeperiod==1], avg[timeperiod==2]) ), 
                        by=.(NAME, UNITNAME)]

nat_fmu_diff


human_fmu_diff <- human_fmu[timeperiod==1 | timeperiod==2, 
                          .(number=perc(number[timeperiod==1], number[timeperiod==2]) , 
                            acres = perc(acres[timeperiod==1], acres[timeperiod==2]) , 
                            avg = perc(avg[timeperiod==1], avg[timeperiod==2]) ), 
                          by=.(NAME, UNITNAME)]

human_fmu_diff

rx_fmu_diff <- rx_fmu[timeperiod==1 | timeperiod==2, 
                         .(number=perc(number[timeperiod==1], number[timeperiod==2]) , 
                           acres = perc(acres[timeperiod==1], acres[timeperiod==2]) , 
                           avg = perc(avg[timeperiod==1], avg[timeperiod==2]) ), 
                      by=.(NAME, UNITNAME)]

rx_fmu_diff

ma_fmu_diff <- ma_fmu[timeperiod==1 | timeperiod==2, 
                         .(number=perc(number[timeperiod==1], number[timeperiod==2]) , 
                           acres = perc(acres[timeperiod==1], acres[timeperiod==2]) , 
                           avg = perc(avg[timeperiod==1], avg[timeperiod==2]) ), 
                      by=.(NAME, UNITNAME)]

write.csv(nat_fmu_diff, "C:/ncstate/NPS/southflorida/viz_summary/nat_fmu_diff.csv")
write.csv(human_fmu_diff, "C:/ncstate/NPS/southflorida/viz_summary/human_fmu_diff.csv")
write.csv(rx_fmu_diff, "C:/ncstate/NPS/southflorida/viz_summary/rx_fmu_diff.csv")
 names(fmusdt)


#Pine rocklands bubble
pr<-fmusdt[NAME == 'Pine Rocklands',,]

pr_bubble<-ggplot(pr, aes(y=CalendarYear)) +
  geom_hline(yintercept = seq(1971, 2018, by = 1), color = "gray", size = 0.04) +
  scale_size_area(max_size = 15, guide=FALSE) +
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
  
  geom_point(aes(size = ControlAcres*2, x = plot_date, group=CalendarYear, text=paste('Name:', FireName, '<br>Park:', ReportingUnitName, '<br>Type:', NEWCAT, '<br>Date:', format(StartDate, "%b %d %Y"), '<br>Duration:', duration, "days", '<br>WFU?:', ifelse(FireTypePr==14 | FireTypePr==49, "Yes", "No"), "<br>Size class:", SizeClass)),
             color = "#03c700", alpha=0.5) +
  guides(colour = guide_legend(override.aes = list(size=10)))

ggplotly(pr_bubble, tooltip='text')

##Look at natral outs to figure out how reporting as changed
#Filter 21-26
nos<-firesdt[FireTypePr >= 21 & FireTypePr <= 26,,]


#Make bubble chart of natural outs
parkpal<-c("#0050d1", "#0a9900")
n <- highlight_key(nos, ~ReportingUnitName)
p<-ggplot(n, aes(y=CalendarYear)) +
  geom_hline(yintercept = seq(1971, 2018, by = 1), color = "gray", size = 0.04) +
  scale_size_area(max_size = 15, guide=FALSE) +
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
  
  geom_point(aes(size = ControlAcres*3, x = plot_date, color = ReportingUnitName, group=CalendarYear, text=paste('Name:', FireName, '<br>Park:', ReportingUnitName, '<br>Type:', NEWCAT, '<br>Date:', format(StartDate, "%b %d %Y"), '<br>Duration:', duration, "days", '<br>WFU?:', ifelse(FireTypePr==14 | FireTypePr==49, "Yes", "No"), "<br>Size class:", SizeClass)), alpha=0.75) +
  scale_color_manual(values=parkpal) +
  guides(colour = guide_legend(override.aes = list(size=10)))

ggplotly(p, tooltip='text') %>% hide_legend()






##OLD ANALYSIS BELOW




#Make base chart, all fires, colored by Cause

pal<-c("#FF5733", "#d397fc", "#ffff00", "#56B4E9")
n <- highlight_key(fires, ~NEWCAT)
p<-ggplot(n, aes(y=CalendarYear)) +
  geom_hline(yintercept = seq(1971, 2018, by = 1), color = "gray", size = 0.04) +
  scale_size_area(max_size = 15, guide=FALSE) +
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
  
  geom_point(aes(size = ControlAcres*2, x = plot_date, color = NEWCAT, group=CalendarYear, text=paste('Name:', FireName, '<br>Park:', ReportingUnitName, '<br>Type:', NEWCAT, '<br>Date:', format(StartDate, "%b %d %Y"), '<br>Duration:', duration, "days", '<br>WFU?:', ifelse(FireTypePr==14 | FireTypePr==49, "Yes", "No"), "<br>Size class:", SizeClass)), alpha=0.5) +
  scale_color_manual(values=c("#FF5733", "#d397fc", "#ffff00", "#56B4E9")) +
  guides(colour = guide_legend(override.aes = list(size=10)))

library(dplyr)
ftable(fires$SizeClass)
pg<-ggplotly(p, tooltip='text')


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
p2g<-ggplotly(p2, tooltip=FALSE, dynamicTicks = T) %>% layout(xaxis=list( type='date',
                                                                        tickformat='%b',
                                                                        dtick="M1"))
p3g<-ggplotly(p3, tooltip=FALSE, dynamicTicks = T) %>% layout(xaxis=list( type='date',
                                                                          tickformat='%b',
                                                                          dtick="M1"))
subplot(pg, subplot(p2g,p3g, margin=c(.02,.1,.1,.1), titleY=T), nrows = 2, heights=c(.7,.3),titleY=T) %>% highlight("plotly_click") %>% highlight("plotly_click")

ggplot(n, aes(x=plot_date, y=CalendarYear, fill = NEWCAT)) + 
  geom_col()

l <- list(
  font = list(
    family = "sans-serif",
    size = 12,
    color = "#FAFAFA"),
  bgcolor = "gray20")


f<- list(
  family = "Arial, sans-serif",
  size = 18,
  color = "gray98")



hline <- function(y = 0, color = "#FAFAFA") {
  list(
    type = "line", 
    x0 = 0, 
    x1 = 1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = color,
                width=0.2)
  )
}

years<-c(1971:2018)


plotly1<-plot_ly(data=n, x=~plot_date, 
        y=~CalendarYear, 
        color=~NEWCAT,
        colors=pal,
        alpha=0.7,
        type='scatter',
        mode='markers',
        size=~(ControlAcres^(2/3)),
        text=paste('Name:', fires$FireName, '<br>Park:', fires$ReportingUnitName,'<br>Type:', fires$NEWCAT, '<br>Date:', format(fires$StartDate, "%b %d %Y"), '<br>Duration:', fires$duration, "days", '<br>WFU?:', ifelse(fires$FireTypePr==14, "Yes", "No"), '<br>Acres:', fires$ControlAcres),
        hoverinfo='text',
        marker = list(sizeref=4,sizemode='diameter')) %>% layout(paper_bgcolor='#333333', 
         plot_bgcolor='#333333',
         yaxis=list(family = "Arial, sans-serif",
                    size = 18,
                    color = "#FAFAFA",
                    title='',
                    autorange="reversed",
                    gridcolor = toRGB("gray98"),
                    gridwidth=2,
                    tickvals = seq(1975, 2015, 10)),
         xaxis=list( family = "Arial, sans-serif",
                     size = 18,
                     color = "#FAFAFA",
                     title='',
                     ticks="outside",
                     type='date',
                     tickformat='%b',
                     dtick="M1",
                     showgrid=F),
         shapes = lapply(years, FUN=hline))

















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

?ggplotly

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
  theme(plot.background = element_rect(fill='gray20', color = NA), 
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



#Linked brushing with leaflet 


devtools::install_github("rstudio/leaflet#346")



library(leaflet)

qquery <- highlight_key(fires)

l <- list(
  font = list(
    family = "sans-serif",
    size = 12,
    color = "#FAFAFA"),
  bgcolor = "gray20")


f<- list(
  family = "Arial, sans-serif",
  size = 18,
  color = "gray98")



hline <- function(y = 0, color = "#FAFAFA") {
  list(
    type = "line", 
    x0 = 0, 
    x1 = 1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = color,
                width=0.2)
  )
}

years<-c(1971:2018)


plotly1<-plot_ly(data=qquery, x=~plot_date, 
                 y=~CalendarYear, 
                 color=~NEWCAT,
                 colors=pal,
                 alpha=0.7,
                 type='scatter',
                 mode='markers',
                 size=~(ControlAcres^(2/3)),
                 text=paste('Name:', fires$FireName, '<br>Park:', fires$ReportingUnitName,'<br>Type:', fires$NEWCAT, '<br>Date:', format(fires$StartDate, "%b %d %Y"), '<br>Duration:', fires$duration, "days", '<br>WFU?:', ifelse(fires$FireTypePr==14, "Yes", "No"), '<br>Acres:', fires$ControlAcres),
                 hoverinfo='text',
                 marker = list(sizeref=4,sizemode='diameter')) %>% layout(paper_bgcolor='#333333', 
                                                                          plot_bgcolor='#333333',
                                                                          yaxis=list(family = "Arial, sans-serif",
                                                                                     size = 18,
                                                                                     color = "#FAFAFA",
                                                                                     title='',
                                                                                     autorange="reversed",
                                                                                     gridcolor = toRGB("gray98"),
                                                                                     gridwidth=2,
                                                                                     tickvals = seq(1975, 2015, 10)),
                                                                          xaxis=list( family = "Arial, sans-serif",
                                                                                      size = 18,
                                                                                      color = "#FAFAFA",
                                                                                      title='',
                                                                                      ticks="outside",
                                                                                      type='date',
                                                                                      tickformat='%b',
                                                                                      dtick="M1",
                                                                                      showgrid=F),
                                                                          shapes = lapply(years, FUN=hline)) %>% highlight("plotly_selected", dynamic = TRUE)


Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1Ijoibmlra2lpIiwiYSI6ImNqdWxhcHJqMTB1eXk0ZG80dDR2NjVkZnMifQ.jCTe3UM5cUDd67MWTlAp1A')
share<-SharedData$new(fires)

library(leaflet)
bscols(
leaflet(share) %>%
  addProviderTiles(providers$Esri.WorldImagery, options=providerTileOptions(opacity = 0.5)) %>%
  addCircles(fillColor = ~NEWCAT, weight = 1,
                          opacity = 0.7, fillOpacity = 0.5, color='gray', lat=fires$LatitudeNAD83, lng=fires$LongitudeNAD83) %>% 
  highlight(dynamic = TRUE, color=c('#e98756', '#ff00ff', '#33ffc2', '#fce897')), pg %>% highlight("plotly_selected", dynamic = TRUE),
widths=c(6,6))
  


plot_mapbox(share, x=~LongitudeDD, y=~LatitudeDD, mode="scattermapbox") %>% layout(title = 'Meteorites by Class',
                                                                                font = list(color='white'),
                                                                                plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
                                                                                mapbox = list(style = 'dark'),
                                                                                legend = list(orientation = 'h',
                                                                                              font = list(size = 8)),
                                                                                margin = list(l = 25, r = 25,
                                                                                              b = 25, t = 25,
                                                                                              pad = 2))








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
