citation(package = 'plotly')

fires <-read.csv("C:/ncstate/715/finalproject/nps-fire-viz-project/data/all_data_fmus.csv", stringsAsFactors = F)

fires$StartDate

#Calculate NEWCAT 
fires$NEWCAT<-case_when(fires$FireTypePr == 48 ~ "Prescribed Fire",
                        fires$FireTypePr == 15 | fires$FireTypePr == 16 & is.na(fires$NAME) == T ~ "Mutual aid",
                        fires$CauseCategory == 'Human' ~ "Human-caused wildfire",
                        fires$CauseCategory == 'Natural'  ~ "Natural wildfire", TRUE ~ "other")

fires<-fires[fires$NEWCAT !='other',]


fires$StartDate<-as.character(fires$StartDate)
fires$ContrDate<-as.character(fires$ContrDate)
fires$StartDate<-as.Date(fires$StartDate, format="%Y-%m-%d", origin="1970-01-01")
fires$ContrDate<-as.Date(fires$ContrDate, format="%Y-%m-%d", origin="1970-01-01")

fires$duration<-difftime(fires$ContrDate, fires$StartDate, units="days")

#Add plotting date (maintain month and day, static year (2017))
fires$plot_date<-as.Date(fires$StartDate, format = "%Y-%m-%d")
fires$plot_date<-as.Date(format(fires$plot_date,"2017-%m-%d"))

fires$week <- floor_date(fires$plot_date, "week")

pal<-c("#FF5733", "#d397fc", "#ffff00", "#56B4E9")


fires$plot_date


Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1Ijoibmlra2lpIiwiYSI6ImNrNDdjMHVrNjB0am4za25uMnpmaDQ4dWsifQ.Y8FjR-KmYKCabgdcYyfWAg')
firesb<-fires[fires$FireName != "LOGGERHEAD" & fires$FireName!= "CRACKUP #1" & fires$FireName!= "ROBERTS" & fires$FireName != "GUM SLGH 1" & fires$FireName != "BLOCKSDOF" & fires$FireName != "ROG NE",]
share<-SharedData$new(firesb)

pp<-ggplot(share, aes(y=CalendarYear)) +
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
  
  geom_point(aes(size = ControlAcres*2, x = plot_date, color = NEWCAT, text=paste('Name:', FireName, '<br>Park:', ReportingUnitName, '<br>Type:', NEWCAT, '<br>Date:', format(StartDate, "%b %d %Y"), '<br>Duration:', duration, "days", '<br>WFU?:', ifelse(FireTypePr==14 | FireTypePr==49, "Yes", "No"), "<br>Size class:", SizeClass)), alpha=0.5) +
  scale_color_manual(values=c("#FF5733", "#d397fc", "#ffff00", "#56B4E9")) +
  guides(colour = guide_legend(override.aes = list(size=10)))


comp<-c('#33ffc2', '#fce897', '#ff00ff', '#e98756')
names(comp)<-c("1", "2", "3", "4")
ppg<-ggplotly(pp, tooltip='text')

bscols(ppg %>% highlight("plotly_selected", persistent=T) %>% hide_legend(),
       plot_mapbox(share, x=~LongitudeDD, y=~LatitudeDD, color=~NEWCAT, colors=pal,
                   alpha=0.7, size=~(ControlAcres^(1/2)), mode="scattermapbox", text=~paste('Name:', FireName, '<br>Park:', ReportingUnitName, '<br>Type:', NEWCAT, '<br>Date:', format(StartDate, "%b %d %Y"), '<br>Duration:', duration, "days", '<br>WFU?:', ifelse(FireTypePr==14 | FireTypePr==49, "Yes", "No"), "<br>Size class:", SizeClass), hoverinfo="text") %>% layout(plot_bgcolor = '#333333', paper_bgcolor = '#333333', mapbox = list(style = 'dark', zoom=7, center =list(lat =25.683891,lon = -80.872782))) %>% highlight(dynamic = TRUE, persistent=T, color=comp),  widths=c(6,6))

names(fires)

library(data.table)

firesdt<-as.data.table(fires)

fires_rxacerage<-firesdt[(NEWCAT == "Prescribed Fire"), (acres = sum(na.omit(ControlAcres))), by = CalendarYear]

mean(fires_rxacerage$V1)
