#This script does profiling and creates figures on simulation output
#Check to see if required pacakges are installed, and if not, install them
if(!require(wesanderson)){
  install.packages("wesanderson")
  library(wesanderson)
}
library(rgdal)
library(ggmap)

source("Load_Incidents.R")

#Read in simulation output information
simulation_set <- read_csv("simulation_set.csv")

simulation_summary <- read_csv("simulation_summary.csv")
plus1simulation_summary <- read_csv("simulation_summary_sensitivityplus1.csv")
plus2simulation_summary <- read_csv("simulation_summary_sensitivityplus2.csv")

simulatedincidents_by_firehouse <- apply(simulation_summary[2:9],2,sum)
names(simulatedincidents_by_firehouse) <- str_replace(names(simulatedincidents_by_firehouse),'\\.',' ')

#Sensitivity Analysis
plus1simulatedincidents_by_firehouse <- apply(plus1simulation_summary[2:9],2,sum)
names(plus1simulatedincidents_by_firehouse) <- str_replace(names(plus1simulatedincidents_by_firehouse),'\\.',' ')
plus1 <- data.frame(plus1simulatedincidents_by_firehouse) %>% rownames_to_column() %>% 
  select(Stations=rowname,'Add 1 Engine'=plus1simulatedincidents_by_firehouse)

plus2simulatedincidents_by_firehouse <- apply(plus2simulation_summary[2:9],2,sum)
names(plus2simulatedincidents_by_firehouse) <- str_replace(names(plus2simulatedincidents_by_firehouse),'\\.',' ')
plus2 <- data.frame(plus2simulatedincidents_by_firehouse) %>% rownames_to_column() %>% 
  select(Stations=rowname,'Add 2 Engines'=plus2simulatedincidents_by_firehouse)

sensitivity <- data.frame(simulatedincidents_by_firehouse) %>% rownames_to_column() %>% 
  select(Stations=rowname,'Existing Engines'=simulatedincidents_by_firehouse) 
sensitivity <- inner_join(sensitivity,plus1)
sensitivity <- inner_join(sensitivity,plus2)

sensitivity %>% gather(-Stations,key=Sensitivity, value=Engines) %>%  
ggplot() +
  geom_col(aes(x=Stations,y=Engines,fill=Sensitivity),position='dodge') +
  scale_fill_manual(values=wes_palette(n=3, name='Chevalier1')) +
  labs(x='Fire Station',y='Simulated Incidents',title='Simulated Incidents by Fire Station') +
  theme_bw()

simulatedincidents_by_borough <- inner_join(simulation_set,incidents_filter) %>% 
  select(Period_ID,Period,IncidentNumber,IncGeo_BoroughCode,IncGeo_BoroughName) %>%
  group_by(IncGeo_BoroughCode) %>% count() %>% select(Borough=IncGeo_BoroughCode,IncidentCount=n)

#Calculate average frequency for simulation set
avg_sim_freq <- nrow(simulation_set)/n_distinct(simulation_set$Period_ID)

#Frequency plot for simulation week
start_date <- as.POSIXct(ymd(20180101))
end_date <- as.POSIXct(ymd(20180103))

simulation_set %>% group_by(Period) %>% count() %>%
  ggplot() +
  geom_point(aes(x=Period,y=n)) +
  geom_line(aes(x=Period,y=n)) +
  scale_x_datetime(limits=c(start_date,end_date)) +
  scale_y_continuous(limits=c(0,10)) +
  labs(x='Date',y='Number of Incidents per 15-Minute Period',title='Incidents per Hour for Simulation Set') +
  theme_bw()

#Actual frequency of calls (all data)
#Create day/time of call combined field
incidents_clean$DateTimeOfCall <- incidents_clean$TimeOfCall + days(difftime(ymd(incidents_clean$DateOfCall),ymd('1899-12-31')))
#Chunk data into 15-minute intervals
incidents_clean$quarter_hours <- cut(incidents_clean$DateTimeOfCall, breaks="15 mins")
incidents_clean$quarter_hours <- as.POSIXct(strptime(incidents_clean$quarter_hours, "%Y-%m-%d %H:%M:%S"))
actual_freq <- incidents_clean %>% group_by(quarter_hours) %>% summarize(IncidentCount=n()) %>% 
  group_by(IncidentCount) %>% summarize(ActualPeriodCount=n()) %>% 
  mutate(Historical=ActualPeriodCount/sum(ActualPeriodCount))

simulated_freq <- simulation_set %>% group_by(Period) %>% summarize(IncidentCount=n()) %>% 
  group_by(IncidentCount) %>% summarize(SimPeriodCount=n()) %>% 
  mutate(Simulated=SimPeriodCount/sum(SimPeriodCount))

#Histograms
inner_join(actual_freq,simulated_freq) %>% select(IncidentCount,Historical,Simulated) %>%
  gather(-IncidentCount,key=DataSet,value=PercentofIncidents) %>%
  ggplot() +
  geom_col(aes(x=IncidentCount,y=PercentofIncidents,fill=DataSet),position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  scale_y_continuous(labels=scales::percent, breaks=c(0,0.05,0.10,0.15,0.20,0.25,0.30)) +
  scale_fill_manual(values=wes_palette(n=2, name="Chevalier1")) +
  labs(x='Number of Incidents',y='Percent of Total Incidents',title='Incidents Per 15-Minute Period for Historical Data and Simulation Set') +
  annotate('text',x=8, y=c(0.24,0.25),label=c('Simulated Average Frequency = 2.87','Historical Average Frequency = 2.77')) +
  theme_bw()

#Number of Incidents Tables
simulatedincidents_perquarterhour <- 
  simulation_set %>% group_by(Period) %>% count() %>% select(Period,IncidentsPerQuarterHour=n) %>%
  group_by(IncidentsPerQuarterHour) %>% count() %>% select(IncidentsPerQuarterHour,IncidentCount=n)

#Map incidents and fire stations
boroughs <- readOGR("statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp", encoding = "ESRI Shapefile")

boroughs_transform <- spTransform(boroughs, CRS("+proj=longlat +datum=OSGB36"))
boroughs_points <- fortify(boroughs_transform, region='GSS_CODE')
boroughs_df <- inner_join(boroughs_points, boroughs@data, by =c("id"="GSS_CODE"))

boroughs_list <- data.frame(Borough=boroughs_transform@data$GSS_CODE)
boroughs_list <- inner_join(boroughs_list,simulatedincidents_by_borough)
boroughs_list[is.na(boroughs_list$IncidentCount),'IncidentCount'] <- 0

boroughs_sim <- inner_join(boroughs_df,boroughs_list,by=c("id"="Borough"))

register_google(key ='AIzaSyDAvX5nyp1H-0oFfczQjQIfP1_TyLADE_0')

#Axis cleanup
remove_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

london_map <- get_map(location='London', zoom=10, scale="auto", maptype="roadmap", source="google")
london_map_zoom <- get_map(location=c(lon=-0.15,lat=51.57), zoom=11, scale="auto", maptype="roadmap", source="google")

#Map of simulated incidents
ggmap(london_map_zoom) +
  geom_polygon(data=boroughs_sim, aes(x=long, y=lat, fill=IncidentCount, group=group), size=.2,color='black', alpha=0.5) +
  geom_point(data=stations, aes(x=Longitude,y=Latitude), shape=10) +
  geom_text(data=stations, aes(x=Longitude,y=Latitude,label=`Station name`), size=2.5, color='blue',nudge_y=0.003) +
  scale_fill_gradient(low="green",high="red",
                      trans = "log10") +
  remove_axes

#Map of historical incidents
incidents_by_borough <- incidents_clean %>% group_by(IncGeo_BoroughCode,ProperCase,IncidentGroup) %>% count() %>%
  select(Borough_Code=IncGeo_BoroughCode,Borough=ProperCase,IncidentGroup,Incidents=n) %>%
  spread(key=IncidentGroup, value=Incidents) %>%
  mutate('Total Incidents'=sum(`False Alarm`+`Special Service`+Fire))
boroughs_historical <- data.frame(Borough=boroughs_transform@data$GSS_CODE)
boroughs_historical <- left_join(boroughs_historical,incidents_by_borough,by=c("Borough"="Borough_Code"))

boroughs_actual <- inner_join(boroughs_df,boroughs_historical,by=c("id"="Borough"))

#With fire stations
ggmap(london_map) +
  geom_polygon(data=boroughs_actual, aes(x=long, y=lat, fill=`Total Incidents`, group=group), size=.2,color='black',alpha=0.5) +
  geom_point(data=current_stations, aes(x=Longitude,y=Latitude), shape=10) +
  geom_text(data=current_stations, aes(x=Longitude,y=Latitude,label=`Station name`), size=2.5, color='blue',position=position_jitter(width=0.01,height=0.007)) +
  scale_fill_gradient(low="green",high="red",
                      trans = "log10") +
  remove_axes

#Without fire stations
ggmap(london_map) +
  geom_polygon(data=boroughs_actual, aes(x=long, y=lat, fill=`Total Incidents`, group=group), size=.2,color='black', alpha=0.5) +
  scale_fill_gradient(low="green",high="red",
                      trans = "log10") +
  remove_axes

#Simulation Results
#Limiting to 6 incidents per period captures 98 percent of the simulations
inner_join(simulation_set %>% select(Period_ID) %>% group_by(Period_ID) %>% summarize(Calls=n()),simulation_summary) %>%
ggplot() +
  geom_boxplot(aes(x=as.factor(Calls),y=Total_Distance)) +
  scale_x_discrete(limits=c(1:6)) +
  labs(x='Number of Incidents per 15-Minute Period',y='Total Effective Distance (km)',title='Optimized Total Effective Distance') +
  theme_bw()
