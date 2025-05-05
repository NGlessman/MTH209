library(tidyverse)

library(pacman)
p_load(tidyverse)

#Simple World Map

map<-map_data("world")
ggplot(map, aes(x=long, y=lat, group=group))+geom_polygon(fill="lightblue", colour="white")+theme_void()

North_Asia<- c("China", "Japan", "Mongolia", "South Korea", "North Korea", "Taiwan")
North_Asia_Map<- map_data("world", region= North_Asia)

#Center Locations
region.data <- North_Asia_Map %>% group_by(region)%>%summarise(long=mean(long), lat=mean(lat))%>%arrange()
ggplot(North_Asia_Map, aes(x=long, y=lat))+geom_polygon(aes(group=group, fill=region))+geom_text(data=region.data, aes(label=region), size=5, hjust=.5, col="#808080", fontface="bold")+scale_fill_viridis_d()+theme_void()+theme(legend.position="none")


#Chloropleth map
drinks<-read_csv("C:/Users/ngles/Downloads/drinks.csv")
drinks_map<-drinks%>%left_join(map, by=c("country"="region"))
ggplot(drinks_map, aes(long, lat, group=group))+geom_polygon(aes(fill= total_litres_of_pure_alcohol), color="white")+scale_fill_viridis_c(option="C")+labs(fill="Total Liters of Pure Alchohol")+theme_void()+theme(legend.position = "bottom")

ggplot(drinks, aes(map_id=country))+
  geom_map(aes(fill=total_litres_of_pure_alcohol), 
           map=map, color="white")+
  expand_limits(x=map$long, y=map$lat)+
  labs(fill="Total Liters of Pure Alcohol")+
  theme_void()+
  theme(legend.position="bottom")

#U.S State Map
US_map<-map_data("state")

state_data<- US_map%>% filter(region != "district of columbia") %>% 
  group_by(region)%>%
  summarise(long=mean(long), lat=mean(lat))%>%
  arrange(region)

state_data$region.abb <- state.abb[-c(2,11)]

p <- ggplot(US_map, aes(x=long, y= lat))+
  geom_polygon(aes(group=group, fill=region), color="white")+
  geom_text(data=state_data, aes(label=region.abb), fontface="bold")+
  theme_void()+
  theme(legend.position="none")

p

library(plotly)
ggplotly(p)

US_map<-map_data("state")

state_data<- US_map%>% filter(region !="district of columbia")%>%
  group_by(region) %>%
  summarise(long=mean(long), lat=mean(lat))%>%
  arrange(region)

state_data$region.abb<-state.abb[-c(2,11)]

crimes<-data.frame(region=rownames(USArrests), USArrests)%>%filter(region!=c("Alaska", "Hawaii"))

crimes$region<-tolower(crimes$region)

crimes_map<-crimes%>%left_join(US_map, by="region")

g1<-ggplot(crimes_map, aes(x=long, y=lat))+
  geom_polygon(aes(group=group, fill=Murder, 
                   text= paste0(region, ":\n", Murder, " murder arrests per 100,000"), colour="white"))+
  geom_text(data=state_data, 
            aes(label=region.abb), fontface="bold", size=3)+
  scale_fill_viridis_c(option="C")+
  theme_void()

ggplotly(g1, tooltip="text")

covid<- read_csv("C:/Users/ngles/Downloads/COVID19.csv")

ohio<- covid%>% filter(administrative_area_level_2=="Ohio", date == "2021-12-31")

ohio<-ohio%>%rename(county=administrative_area_level_3)

ohio_county<- map_data("county", region="ohio")

ohio_county$subregion<- str_to_title(ohio_county$subregion)

ohio_map<- ohio%>% left_join(ohio_county, by= c("county"="subregion"))

g2<-ggplot(ohio_map, aes(x=long, y=lat))+geom_polygon(aes(group=group, fill=deaths, text=paste0("County: ", county, "\n", "Total Deaths: ", deaths )))+
  geom_text(data=ohio, 
            aes(x=longitude, y=latitude, label=county), 
            color="white", fontface="bold")+
  scale_fill_viridis_c(option="H")+
  theme_minimal()+
  theme_void()

font<- list(family="Arial", size=15, color="white")

label<-list(
  bgcolor="#232F34",
  bordercolor="transparent",
  font=font)

ggplotly(g2, 
         tooltip="text", 
         width=850, height=800)%>%
  style(hoverlabel=label)%>%
  layout(font=font)
