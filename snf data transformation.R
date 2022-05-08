library(ggplot2)
library(shiny)
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(maps)
library(mapdata)
library(mapproj)

snf=read.csv("https://data.chhs.ca.gov/dataset/7759311f-1aa8-4ff6-bfbb-ba8f64290ae2/resource/d4d68f74-9176-4969-9f07-1546d81db5a7/download/covid19datanursinghome.csv")

snf0 <- snf

df0 <- snf0 %>% 
  select(-c(facility_id,facility_name,note)) %>%
  mutate(ccounty = tolower(county)) %>% 
  na.omit() %>%
  select(-county) %>% 
  rename("Date" = as_of_date) %>% 
  rename("Total Resident Cases" = total_resident_cases) %>% 
  rename("New Resident Cases" = new_resident_cases) %>% 
  rename("Total Resident Deaths" = total_resident_deaths) %>% 
  rename("Total Healthcare Worker Cases" = total_health_care_worker_cases) %>% 
  rename("New Healthcare Worker Cases" = new_health_care_worker_cases) %>% 
  rename("Total Healthcare Worker Deaths" = total_health_care_workers_deaths) %>% 
  rename("County" = ccounty)

df0 <- data.frame(df0, stringsAsFactors = F)

states <- map_data("state")

calif <- subset(states,region == "california")

ca_counties <- subset(map_data("county"), region=="california")

dfz <- df0 %>% 
  group_by(County) %>% 
  summarise(totalhcwcases_min = min(Total.Healthcare.Worker.Cases),
            totalhcwcases_med = median(Total.Healthcare.Worker.Cases),
            totalrcases_min = min(Total.Resident.Cases),
            totalrcases_med = median(Total.Resident.Cases),
            totalrdeaths_min = min(Total.Resident.Deaths),
            totalrdeaths_med = median(Total.Resident.Deaths),
            totalhcwdeaths_min = min(Total.Healthcare.Worker.Deaths),
            totalhcwdeaths_med = median(Total.Healthcare.Worker.Deaths),
            newrcases_min = min(New.Resident.Cases),
            newrcases_med = median(New.Resident.Cases),
            newhcwcases_med = median(New.Healthcare.Worker.Cases),
            newhcwcases_min = min(New.Healthcare.Worker.Cases))

ca_counties2 <- inner_join(x=ca_counties,
                           y=dfz,
                           by = c("subregion" = "County"))

ca_counties2 %>%
  ggplot(aes(x=long,y=lat,group=subregion, fill=totalhcwcases_med)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
  scale_fill_continuous(type = "viridis")+
  theme(legend.position="bottom",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank())

ca_counties2 %>%
  ggplot(aes(x=long,y=lat,group=subregion, fill=newhcwcases_med)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
  scale_fill_continuous(type = "viridis")+
  theme(legend.position="bottom",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank())

ca_counties2 %>%
  ggplot(aes(x=long,y=lat,group=subregion, fill=totalrcases_med)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
  scale_fill_continuous(type = "viridis")+
  theme(legend.position="bottom",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank())

ca_counties2 %>%
  ggplot(aes(x=long,y=lat,group=subregion, fill=totalrdeaths_med)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
  scale_fill_continuous(type = "viridis")+
  theme(legend.position="bottom",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank())

ca_counties2 %>%
  ggplot(aes(x=long,y=lat,group=subregion, fill=totalhcwdeaths_med)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
  scale_fill_continuous(type = "viridis")+
  theme(legend.position="bottom",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank())

ca_counties2 %>%
  ggplot(aes(x=long,y=lat,group=subregion, fill=newrcases_med)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
  scale_fill_continuous(type = "viridis")+
  theme(legend.position="bottom",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank())