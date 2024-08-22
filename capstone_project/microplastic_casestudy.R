ggplot() + 
  geom_map( 
  data = mapdata, map = mapdata, 
  aes(long, lat, map_id = region)) +
  geom_point(data = micro_plastics, aes(Longitude, Latitude, color = `Density Range`, size = `Measurement`))+
  facet_wrap(~Oceans)



sorted_data<-micro_plastics[order("Date")]
df<-micro_plastics

df %>% arrange(mdy_hms(df$Date)) %>% 
  group_by(Unit) %>% 
  if (Unit == "pieces/m3") %>% 
          sum(df(which(df$Measurement =='pieces/m3', na.rm = TRUE))
              
sum(df[which(df$Unit=='pieces/m3'),6])

sum(df[which(df$Measurement == 'pieces/10mins'), 6])

plastic_percentage<-micro_plastics %>% 
  group_by(Oceans) %>%
  filter(any(Unit == "pieces/m3", na.rm = TRUE)) %>% 
  summarise(percentage = n() / nrow(micro_plastics) * 100)
  
percentage_by_year<-micro_plastics %>% 
  group_by(year = mdy_hms(Date)) %>%
  summarize(percentage = n() / nrow(micro_plastics) * 100)

dates_to_years <- micro_plastics(Date = as.Date(c(mdy_hms())))
years_lubridate <- year(df$Date)

ggplot(data = plastic_percentage, aes(x=Oceans, y=percentage))+
         geom_bar(stat = "identity")