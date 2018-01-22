library(ggplot2)
library(maps)
library(rgeos)
library(maptools)
library(ggmap)
library(geosphere)
library(plyr)



df = read.csv('F:/2017-SOFTWARE-WORKSPACES/FLIGHT-DATA-PROJECT/routes_all.csv',stringsAsFactor=F)
df=df[df$from %in% 'India',]
#print(df)

# calculating rotes
routes = gcIntermediate(df[,c('lon1', 'lat1')], df[,c('lon2', 'lat2')], 200, breakAtDateLine = FALSE, addStartEnd = TRUE, sp=TRUE)


fortify_routes=ldply(routes@lines, fortify)

# merge to form great circles
num_routes = data.frame('count'=df$count, 'id'=1:nrow(df), 'Countries'=df$from)
greatcircles = merge(fortify_routes, num_routes, all.x=T, by='id')


worldmap = map_data ("world")

# background layer
background<-c(geom_polygon(aes(long,lat,group=group), size = 0.1, colour= "#000000",
                     fill="#000000", alpha=0.8, data=worldmap))
# urban layer
urbanareas <- readShapePoly("F:/2017-SOFTWARE-WORKSPACES/15 JAN 2018 FLIGHT DATA PRACTICE/ne_10m_urban_areas.shp")
urb <- c(geom_polygon(aes(long, lat, group = group),
                      size = 0.3,
                      color = "#ffffff",
                      fill = "#ffffff",
                      alpha = 0.6,
                      data = urbanareas))

# combine
ggplot() +
  background +
  urb +
  geom_line(aes(long,lat,group=id, color=Countries), alpha = 0.3, size=0.01, data= greatcircles) +scale_color_manual(values=c("#0000ff"))+
  theme(panel.background = element_rect(fill='#00001C',colour='#00001C'), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  
   
  annotate("text",x=max(worldmap$long),y=-60,hjust=.9,size=6,
           label=paste("International Flight Routes via INDIA","Data From OpenFlights.org", sep="\n"),color="white")
