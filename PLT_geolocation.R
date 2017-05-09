#plotting slow and fast PC users in en-US

library(ggmap)
library(mapproj)




dat <- read.csv("./UsersSlowFastWithGeoLocation_2017-03-01_2017-03-01.csv", sep = ',', header = FALSE)
colnames(dat) <- c("ImpressionGuid","Cluster","Muid","Country","State","City","region","PostalCode","Lat","Long","TImeZone","Server","Date")
head(dat)

summary(dat$State)
montanaDat <- subset(dat,dat$State=='Montana')

map<- get_map(location="united states", zoom=4, maptype = "roadmap")
ggmap(map)+
  geom_point(aes(x = Long, y = Lat, colour = factor(Cluster), alpha=0.4 ), data = dat)

montanaDat <- subset(dat,dat$State=='Montana')
map<- get_map(location="montana", zoom=6, maptype = "roadmap")
ggmap(map)+
  geom_point(aes(x = Long, y = Lat, colour = factor(Cluster), alpha=0.9 ), data = montanaDat)




texasDat <- subset(dat,dat$State=='Texas')
map<- get_map(location="texas", zoom=6, maptype = "roadmap")
ggmap(map)+
  geom_point(aes(x = Long, y = Lat, colour = factor(Cluster), alpha=0.5 ), data = texasDat)
dat$City



  newMXDat <- subset(dat,dat$State=='New Mexico')
map<- get_map(location="new mexico", zoom=6, maptype = "roadmap")
ggmap(map)+
  geom_point(aes(x = Long, y = Lat, colour = factor(Cluster), alpha=0.9 ), data = newMXDat)


KenDat <- subset(dat,dat$State=='Kentucky')
map<- get_map(location="Kentucky", zoom=7, maptype = "hybrid")
ggmap(map)+
  geom_point(aes(x = Long, y = Lat, colour = factor(Cluster), alpha=0.9 ), data = KenDat)


OhDat <- subset(dat,dat$State=='Ohio')
map<- get_map(location="Ohio", zoom=7, maptype = "hybrid")
ggmap(map)+
  geom_point(aes(x = Long, y = Lat, colour = factor(Cluster), alpha=0.9 ), data = OhDat)
slowDat <- subset(dat,dat$Cluster==2)
fastDat <- subset(dat,dat$Cluster==5)

subset(fastDat, fastDat$City %in% slowDat$city & fastDat$State %in% slowDat$State)


#slo fast in the same city
overlapDat<- read.csv("./SlowFastInSameCity.csv", sep = ',', header = FALSE)
colnames(overlapDat) <- c("ImpressionGuid","Cluster","Muid","Country","State","City","region","PostalCode","Lat","Long","TImeZone","Server","Date")
head(overlapDat)


map<- get_map(location="united states", zoom=4, maptype = "state")
ggmap(map)+
  geom_point(aes(x = Long, y = Lat, colour = factor(Cluster), alpha=0.3 ), data = overlapDat)



#places that never had a fast impression
slowPlaces <- subset(dat,dat$City %in% c("Glenmont","Lowes","Terry","Rapelje","Harris","Farnam","Nowata","Meeker","Eufaula","Mount Olive","Laingsburg"))
map<- get_map(location="USA", zoom=4, maptype = "roadmap")
ggmap(map)+
  geom_point(aes(x = Long, y = Lat, colour = "Red", alpha=0.9 ), data = slowPlaces)

