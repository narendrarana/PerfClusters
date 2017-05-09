userDat<- read.table("./DataForUserLevelClustering.csv", header = FALSE,sep=",", 
                     col.names = c("ClientId" ,           
                                   "Request_Browser",
                                   "SumClickCount",
                                   "SumAdClickCount",        
                                   "SumAdRevenue",          
                                   "SumDSQ",
                                   "NoOfSession",
                                   "NoOfActiveDays",
                                   "AVGSessionDuration",
                                   "AvgPLT",
                                   "AvgPLTv2"         ,
                                   "AvgResponseLatency", 
                                   "AvgRequestLatency",  
                                   "AvgConnectionTime",
                                   "AvgFetchLatency",
                                   "AvgProcessingTime",
                                   "AvgDOMLatency",
                                   "AvgRTT",
                                   "AvgServerLatency",
                                   "AvgBrowser_ImgTagCount",
                                   "AvgResponseBytes",
                                   "AvgElementCount",
                                   "AvgScriptCount",
                                   "AvgMetrics_AlgoAndAnserAndAdsCount",
                                   "SumClientErrorEventPresent",
                                   "count",
                                   "P75thPLT",
                                   "P75thPLTv2",
                                   "P75thResponseLatency",
                                   "P75thRequestLatency",
                                   "P75thConnectionTime",              
                                   "P75thFetchLatency",
                                   "P75thProcessingTime",
                                   "P75thDOMLatency",
                                   "P75thFdServerLatency"
                     ))


str(userDat)

typeof<-function(x){
  if(class(x))
}
normalizeNumericData<-function(x){
  if(class(x)=="factor")
  {
    out<- x
  }
  else
  {
    out <-((x-min(x))/(max(x)-min(x)))
  }
  return (out)
 }

class(userDat$SumDSQ)

normalizeNumericData(userDat$count)
summary(userDat$NoOfActiveDays)
summary(userDat$SumDSQ)

userDat$DSQ <- normalizeNumericData(userDat$SumDSQ)
summary(userDat$DSQ)
summary(userDat$SumDSQ)
str(userData)
userData<- as.data.frame(lapply(userDat,normalizeNumericData))
summary(userData)

userData$AvgPLT<- NULL
userData$AvgPLTv2<- NULL

userData$DSQ <- NULL
k_5 <- kmeans(userData[3:34], centers=5, nstart =50) # 65%
k_8 <- kmeans(userData[3:34], centers=8, nstart =50) # 72%
k_12 <- kmeans(userData[3:34], centers=12, nstart =50) #77.2
k_15 <- kmeans(userData[3:32], centers=15, nstart =50) # 79.5
k_16 <- kmeans(userData[3:32], centers=16, nstart =50)

write.csv(cbind(k_15$centers,k_15$size),"c:/data/Perf_ml/UsrClusteringResults_15.csv")

str(userData)

?kmeans
#Happy User : User with lots of DSQ/Session and high number of sessions , also High Revenue...

# finding outliers
summary(userDat)
#if(class(userDat$ClientId)=="factor") {print "hh"} else {print 'tt'}

# Lets only take rows who have been active for more than 3 days
activeUsersData <- subset(userDat,userDat$NoOfActiveDays>3)
#normalize
activeUsersData<- as.data.frame(lapply(activeUsersData,normalizeNumericData))
str(activeUsersData)

activeUsersData$DSQ<- NULL
activeUsersData$AvgPLT <-NULL
activeUsersData$AvgPLTv2<-NULL
activeUsersData$AvgResponseLatency<-NULL
activeUsersData$AvgRequestLatency<-NULL
activeUsersData$AvgConnectionTime<-NULL
activeUsersData$AvgFetchLatency<-NULL
activeUsersData$AvgServerLatency<-NULL
activeUsersData$AvgDOMLatency<-NULL
activeUsersData$AvgProcessingTime<-NULL
activeUsersData$NoOfActiveDays<-NULL

activek_24<- kmeans(activeUsersData[,3:25],centers = 24, nstart = 50)

write.csv(cbind(activek_24$centers,activek_24$size),"c:/data/Perf_ml/ActiveUsrClusteringResults_24.csv")




# reading larger dataset for DOM flight
domData <- read.table("c:/data/Perf_ML_Laptop/DomElementUserPerformanceLog.csv", header = FALSE,sep=",", 
                      col.names = c("ClientId" ,           
                                    "Request_Browser",
                                    "DSQ",
                                    "ClickCount",
                                    "AdClickCount",        
                                    "AdRevenue",          
                                    "Sessions",
                                    "SessionDuration",
                                    "PLT",
                                    "PLTv2"         ,
                                    "ResponseLatency", 
                                    "RequestLatency",  
                                    "ConnectionTime",
                                    "FetchLatency",
                                    "ProcessingTime",
                                    "DOMLatency",
                                    "AvgServerLatency",
                                    "ImgTagCount",
                                    "ResponseBytes",
                                    "DOMElements",
                                    "ScriptTags",
                                    "AlgoAndAnserAndAdsCount",
                                    "ClientErrors"
                      ))

str(domData)

domDataNormalized<- as.data.frame(lapply(domData,normalizeNumericData))

str(domDataNormalized)

summary(domDataNormalized)

nk_34<- kmeans(domDataNormalized[3:23], centers=34,nstart=50)

write.csv(cbind(nk_34$size,nk_34$centers),"./DomFlightClusteringResults.csv")