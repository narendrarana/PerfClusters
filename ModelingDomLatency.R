dat<- read.table("C:/Data/Perf_ML_Laptop/totaldataset.tsv", sep=",", header=TRUE)
head(dat)
str(dat)
library(MASS)

# sample first 2 million rows

sampleIndex <-sample(nrow(dat), 2000000,replace=FALSE)
sampleDat <- dat[sampleIndex,]
head(sampleDat)

summary(sampleDat$ConnectionTime)
nrow(subset(sampleDat,sampleDat$ConnectionTime<0)) # 35 rows with negative connection time.
summary(sampleDat$DOMLatency)
nrow(subset(sampleDat,sampleDat$DOMLatency<0)) #1757 rows with negative domlatency
summary(sampleDat$SingleAnswer_LegacyPLT)
nrow(subset(sampleDat,sampleDat$SingleAnswer_LegacyPLT<0)) # 7334 rows with negative values for legacy PLT

sampleDat <- subset(sampleDat,sampleDat$SingleAnswer_LegacyPLT> 0 & sampleDat$DOMLatency >0 & sampleDat$ConnectionTime >0)
sampleDat <- subset(sampleDat,sampleDat$IsControlFlight == 'False')
str(sampleDat)
sampleDat$X<- NULL
sampleDat$RTT<- NULL









videodat <- subset(sampleDat,sampleDat$SingleAnswer_Service=='MULTIMEDIAKIFVIDEOANSWER' & sampleDat$SingleAnswer_Scenario=="VIDEO" & sampleDat$IsControlFlight=='False')
str(videodat)


# divide into training and test
trainIndex <-sample(nrow(videodat), 63506*.7,replace=FALSE)
trainingData <- videodat[trainIndex,]
testData <- videodat[-trainIndex,]



summary(videodat$ConnectionTime)
mod1 <- lm(DOMLatency~SingleAnswer_lenJavascript+SingleAnswer_lenCSS+SingleAnswer_lenHTML+SingleAnswer_numLinks+
             SingleAnswer_numImages+SingleAnswer_numRequests,
           data=trainingData)
summary(mod1)


mod2 <- rlm(DOMLatency~SingleAnswer_lenJavascript+SingleAnswer_lenCSS+SingleAnswer_lenHTML+SingleAnswer_numLinks+
             SingleAnswer_numImages+SingleAnswer_numRequests, data=trainingData)
summary(mod2)

summary(videodat$DOMLatency)
quantile(videodat$ConnectionTime,.90)
videodat<- subset(videodat,videodat$DOMLatency<600)

plot(videodat$ConnectionTime,videodat$DOMLatency)
plot(videodat$SingleAnswer_numImages,videodat$DOMLatency)

subset(videodat,videodat$ConnectionTime > videodat$DOMLatency)

videodat <- subset(videodat,videodat$DOMLatency)


summary(videodat$DOMLatency)
summary(videodat$ConnectionTime)
videodat <- subset(videodat,videodat$ConnectionTime<350)
nrow(videodat)



3+4
pairs(videodat)