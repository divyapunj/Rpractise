#########################
# Import all libraries
#########################

library(readr)
library(plyr)
library(dplyr)

#########################
# Import data Files
#########################

Price_3RDParty <- read_csv("/Volumes/USB_Storage-1/ILS FMV - 737 Sample Data-Dec 18 2018-03 16 PM/30269510/737_Price_3RDParty.csv")
ILSBids <- read_csv("/Volumes/USB_Storage-1/ILS FMV - 737 Sample Data-Dec 18 2018-03 16 PM/30269510/737_Price_ILSBids.csv")

#########################
# 3rd Party Price Analysis
#########################

Price_3RDParty$SCRAPEDATE<-as.Date(as.character(Price_3RDParty$SCRAPEDATE), "%m/%e/%Y")

#Temp Analysis for this data set

b1<-ddply(Price_3RDParty,.(PARTNUMBER), summarize,occurance = length(unique(SCRAPEDATE)))
b2<-ddply(b1,.(occurance), summarize,count = length(PARTNUMBER))


#########################
# ILS Bids Price Analysis
#########################
#Remove the temp analysis from the previous levels
rm(b1,b2)

length(unique(ILSBids$PARTNUMBER))
ILSBids$BIDCREATEDATE<-as.Date(as.character(ILSBids$BIDCREATEDATE), "%m/%e/%Y")

partsList<-as.list(unique(ILSBids$PARTNUMBER))

#Temp Analysis for this data set
#Find the number of times every part has been transacted upon
b1<-ddply(ILSBids,.(PARTNUMBER), summarize,occurance = length(unique(BIDCREATEDATE)))
summary(b1$occurance)
#Find the average and the median price of all the parts under consideration
b2<-ddply(ILSBids,.(PARTNUMBER), summarize,averageTicketSize = mean(BIDPRICE*BIDQUANTITY),medianTicketSize = median(BIDPRICE*BIDQUANTITY))

#############################
# Polynomial long term trend fit
# Data is ILS Bids
#############################

partsList<-as.list(unique(ILSBids$PARTNUMBER))

part<-subset(ILSBids, ILSBids$PARTNUMBER==partsList[1] & !is.na(ILSBids$TRACEABILITYCODE) & ILSBids$RFQUENDUSERIND==1)

b3<-ddply(part,.(PARTNUMBER,CONDITIONCODE,dayLine), summarize,averageTicketSize = mean(BIDPRICE*BIDQUANTITY),medianTicketSize = median(BIDPRICE*BIDQUANTITY))

minBidDate<-min(part$BIDCREATEDATE)
part$dayLine<-part$BIDCREATEDATE-minBidDate
part$price<-part$BIDPRICE/part$BIDQUANTITY

require(cluster)
library(factoextra,NbClust)
fviz_nbclust(as.data.frame(part$dayLine), kmeans, method = "silhouette")
set.seed(123)
NbClust(part$dayLine, distance = "euclidean",
        min.nc = 2, max.nc = 6, 
        method = "kmeans", index ="silhouette")



model<-lm(price ~poly(dayLine,3),data = part)


predicted.intervals <- predict(model,(dayLine=part$dayLine),interval='confidence',
                               level=0.99)

plot(part$dayLine,part$price,col='deepskyblue4',xlab='days',main='Observed data')
lines(q,y,col='firebrick1',lwd=3)

lines(q,predicted.intervals[,1],col='green',lwd=3)
lines(q,predicted.intervals[,2],col='black',lwd=1)
lines(q,predicted.intervals[,3],col='black',lwd=1)