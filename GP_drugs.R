setwd("/Users/francinebennett/Desktop/NHS_analysis/")

# Load packages
require(ggplot2)
require(plyr)
require(googleVis)
require(RSQLite)
require(gdata)

# List filenames
file.list<-c("T201109PDP IEXT.csv","T201110PDP IEXT.csv","T201111PDP IEXT.csv","T201112PDP IEXT.csv","T201201PDP IEXT.csv","T201202PDP IEXT.csv","T201203PDP IEXT.csv","T201204PDP IEXT.csv","T201205PDP IEXT.CSV")
addresses<-read.csv("T201204ADD REXT.CSV",header=FALSE)
short.addresses<-addresses[,c(2,3,6,8)]

# Create list of potential problem drugs
GP.drugs <- read.csv("T201109PDP IEXT.csv", header=TRUE)
drug.list<-unique(GP.drugs$BNF.NAME)
drug.list<-drug.list[order(drug.list)]
statins<-drug.list[grep("statin",drug.list)]
statins<-statins[-grep("Nystatin",statins)]
clopidogrel<-c("Clopidogrel")
sartans<-c(
  "Azilsartan Medoxomil",
  "Candesartan Cilexetil",
  "Eprosartan",
  "Irbesartan",
  "Olmesartan Medoxomil",
  "Telmisartan",
  "Valsartan",
  "Losartan Potassium")

problem.drugs<-as.data.frame(rbind(cbind(as.character(statins),"statin"),cbind(clopidogrel,"clopidogrel"),cbind(sartans,"sartan")))
names(problem.drugs)<-c("Drug","category")
problem.drugs$Drug<-trim(problem.drugs$Drug)
problem.drugs$saving<-as.numeric(0)
problem.drugs[problem.drugs$Drug=="Rosuvastatin Calcium",]$saving<-0.9239047
problem.drugs[problem.drugs$Drug=="Atorvastatin",]$saving<-0.91
problem.drugs[problem.drugs$Drug=="Candesartan Cilexetil",]$saving<-0.884934903905766

# Set up data frames for results
total.problem.spend<-data.frame(matrix(nrow=0,ncol=4))
spend.practice<-data.frame(matrix(nrow=0,ncol=11))
spend.pct<-data.frame(matrix(nrow=0,ncol=6))

# Loop to load, analyse, and remove large data files
for (i in 1:length(file.list)){
#for (i in 1:1){
file.name<-file.list[i]
GP.drugs <- read.csv(file.name, header=TRUE)
GP.drugs$BNF.NAME<-trim(GP.drugs$BNF.NAME)
t<-subset(GP.drugs,BNF.NAME %in% problem.drugs$Drug)
problem.spend<-aggregate(t[,c("ACT.COST","ITEMS")],by=list(t$BNF.NAME,t$PERIOD),FUN=sum)
names(problem.spend)<-c("Drug","Period","Spend","Items")
problem.spend$Spend<-round(problem.spend$Spend,digits=0)
problem.spend$Drug<-as.character(problem.spend$Drug)
total.problem.spend<-rbind(total.problem.spend,problem.spend)
  
# Calculations by practice
s<-aggregate(t[,c("ACT.COST","ITEMS")],by=list(t$PRACTICE,t$PERIOD,t$BNF.NAME),FUN=sum)
s<-merge(s,short.addresses,by.x="Group.1",by.y="V2",all.x=TRUE)
spend.practice<-rbind(spend.practice,s)
}  
  
total.problem.spend<-merge(total.problem.spend,problem.drugs,all.x=TRUE)
total.problem.spend$amount.wasted<-total.problem.spend$Spend*total.problem.spend$saving
wasted.totals<-aggregate(total.problem.spend[,c("Spend","Items","amount.wasted")],by=list("Drug"=total.problem.spend$Drug,"category"=total.problem.spend$category),FUN=sum)

## Calculate waste per practice
names(spend.practice)<-c("Practice.code","Month","Drug","ACT.COST","ITEMS","Practice.name","Town","Postcode")
spend.practice<-merge(spend.practice,problem.drugs,all.x=TRUE)
spend.practice$amount.wasted<-spend.practice$ACT.COST*as.numeric(spend.practice$saving)
spend.practice.totals<-
  aggregate(spend.practice[,c("ACT.COST","ITEMS","amount.wasted")],
            by=
              list("Drug"=spend.practice$Drug,
                   "Practice.name"=spend.practice$Practice.name,
                   "Town"=spend.practice$Town,
                   "Postcode"=spend.practice$Postcode,
                   "category"=spend.practice$category),
            FUN=sum)

# Read in postcode table and extend spend practice totals
#drv <- dbDriver("SQLite")
#db<-dbConnect(drv,"zip_and_post_codes.sqlite")
#postcodes<-dbGetQuery(db,"select * from codes where region='GB'")
#spend.practice.totals<-merge(spend.practice.totals,postcodes,all.x=TRUE)
#spend.practice.totals$latlong<-paste("'",spend.practice.totals$lat,":",spend.practice.totals$lng,"'",sep="")

# Maps for statins
statins<-cast(subset(spend.practice.totals,category=="statin"),Practice.name+Town+Postcode~Drug,value="amount.wasted",fun.aggregate=sum)
statins<-statins[,c("Practice.name","Town","Postcode","Atorvastatin","Rosuvastatin Calcium")]
t<-subset(spend.practice.totals,category=="statin")
t<-aggregate(t$ACT.COST,
             by=
               list("Practice.name"=t$Practice.name,
                    "Town"=t$Town,
                    "Postcode"=t$Postcode),
             FUN=sum)
statins<-merge(statins,t,all.x=TRUE)
names(statins)<-c("Surgery","Town","Postcode","Wasted.Spend.Atorvastatin","Wasted.Spend.Rosuvastatin","Total.Statin.Spend")

big.spenders.rosuva<-statins[order(statins$Wasted.Spend.Rosuvastatin,decreasing=TRUE),]
big.spenders.rosuva$Monthly.Waste<-round(big.spenders.rosuva$Wasted.Spend.Rosuvastatin/length(file.list),0) 

big.spenders.atorva<-statins[order(statins$Wasted.Spend.Atorvastatin,decreasing=TRUE),]
big.spenders.atorva$Monthly.Waste<-round(big.spenders.atorva$Wasted.Spend.Atorvastatin/length(file.list),0) 

PracticeGeoRosuva<-gvisGeoMap(big.spenders.rosuva[1:50,],
                        locationvar="Postcode", 
                        numvar="Monthly.Waste", 
                        hovervar="Surgery",
                        options=list(dataMode="markers",region="GB",
                                     showLegend=FALSE,
                                     width='600px',
                                     height='400px',
                                     colors='[0xFFFFFF,0xCC0000]'
                                     )
)
PracticeGeoAtorva<-gvisGeoMap(big.spenders.atorva[1:50,],
                              locationvar="Postcode", 
                              numvar="Monthly.Waste", 
                              hovervar="Surgery",
                              options=list(dataMode="markers",region="GB",
                                           showLegend=FALSE,
                                           width='600px',
                                           height='400px',
                                           colors='[0xFFFFFF,0xCC0000]'
                              )
                              #options=list(showTip=TRUE, mapType='normal',enableScrollWheel=TRUE)
)

plot(PracticeGeoRosuva)
plot(PracticeGeoAtorva)
#write.csv(spend.practice.totals,"Practice_spend_totals.csv",row.names=FALSE,quote=FALSE)
#write.csv(wasted.month,"Wasted_per_month.csv",row.names=FALSE,quote=FALSE)

# Maps for candesartan
sartans<-cast(subset(spend.practice.totals,Drug=="Candesartan Cilexetil"),Practice.name+Town+Postcode~Drug,value="amount.wasted",fun.aggregate=sum)
big.spenders.candesartan<-sartans[order(sartans[,4],decreasing=TRUE),]
big.spenders.candesartan$Monthly.Waste<-round(big.spenders.candesartan[,4]/length(file.list),0) 

PracticeGeoCandesartan<-gvisGeoMap(big.spenders.candesartan[1:50,],
                              locationvar="Postcode", 
                              numvar="Monthly.Waste", 
                              hovervar="Practice.name",
                              options=list(dataMode="markers",region="GB",
                                           showLegend=FALSE,
                                           width='600px',
                                           height='400px',
                                           colors='[0xFFFFFF,0xCC0000]'
                              )
)

plot(PracticeGeoCandesartan)

#p <- ggplot(wasted.month, aes(x=Period, y=amount.wasted, group=Drug)) 
#wasted.month$Period<-as.Date(paste(wasted.month$Period,"01",sep=""),'%Y%m%d')
#r <- p + geom_line(aes(colour = Drug))+scale_y_continuous(limits=c(0,25000000),breaks=c(0,5000000,10000000,15000000,20000000,25000000), labels=c("£0", "£5 million","£10 million", "£15 million","£20 million","£25 million"))
#ggsave(filename="Wasted_month.jpg",plot=r)

#spend.practice.totals$Percent.Rosuvastatin<-round(spend.practice.totals$Wasted.Spend.Rosuvastatin/spend.practice.totals$Total.problem.spend,3)
#spend.practice.totals$Percent.Atorvastatin<-round(spend.practice.totals$Wasted.Spend.Atorvastatin/spend.practice.totals$Total.problem.spend,3)

#rosuva.waste.percent<-subset(spend.practice.totals[order(spend.practice.totals$Percent.Rosuvastatin,decreasing=TRUE),],Total.problem.spend>5000)[1:50,]
#atorva.waste.percent<-subset(spend.practice.totals[order(spend.practice.totals$Percent.Atorvastatin,decreasing=TRUE),],Total.problem.spend>5000)[1:50,]

#write.csv(rosuva.waste.percent,"Rosuva_waste_percent.csv",row.names=FALSE,quote=FALSE)
#write.csv(atorva.waste.percent,"Atorva_waste_percent.csv",row.names=FALSE,quote=FALSE)