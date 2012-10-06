setwd("/Users/francinebennett/Desktop/analysis/NHS_analysis/")

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
viagra<-"Sildenafil (Erectile Dysfunction)"

problem.drugs<-as.data.frame(rbind(cbind(as.character(statins),"statin"),cbind(clopidogrel,"clopidogrel"),cbind(sartans,"sartan"),cbind(viagra,"viagra")))
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
#t<-subset(GP.drugs,BNF.NAME %in% problem.drugs$Drug)
t<-GP.drugs
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

write.csv(spend.practice,"spend_practice.csv",row.names=FALSE,quote=FALSE)
write.csv(problem.drugs,"problem_drugs.csv",row.names=FALSE,quote=FALSE)
write.csv(problem.spend,"problem_spend.csv",row.names=FALSE,quote=FALSE)