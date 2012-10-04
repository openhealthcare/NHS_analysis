setwd("/Users/francinebennett/Desktop/NHS_analysis/")

# Load packages
require(ggplot2)
require(plyr)
require(gdata)
require(tm)
require(reshape)

# Create dictionary of drugs terms
GP.drugs <- read.csv("T201204PDP IEXT.csv", header=TRUE)
drug.list<-unique(GP.drugs$BNF.NAME) # Create list of drug names using GP prescribing data
drugs.mentions<-read.csv("drug_list.txt",header=FALSE)
drugs.mentions$V1<-as.character(drugs.mentions$V1)
drug.list.temp<-as.data.frame(tolower(trim(drug.list)))
names(drug.list.temp)<-"V1"
drug.list.temp$V1<-as.character(drug.list.temp$V1)
drugs.mentions<-unique(rbind(drugs.mentions,drug.list.temp))
drugs.mentions$V1<-tolower(drugs.mentions$V1)
drugs.dictionary <- Dictionary(drugs.mentions$V1)

# Load data and remove duplicative fields
medication.incidents<-read.csv("Medication_incidents_sample.csv",sep="\t")
useful.columns<-c(
"Age.At.Time.Of.Incident",
#"Date.Incident.Received.By.Npsa", Not useful unless we know more about NPSA systems
#"Dv01.Patent.Age.At.Time.Of.Incident", Duplicated in "Age at time of incident"
#"Date.Record.Exported.To.Nrls.Cleansed",  Not useful
"In03.Location..lvl1.",
"Date.Of.Incident",
"In03.Location..lvl2.",
"In03.Location..lvl3.",
"In03.Location...Free.Text",
#"In05.Incident.Category...Lvl1", All "Medication" - remove
#"In05.Incident.Category...Lvl2", Blank - remove
#"In05.Incident.Category...Free.Text", Blank - remove
"IN07", #This is the description of the event
"In10.Actions.Preventing.Reoccurrence", 
"In11.Apparent.Causes",
"Md01.Med.Process",
#"Rp01.Unique.Incident.Id", Not relevant in this context
"Md01.Med.Process.Free.Text",           
"Md02.Med.Error.Category",
"Md02.Med.Error.Category.Free.Text",      
"Md05.Approved.Name..drug.1.",
"Md06.Proprietary.Name..drug.1.",            
"Md30.Approved.Name..drug.2.",
"Md31.Proprietary.Name..drug.2.",
#"No.Of.Patients.Who.Died", Duplicated in 'degree of harm'
#"No.Of.Patients...Who.Experienced.Low.Harm", Duplicated in 'degree of harm'
#"No.Of.Patients.Who.Experienced.Moderate.Harm", Duplicated in 'degree of harm'
#"No.Of.Patients.Who.Experienced.No.Harm", Duplicated in 'degree of harm'
#"Pd01.B.Patient.Age.Range", Duplicated by 'age at time of incident'
#"No.Of.Patients.Who.Experienced.Severe.Harm", Removing as duplicated in 'degree of harm' - no incidents with >1 patient
"Pd02.Patient.Sex",
"Pd05.Specialty...Lvl.1",
"Pd05.Specialty...Lvl.2", # These seem non-hierarchical
"Pd05.Speciality...Free.Text",
"Pd09.Degree.Of.Harm..severity....Display",
#"Pd20.Paediatric.Care", # Removing - only 4 non-blank, and those say 'no'
#"Rm04.Source.Of.Notification", Removing this column as not useful in this context
"Rp07.Nhs.Organisation.Code",
"Rp02.Care.Setting.Of.Occurrence",
#"Local.Trust.Incident.Id", Removing this column as not useful in this context
#"Re.coded.Degree.Of.Harm..Death.", Removing this column as all blank
#"Actual.Death", Duplicated in 'degree of harm' column
"Psi",
"Recommended.Exclusion",
#"Actual.Severe", Duplicated in 'degree of harm' column. Query - two columns where degree of harm 'severe' recorded as 'no' in this column
"Psi.S",
"Recommended.Exclusion.S",
"Strategic.Health.Authority.Code",
"Trust.Name",
"Strategic.Health.Authority.Name.",
"Pd11.Patient.Ethnic.Category",
#"Re.coded.Degree.Of.Harm..Severe.", Removing this column as all blank
"Staff.Type.0.Lvl1",
"Staff.Type.0.Lvl1..version2.",
"De01.Type.Of.Device",
"De01.Type.Of.Device...Free.Text"  
)
medication.incidents<-medication.incidents[,useful.columns]

# Combine staff classification columns
medication.incidents$Combined.Staff.Type<-
  paste(medication.incidents$Staff.Type.0.Lvl1,medication.incidents$Staff.Type.0.Lvl1..version2.)
medication.incidents<-
  medication.incidents[,!(names(medication.incidents) %in% c("Staff.Type.0.Lvl1","Staff.Type.0.Lvl1..version2."))]

# Make incident description into a text corpus & create documnet term matrix
incident.corpus<-Corpus(VectorSource(medication.incidents$IN07))
incident.corpus<-tm_map(incident.corpus,removeWords,stopwords("english"))
incident.corpus<-tm_map(incident.corpus,removeNumbers)
incident.corpus<-tm_map(incident.corpus,removePunctuation)
incident.corpus<-tm_map(incident.corpus,tolower)
dtm<-DocumentTermMatrix(incident.corpus,list(dictionary=drugs.dictionary))

# Label reports with drugs involved
intersections<-data.frame(matrix(nrow=0,ncol=1))

for (i in 1:length(incident.corpus)){
temp<-as.vector(unlist(strsplit(incident.corpus[[i]], " ")))
print(i)
temp.two<-intersect(as.vector(temp),as.vector(tolower(drugs.mentions$V1)))
print(temp.two)
intersections<-rbind(intersections,temp.two)
}

term.matrix<-as.matrix(dtm)
term.matrix<-term.matrix[,colSums(term.matrix)!=0]

term.df<-as.data.frame(inspect(dtm))
term.df[term.df>0]<-1
term.df<-term.df[,colSums(term.df)>1]
term.df<-term.df[rowSums(term.df)>0,]

drug.mixups<-data.frame(matrix(nrow=0,ncol=3))

for (i in 1:nrow(term.df)) {
  #t<-colnames(term.df[i,colSums(term.df[i,])>0])
  t<-colnames(term.df)[which(term.df[i,]>0)]
  t<-t[order(t)]
  temp<-c(NA,NA,NA)
  temp[1]<-t[1]
  temp[2]<-t[2]
  temp[3]<-t[3]
  drug.mixups[i,]<-temp
}


medication.incidents[grep("fluoxetine",tolower(medication.incidents$IN07)),
c("IN07","Md05.Approved.Name..drug.1.",
"Md06.Proprietary.Name..drug.1.",            
"Md30.Approved.Name..drug.2.",
"Md31.Proprietary.Name..drug.2.",
"Pd09.Degree.Of.Harm..severity....Display")]

drugrows<-row.names(term.df[term.df$fluoxetine!=0&term.df$flucloxacillin!=0,])
medication.incidents[drugrows,
                     c("IN07","Md05.Approved.Name..drug.1.",
                       "Md06.Proprietary.Name..drug.1.",            
                       "Md30.Approved.Name..drug.2.",
                       "Md31.Proprietary.Name..drug.2.",
                       "Pd09.Degree.Of.Harm..severity....Display")]

# Fluoxetine /fluoxacillin bad

# Add back drug names to original dataset
drug.mixups<-cbind(rownames(term.df),drug.mixups)
medication.incidents$drug.name.new.1<-NA
medication.incidents$drug.name.new.2<-NA
medication.incidents$drug.name.new.3<-NA
medication.incidents[rownames(term.df),c("drug.name.new.1","drug.name.new.2","drug.name.new.3")]<-drug.mixups[,c("X1","X2","X3")]

# Success checking
temp.gaps<-medication.incidents[,c("Md05.Approved.Name..drug.1.",
                                   "Md06.Proprietary.Name..drug.1.",            
                                   "Md30.Approved.Name..drug.2.",
                                   "Md31.Proprietary.Name..drug.2.",
                                   "drug.name.new.1","drug.name.new.2","drug.name.new.3","IN07")]
temp.gaps$all.meds<-paste(temp.gaps$Md05.Approved.Name..drug.1.,temp.gaps$Md06.Proprietary.Name..drug.1.,temp.gaps$Md30.Approved.Name..drug.2.,temp.gaps$Md31.Proprietary.Name..drug.2.)
temp.gaps$first.drug.found<-FALSE
temp.gaps$second.drug.found<-FALSE
temp.gaps$third.drug.found<-FALSE
temp.gaps$missing<-NA

for (i in 1:nrow(temp.gaps)){
  temp.gaps[i,"first.drug.found"]<-grepl(temp.gaps[i,"drug.name.new.1"],tolower(temp.gaps[i,"all.meds"]))
  temp.gaps[i,"second.drug.found"]<-grepl(temp.gaps[i,"drug.name.new.2"],tolower(temp.gaps[i,"all.meds"]))
  temp.gaps[i,"third.drug.found"]<-grepl(temp.gaps[i,"drug.name.new.3"],tolower(temp.gaps[i,"all.meds"]))
  temp.gaps$missing[i]<-min(temp.gaps[i,9],temp.gaps[i,10],temp.gaps[i,11],na.rm=TRUE) 
}

#temp.gaps<-temp.gaps[,c("all.meds","drug.name.new.1","drug.name.new.2","drug.name.new.3","first.drug.found","second.drug.found","third.drug.found","missing")]
temp.gaps[temp.gaps$missing==0,]$missing<-"missing"
temp.gaps[temp.gaps$missing!="missing",]$missing<-"not missing"

temp.interesting<-subset(temp.gaps,missing=="missing")

potential.mixup<-cbind(medication.incidents,temp.gaps$missing)
potential.mixup<-subset(potential.mixup,!is.na(drug.name.new.2))
names(potential.mixup)[ncol(potential.mixup)]<-"is.missing"
nrow(potential.mixup)

mixups.count<-unique(drug.mixups[duplicated(drug.mixups[,c(2:4)]),2:4])
mixups.count<-mixups.count[order(mixups.count$X1,mixups.count$X2),]
mixups.count$count<-1
#mixups.count$missing.count<-0

for (i in 1:nrow(mixups.count)){
  mixups.count[i,"count"]<-nrow(drug.mixups[drug.mixups$X1==mixups.count[i,1] & drug.mixups$X2==mixups.count[i,2] & drug.mixups$X3==mixups.count[i,3],])
#  mixups.count[i,"missing.count"]<-
#    nrow(medication.incidents[medication.incidents$drug.name.new.1==mixups.count[i,1] & 
#    medication.incidents$drug.name.new.2==mixups.count[i,2] & 
#    medication.incidents$drug.name.new.3==mixups.count[i,3],])
  }

mixups.count<-mixups.count[order(mixups.count$count,decreasing=TRUE),]
names(mixups.count)<-c("Drug.A","Drug.B","Drug.C","Count.of.incidents")
#subtract 1 as count function overcounts (not sure why, seems to pick up an "NA" row in response to any query!)
mixups.count$Count.of.incidents<-mixups.count$Count.of.incidents-1
mixups.count<-mixups.count[mixups.count$Count.of.incidents>1 & !is.na(mixups.count$Drug.B),]

for (i in 1:nrow(mixups.count)){
  mixups.count[i,"missing.from.metadata"]<-
    nrow(temp.gaps[temp.gaps$drug.name.new.1==mixups.count[i,"Drug.A"] & 
           temp.gaps$drug.name.new.2==mixups.count[i,"Drug.B"] & 
           temp.gaps$drug.name.new.3==mixups.count[i,"Drug.C"] & temp.gaps$missing=="missing",] )
}

## Annoying problem - 'missing' count seems correct but the total count looks wrong
