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
drugs.remove<-c("antibiotic","antibiotics")
drugs.mentions$V1<-tolower(drugs.mentions$V1)
drugs.mentions<-subset(drugs.mentions,!V1 %in% drugs.remove)
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

term.df<-as.data.frame(inspect(dtm))
term.df[term.df>0]<-1 ## Change from count to 'present/not present'
term.df<-term.df[,colSums(term.df)>1] ## Include only drugs that appear more than once
term.df<-term.df[rowSums(term.df)>0,] ## Include only reports that mention at least 1 drug - cuts count of incidents by ~40%

drug.mixups<-data.frame(matrix(nrow=0,ncol=3))
for (i in 1:nrow(term.df)) {
  t<-colnames(term.df)[which(term.df[i,]>0)]
  t<-t[order(t)]
  temp<-c(NA,NA,NA)
  temp[1]<-t[1]
  temp[2]<-t[2]
  temp[3]<-t[3]
  drug.mixups[i,]<-temp
}

# Add back drug names to original dataset
drug.mixups<-cbind(rownames(term.df),drug.mixups)
medication.incidents$drug.name.new.1<-NA
medication.incidents$drug.name.new.2<-NA
medication.incidents$drug.name.new.3<-NA
medication.incidents[rownames(term.df),c("drug.name.new.1","drug.name.new.2","drug.name.new.3")]<-drug.mixups[,c("X1","X2","X3")]

# Success checking
medication.incidents$all.meds<-paste(medication.incidents$Md05.Approved.Name..drug.1.,
                                     medication.incidents$Md06.Proprietary.Name..drug.1.,
                                     medication.incidents$Md30.Approved.Name..drug.2.,
                                     medication.incidents$Md31.Proprietary.Name..drug.2.)

medication.incidents$first.drug.found<-FALSE
medication.incidents$second.drug.found<-FALSE
medication.incidents$third.drug.found<-FALSE

for (i in 1:nrow(medication.incidents)){
  medication.incidents[i,"first.drug.found"]<-grepl(medication.incidents[i,"drug.name.new.1"],tolower(medication.incidents[i,"all.meds"]))
  medication.incidents[i,"second.drug.found"]<-grepl(medication.incidents[i,"drug.name.new.2"],tolower(medication.incidents[i,"all.meds"]))
  medication.incidents[i,"third.drug.found"]<-grepl(medication.incidents[i,"drug.name.new.3"],tolower(medication.incidents[i,"all.meds"]))
}

medication.incidents[is.na(medication.incidents$first.drug.found),]$first.drug.found<-1
medication.incidents[is.na(medication.incidents$second.drug.found),]$second.drug.found<-1
medication.incidents[is.na(medication.incidents$third.drug.found),]$third.drug.found<-1
medication.incidents$missing<-medication.incidents$first.drug.found*medication.incidents$second.drug.found*medication.incidents$third.drug.found

summary.count<-unique(medication.incidents[,c("drug.name.new.1","drug.name.new.2","drug.name.new.3","missing")])
summary.count[is.na(summary.count)]<-""
medication.incidents[is.na(medication.incidents)]<-""

t<-subset(medication.incidents,
          drug.name.new.1==summary.count[i,"drug.name.new.1"] &
            drug.name.new.2==summary.count[i,"drug.name.new.2"] &
            drug.name.new.3==summary.count[i,"drug.name.new.3"] &
            missing=summary.count[i,"missing"])

summary.count$count<-0
for (i in 1:nrow(summary.count)){
  t<-subset(medication.incidents,
            drug.name.new.1==summary.count[i,"drug.name.new.1"] &
              drug.name.new.2==summary.count[i,"drug.name.new.2"] &
              drug.name.new.3==summary.count[i,"drug.name.new.3"] &
              missing==summary.count[i,"missing"])
  summary.count[i,"count"]<-nrow(t)
  }

summary.count<-cast(summary.count,drug.name.new.1+drug.name.new.2+drug.name.new.3~missing)
names(summary.count)[4:5]<-c("untagged","tagged")
summary.count$total<-summary.count$untagged+summary.count$tagged
summary.count[is.na(summary.count)]<-0
summary.count$pct.missing<-round(summary.count$untagged/summary.count$total,3)
summary.count<-summary.count[order(summary.count$total,decreasing=TRUE),]

drugs.mixups<-subset(summary.count,drug.name.new.2!="")
pct.tagged<-sum(drugs.mixups$tagged)/sum(drugs.mixups$total)

dangerous.drugs<-unique(medication.incidents[!medication.incidents$Pd09.Degree.Of.Harm..severity....Display %in% c("No Harm","Low"),]$drug.name.new.1)
