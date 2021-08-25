###script 1

rm(list=ls())
#clear workspace 
setwd("~/Imperial/Project")
#set working directory 
getwd()
#check working directory 



library(lmtest)
library(lubridate)
library(dplyr)
library(ggplot2)
library(lme4)
library(usdm)
library(tidyr)
library(tidyr)
library(readxl)

library(plyr)
library(rptR)
library(reshape2)
library(MCMCglmm)

###f= reproductive floater
##n= Nr floater

db<-read.csv("Fecundity.csv")
names(db)
###import data 

##working out floater males 
##data for all birds born 
dc<-read.csv("Bird.Basic.Info.csv", header=T, na.strings = c("NA",""))
birds<-data.frame(dc$BirdID,dc$SexEstimate,dc$Cohort,dc$HatchDate,dc$LastLiveRecord)
birds.1<-filter(birds,dc.SexEstimate=="1")
##males only 
names(birds.1)
names(birds.1)[names(birds.1) == "dc.BirdID"] <- "BirdID"
names(birds.1)[names(birds.1) == "dc.SexEstimate"] <- "SexEstimate"
names(birds.1)[names(birds.1) == "dc.Cohort"] <- "Year"
names(birds.1)[names(birds.1) == "dc.HatchDate"] <- "HatchDate"
names(birds.1)[names(birds.1) == "dc.LastLiveRecord"] <- "Death"
##rename columns 
str(birds.1)
birds.1<-subset(birds.1,birds.1$Death!="NA")
is.na(birds.1$Death)
##remove values without last alive date
tail(birds.1)
birds.1<-filter(birds.1,Year<2020)

##remove 2019//2020 those birds not included in fecundity data.remove anything before 1998 also not in fecundity data 
birds.1<-subset(birds.1,birds.1$HatchDate!="NA")
##remove NA values in hatch date

birds.1$HatchDate<-as.factor(birds.1$HatchDate)
birds.1$HatchDate<-strptime(birds.1$HatchDate,format="%d/%m/%Y") 
#defining what is the original format of your date
birds.1$HatchDate<-as.Date(birds.1$HatchDate,format="%Y-%m-%d")
#defining new format as a date
birds.1$Death<-as.factor(birds.1$Death)
birds.1$Death<-strptime(birds.1$Death,format="%d/%m/%Y") 
#defining what is the original format of your date
birds.1$Death<-as.Date(birds.1$Death,format="%Y-%m-%d")
#defining new format as a date

Age <- interval(birds.1$HatchDate,birds.1$Death)
#calculating interval between birth and death
Age= Age%/% months(1)
#converting interval to be measured in months. gives age of bird at last known sighting in months

birds.2<-data.frame(birds.1,Age)
#add age column to dataframe
Deathyear<-substring(birds.2$Death,1,4) 
#This takes only characters in positions 1-4 of the date
Deathyear
birds.2<-data.frame(birds.2,Deathyear)
#add death year column to data frame

birds.2<-subset(birds.2,birds.2$Age!="0")
##anywhere age is 0 cannot be floaters too young to reproduce   
birds.2<-subset(birds.2,birds.2$Age>10)
##remove any birds that died at less than 10 months old too young to reproduce

didbreed<-data.frame(db$SocialDadID,db$BroodYear)
####condense into new dataframe

floaters.1 <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("BirdID", "Year")
colnames(floaters.1) <- x
#create empty dataframe with two columns bird id and year

for(i in birds.2$BirdID){
  a<-subset(birds.2,birds.2$BirdID==i)
  #set bird ID as i 
  b<-a$Year+1
  #set b start date, eyar from whihc could breed
  c<-a$Deathyear
  #set c as end date 
  d<-subset(didbreed,didbreed$db.SocialDadID==i)
  ##d is the bird id of only the birds that have bred
  e<-b:c
  #years in which could of bred 
  if(length(e)<1){}else{
    #if 0 do nothing otherwise continue. <1 died before 1st year 
    f<-d$db.BroodYear
    #f is 0 are floaters for whole life
    if(length(f)>0){
      g<-which(e %in% f)
      ##index years that match between could of bred and did breed
      h<-e[-g]
      ##years didn't breed
      ##whne h is 0 breed every year 
      if(length(h)>0){
        k<-data.frame(BirdID=i,Year=h)
        floaters.1<-rbind(floaters.1,k)
      }
      else{}
    }else {##if length of f is 0 
      l<-data.frame(BirdID=i, Year=e)
      floaters.1<-rbind(floaters.1,l)
      ##keeps adding birds in to the floaters dataframe
      
    }
  }}


#year+1 is the potential first year of breeding. print bird id could breed in year +1
##floaters.1 all individuals that were at some point/multiple point floater individuals 
total<-unique(floaters.1$BirdID)
##237 different individuals 
serial <- floaters.1 %>% group_by(BirdID) %>% filter(n()>1) 
serial
##shows only birds that were a floater for more than one year 
serial.table<-as.data.frame(table(serial$BirdID))
##shows for how many years in total each serial bird was a floater for



##include birthyear to calculate floater age 
unpairedmales <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("BirdID", "Year","BirthYear")
colnames(unpairedmales) <- x
#create empty dataframe with two columns bird id and year

for(i in birds.2$BirdID){
  a<-subset(birds.2,birds.2$BirdID==i)
  #set bird ID as i 
  b<-a$Year+1
  #set b start date, eyar from whihc could breed
  c<-a$Deathyear
  #set c as end date 
  d<-subset(didbreed,didbreed$db.SocialDadID==i)
  ##d is the bird id of only the birds that have bred
  e<-b:c
  #years in which could of bred 
  m<-a$Year
  if(length(e)<1){}else{
    #if 0 do nothing otherwise continue. <1 died before 1st year 
    f<-d$db.BroodYear
    #f is 0 are floaters for whole life
    if(length(f)>0){
      g<-which(e %in% f)
      ##index years that match between could of bred and did breed
      h<-e[-g]
      ##years didn't breed
      ##whne h is 0 breed every year 
      if(length(h)>0){
        k<-data.frame(BirdID=i,Year=h,BirthYear=m)
        unpairedmales<-rbind(unpairedmales,k)
      }
      else{}
    }else {##if length of f is 0 
      l<-data.frame(BirdID=i, Year=e,BirthYear=m)
      unpairedmales<-rbind(unpairedmales,l)
      ##keeps adding birds in to the floaters dataframe
      
    }
  }}
###adds birthyear to the dataset 
Age <- unpairedmales$Year-unpairedmales$BirthYear
unpairedmales.1<-data.frame(unpairedmales,Age)


s<-read.csv("PEDIGREE.csv")
s<-subset(s,s$Sire!="NA")

u<-unpairedmales.1

t<-u%>%
  mutate(both = ifelse(u$BirdID %in% s$Sire & 
                         u$Year %in% s$Cohort, 1,0))

###where both=1 that unpaired bird in a specific year also appears in the pedigree f
##for that year meaning that while unpaired it did still breed it was an EPM. 
##splits unpaired into floaters by reproductive and NR

t<-t%>%mutate(NonRep=case_when(both==0~"Y",both==1~"N"))

NonMales<-subset(t,NonRep=="Y")

names(s)
names(s)[names(s) == "ï..Offspring"] <- "Offspring"

names(t)[names(t) == "Year"] <- "Cohort"
####rename columns
d<- t%>%
  dplyr::select(BirdID, Cohort, both, NonRep) %>%
  mutate(BirdID = as.character(BirdID),
         Cohort = as.character(Cohort))
#####select columns 
p <- read.csv('PEDIGREE.csv')

p<-p%>% filter(Cohort >= 2001) 
###keep birds after 2001
p<-p%>% dplyr::select(Sire, Cohort, ï..Offspring, Dam)
##select certain columns 
p<-p%>% group_by(Sire, Cohort)
names(p)[names(p) == "ï..Offspring"] <- "Offspring"
##rename columns 
detach(package:plyr)
p<-p%>% summarise(
  n = length(Offspring)
)%>%   ungroup() %>%
  rename(BirdID = Sire) %>%
  mutate(BirdID = as.character(BirdID),
         Cohort = as.character(Cohort))
#####count offspring number for males 
##detach plyr stops dplyr working and means group by and summarise dont work 
names(p)

pi <- p %>% 
  left_join(d,p, by = c('BirdID','Cohort')) %>%
  rename(SocialPaired = NonRep) 
###add info on bird id and cohort  
pi$SocialPaired<-coalesce(pi$SocialPaired,"Y")


breedingmales<-pi
##rename dataframe
breedingmales<-dplyr::select(breedingmales,-both)

SocialPaired<-rep("N",times=81)
n<-rep("0",times=81)

#####replace n with 0 for socially unpaired males 

NonMales<-data.frame(NonMales,SocialPaired,n)

NonMales <- NonMales %>%
  rename(Cohort = Year)
NonMales<-dplyr::select(NonMales,BirdID,Cohort,SocialPaired,n)

males <- rbind(breedingmales, NonMales)

##combine dataframes

males <- males %>%
  mutate(SocialPaired = ifelse(SocialPaired == "N","F","P"))

males<-males %>% mutate(SocialPaired = ifelse(n == "0" & SocialPaired == "F", "N", SocialPaired))
####mark which strategy was used in which year 
males <- males %>%
  rename(Strategy =SocialPaired)

males <- males %>%
  rename(BreedingYear =Cohort)
###rename columns 
sum(males$Strategy=="F")

all<-read.csv("Bird.Basic.Info.csv")

##keep only males 

allmales<-subset(all,all$SexEstimate=="1")
allmales<-dplyr::select(allmales,BirdID:LastLiveRecord,-EggDate)
##remove unecessary columns 

##calculate age 
##remove NAS 
allmales<-subset(allmales,allmales$HatchDate!="NA")
allmales<-subset(allmales,allmales$LastLiveRecord!="NA")


allmales$HatchDate<-as.factor(allmales$HatchDate)
allmales$HatchDate<-strptime(allmales$HatchDate,format="%d/%m/%Y") 
#defining what is the original format of your date
allmales$HatchDate<-as.Date(allmales$HatchDate,format="%Y-%m-%d")

HatchYear<-format(allmales$HatchDate,format="%Y")
##add hatchyear to dataframe
allmales<-data.frame(allmales,HatchYear)

str(allmales)
Hatchyear<-as.numeric(allmales$HatchYear)
##change to numeric 
hatchdata<-dplyr::select(allmales,BirdID,HatchYear)

allmales$BirdID<-as.integer(allmales$BirdID)
males$BirdID<-as.integer(males$BirdID)
###columns need to be of same "type" numeric etc to join 
males<-males %>% 
  left_join(hatchdata,males, by = c('BirdID'))


str(males)
males$HatchYear<-as.integer(males$HatchYear)
males$BreedingYear<-as.integer(males$BreedingYear)
str(males)

Age<-males$BreedingYear-males$HatchYear

males<-data.frame(males,Age)

##remove na hatch year
males<-subset(males,males$HatchYear!="NA")


#############adult sex ratio 

##data for all birds born ##males and females 
dc<-read.csv("Bird.Basic.Info.csv", header=T, na.strings = c("NA",""))
basic<-data.frame(dc$BirdID,dc$SexEstimate,dc$Cohort,dc$HatchDate,dc$LastLiveRecord)
basic<-filter(birds,dc.SexEstimate!="NA")

names(basic)
names(basic)[names(basic) == "dc.BirdID"] <- "BirdID"
names(basic)[names(basic) == "dc.SexEstimate"] <- "SexEstimate"
names(basic)[names(basic) == "dc.Cohort"] <- "Cohort"
names(basic)[names(basic) == "dc.HatchDate"] <- "HatchDate"
names(basic)[names(basic) == "dc.LastLiveRecord"] <- "LastLiveRecord"
##rename columns 
str(basic)
basic<-subset(basic,basic$LastLiveRecord!="NA")
basic<-subset(basic,basic$HatchDate!="NA")
##remove values without last alive date


basic$HatchDate<-as.factor(basic$HatchDate)
basic$HatchDate<-strptime(basic$HatchDate,format="%d/%m/%Y") 
#defining what is the original format of your date
basic$HatchDate<-as.Date(basic$HatchDate,format="%Y-%m-%d")
#defining new format as a date
basic$LastLiveRecord<-as.factor(basic$LastLiveRecord)
basic$LastLiveRecord<-strptime(basic$LastLiveRecord,format="%d/%m/%Y") 
#defining what is the original format of your date
basic$LastLiveRecord<-as.Date(basic$LastLiveRecord,format="%Y-%m-%d")
#defining new format as a date

Age <- interval(basic$HatchDate,basic$LastLiveRecord)
#calculating interval between birth and death
Age= Age%/% months(1)
#converting interval to be measured in months. gives age of bird at last known sighting in months
basic<-data.frame(basic,Age)
#add age column to dataframe

basic<-subset(basic,basic$Age>10)
##anywhere age isless than 10 months too young to reproduce   
birds.2<-subset(birds.2,birds.2$Age>10)
##remove any birds that died at less than 10 months old too young to reproduce

##now basic contains all birds that lived to be old enough to reproduce 


LastLiveYear<-substring(basic$LastLiveRecord,1,4) 
#This takes only characters in positions 1-4 of the date
basic<-data.frame(basic,LastLiveYear)
#add last live year year column to data frame

##add a year to hatchdate to make year from first reproductive 
basic<-basic%>% mutate(FirstBreedingYear=HatchDate %m+% months(10) )

liveyears<-basic%>%
  rowwise() %>%
  do(data.frame(BirdID=.$BirdID,SexEstimate=.$SexEstimate, year=seq(.$FirstBreedingYear,.$LastLiveRecord,by="1 year")))

###adds a row for every year between start and end date

BreedingYears<-substring(liveyears$year,1,4) 
liveyears<-data.frame(liveyears,BreedingYears)
##take date into a year easier to use going forward

liveyears$BreedingYears<-as.character(liveyears$BreedingYears)


number.males<-liveyears %>% filter(SexEstimate=="1") %>% group_by(BreedingYears) %>% tally()
number.females<-liveyears %>% filter(SexEstimate=="0") %>% group_by(BreedingYears) %>% tally()
###count number of males/females alive in each year


names(number.females)[names(number.females) == "n"] <- "n.female"
names(number.males)[names(number.males) == "n"] <- "n.male"
###rename columns
adultsexratio<-number.females %>% 
  left_join(.,number.males, by = 'BreedingYears') 
##merge dataframes

sum(adultsexratio$n.female)
sum(adultsexratio$n.male)
##sum =2001 numbe rof values in live years

adultsexratio<-adultsexratio%>% mutate(ASR=n.male/n.female )
##number of males to every female
##yearly ratio of breeding age males to females added to dataframe 

##now include tally of number of N/p/F per year 

year.strategy<-males %>% filter(Strategy=="P") %>% group_by(BreedingYear) %>% tally()
names(year.strategy)[names(year.strategy) == "n"] <- "P"
year.strategy.2<-males %>% filter(Strategy=="N") %>% group_by(BreedingYear) %>% tally()
names(year.strategy.2)[names(year.strategy.2) == "n"] <- "N"
year.strategy.3<-males %>% filter(Strategy=="F") %>% group_by(BreedingYear) %>% tally()
names(year.strategy.3)[names(year.strategy.3) == "n"] <- "F"
##filter and tally by strategy 

year.strategy<-year.strategy %>% 
  left_join(.,year.strategy.2, by = 'BreedingYear') 
year.strategy<-year.strategy%>% 
  left_join(.,year.strategy.3, by = 'BreedingYear') 
#combine into single dataframe

#replace NA with 0
year.strategy[is.na(year.strategy)] <- 0

##make names equal 
names(year.strategy)[names(year.strategy) == "BreedingYear"] <- "BreedingYears"
str(adultsexratio)
year.strategy$BreedingYears<-as.character(year.strategy$BreedingYears)

adultsexratio<-adultsexratio%>% 
  left_join(.,year.strategy, by = 'BreedingYears') 
####merge dataframes

adultsexratio<-adultsexratio%>% mutate(Proportion.F=F/(N+F+P) )
adultsexratio<-adultsexratio%>% mutate(Proportion.N=N/(N+F+P) )
adultsexratio<-adultsexratio%>% mutate(Proportion.P=P/(N+F+P) )
##calculate proportion of each breeding strategy 

r.adultsexratio<-dplyr::select(adultsexratio,BreedingYears,ASR,Proportion.F:Proportion.P)
##reduce data set

##no proportion data fro 2020 remove row 
r.adultsexratio<-r.adultsexratio[-c(20),]



asr.m<-glm(formula=Proportion.F~ASR,family="binomial",data=r.adultsexratio)
summary(asr.m)
###not significant 
###validation 
### pseudo R2 
1-(0.81272/0.85802)
##0.05279597 
##dispsersion 
0.82172/17
####0.04 
##model is underdispersed 

plot(asr.m)

###qq plot looks normal 


asr.m2<-glm(formula=Proportion.N~ASR,family="binomial",data=r.adultsexratio)
summary(asr.m2)
###not significant 
##pseuod r2 
1-(0.76112/0.83066)
##0.0837 

##dispersion 
0.76112/17
###0.0447 

plot(asr.m2)
##QQ plot pretty good. no outliers on residual vs leverage 

##what about combined proportions of floaters and NR. in a year?

adultsexratio<-adultsexratio%>% mutate(Proportion.AF=(F+N)/(N+F+P) )
##all floaating strategies botoh floaters and NR 
r.adultsexratio<-dplyr::select(adultsexratio,BreedingYears,ASR,Proportion.F:Proportion.AF)
##reduce data set

##no proportion data fro 2020 remove row 
r.adultsexratio<-r.adultsexratio[-c(20),]

asr.m3<-glm(formula=Proportion.AF~ASR,family="binomial",data=r.adultsexratio)
summary(asr.m3)

##pseudo R2 
1-(1.0098/1.1313)
###0.107 very low 
##dispersion
1.0098/17
##0.0594 underdispersed 
plot(asr.m3)
##QQ plot pretty normal no outliers on residual vs leverage but other plots 



####################################relative stage of offspring ######################
###find out relative number of fledglings

all 
##all bird basic data 

offspring<-dplyr::select(all,BirdID,NatalBrood,LastStage,HatchDate,Fostered)
##remove any NAS in the data 
offspring<- na.omit(offspring)

##make hatchdate a year 
offspring$HatchDate<-as.factor(offspring$HatchDate)
offspring$HatchDate<-strptime(offspring$HatchDate,format="%d/%m/%Y") 
#defining what is the original format of your date
offspring$HatchDate<-as.Date(offspring$HatchDate,format="%Y-%m-%d")
HatchYear<-substring(offspring$HatchDate,1,4) 
offspring<-data.frame(offspring,HatchYear)


ped<-read.csv("PEDIGREE.csv")
##only have bird basic info from 2000 onwards 
ped<-subset(ped,ped$Cohort>1999)

##remove where male ID not known 
ped<-subset(ped,ped$Sire!="NA")

##remove dam and immigrant
ped<-dplyr::select(ped,-Dam,-Immigrant)

names(ped)
names(ped)[names(ped) == "ï..Offspring"] <- "OffspringID"
names(offspring)[names(offspring) == "BirdID"] <- "OffspringID"
names(offspring)[names(offspring) == "HatchYear"] <- "Cohort"
##rename columns 

offspring<-dplyr::select(offspring,-HatchDate)
##remove hatchdate

str(offspring)
ped$Cohort<-as.character(ped$Cohort)

offspring<-offspring %>% 
  left_join(.,ped, by = c('Cohort',"OffspringID"))

##remove NAs where father is unknown 
offspring<-subset(offspring,offspring$Sire!="NA")
##inlcude male strategy per year 

malesreduced<-malesreduced<-dplyr::select(males,BirdID,BreedingYear,Strategy)
strategyinfo<-malesreduced

##changes names and character state to allow merge
names(strategyinfo)[names(strategyinfo) == "BreedingYear"] <- "Cohort"
names(strategyinfo)[names(strategyinfo) == "BirdID"] <- "Sire"

str(offspring)
strategyinfo$Cohort<-as.character(strategyinfo$Cohort)
strategyinfo$Sire<-as.integer(strategyinfo$Sire)

offspring<-offspring %>% 
  left_join(.,strategyinfo, by =c('Sire',"Cohort"))
##remove where male strategy is unknown 
offspring<- na.omit(offspring)

##now look at stage 3 (fledged) offspring 
##group by male, natal brood, cohort 
offspring.3<-subset(offspring,offspring$LastStage=="3")
m.offspring.3 <- offspring.3 %>% group_by(Sire,Cohort,NatalBrood) %>%
  summarise(
    fledgingnumber = length(OffspringID)
  )%>%ungroup()

##now add in male strategy
m.offspring.3<-m.offspring.3 %>% 
  left_join(.,strategyinfo, by =c('Sire',"Cohort"))

hist(m.offspring.3$fledgingnumber)
##LHS skew large number of 1 values 
ggplot(m.offspring.3,aes(x=Strategy,y=fledgingnumber))+geom_boxplot()
##means look very similar range of paired much greater 
t.test(fledgingnumber~Strategy,m.offspring.3)
##means in F and p significantly different both over 1 


str(m.offspring.3)
m.offspring.3$Cohort<-as.numeric(m.offspring.3$Cohort)


F3M3<-glmer(fledgingnumber~Strategy+(1|Sire)+(1|Cohort)+(1|NatalBrood),family="poisson",data=m.offspring.3)
summary(F3M3)
plot(F3M3)
##produce unlikely values, all below one when are not values below one
F3M2<-glmer(fledgingnumber~Strategy+(1|Sire)+(1|Cohort),family="poisson",data=m.offspring.3)
summary(F3M2)
####linear mixed model appears to better fit the data
F3M1<-lmer(fledgingnumber~Strategy+(1|Sire)+(1|Cohort),data=m.offspring.3)
summary(F3M1)
plot(F3M1)
###estimates seem much more likely are above 1 

F3M4<-lmer(fledgingnumber~Strategy+(1|Sire)+(1|Cohort)+(1|NatalBrood),data=m.offspring.3)
summary(F3M4)
###now value for sire variance rather than 0
##better model 
##maximal model 
plot(F3M4)


##compare models using lrtest 
lrtest(F3M1,F3M4)
###F3M4 is significantly better than F3M1 but not by a lot  

F3M5<-MCMCglmm(fledgingnumber~Strategy,random=~Sire+Cohort+NatalBrood, family="gaussian",data=m.offspring.3,verbose=TRUE)
###run as mcmc glmm for greater number of iterations check result 
summary(F3M5)

rpt(fledgingnumber ~Strategy+(1|Sire)+(1|Cohort)+(1|NatalBrood), grname = c("Sire","Cohort","NatalBrood"), data = m.offspring.3, datatype = "Gaussian", 
    nboot = 0, npermut = 0)


#####Strategy type and LRS ###############################################

str(m.offspring.3)
##need to add NR individuals to be able to include N as a strategy that might later improve LRS
##
Nonly<-subset(malesreduced,malesreduced$Strategy=="N")
##change breeding year to cohort 
names(Nonly)[names(Nonly) == "BreedingYear"] <- "Cohort"
##add columns to make it easier to merge. fledgling number and brood are 0 as these indivudlas didnt breed 
Nonly <- data.frame(fledgingnumber = c(0), Nonly)
Nonly <- data.frame(NatalBrood = c(0), Nonly)
##rename bird ID to sire 
names(Nonly)[names(Nonly) == "BirdID"] <- "Sire"
##merge non reproductive males with male fledging success
N.offspring.3<-rbind(m.offspring.3,Nonly)


newdata= data.frame(matrix(vector(), 0, 3,
                           dimnames=list(c(), c("Sire", "FledgingNumber", "Strategy"))),
                    stringsAsFactors=F)
h<-unique(N.offspring.3$Sire)
###this way will get the number of rows in new data for unique bird ID rather than all the rows in orignal 
##dataset 

for( i in h) { 
  a<-subset(N.offspring.3,N.offspring.3$Sire==i)
  ##set sire as i
  
  b<-a$Strategy 
  ##look at strategies used by each sire
  
  c ="N"
  d ="F"
  if(c %in% b==TRUE) {
    e="N"} else{
      if(d %in% b==TRUE){e="F"}else {
        e="P"}}
  
  f<-sum(a$fledgingnumber)
  ##count fledging number for each sire 
  
  g<-data.frame(Sire=i, FledgingNumber=f,Strategy=e)
  newdata<-rbind(newdata,g)
  ##keep adding to new dataframe
}

###now look so see if lifetime fledging success is different if were either
##f or p at some point in life 


##include age older birds would expect to have had more fledgings in lifetime 

maleage<-dplyr::select(allmales,BirdID,HatchDate:LastLiveRecord,Cohort)

##change bird ID to sire so the dataframes will merge 
names(maleage)[names(maleage) == "BirdID"] <- "Sire"
str(maleage)
str(newdata)

newdataage<- newdata%>% left_join(maleage, by=c("Sire"))

newdataage$LastLiveRecord<-as.factor(newdataage$LastLiveRecord)
newdataage$LastLiveRecord<-strptime(newdataage$LastLiveRecord,format="%d/%m/%Y") 
#defining what is the original format of your date
newdataage$LastLiveRecord<-as.Date(newdataage$LastLiveRecord,format="%Y-%m-%d")

LastLiveYear<-substring(newdataage$LastLiveRecord,1,4) 
newdataage<-data.frame(newdataage,LastLiveYear)

newdataage$HatchDate<-as.factor(newdataage$HatchDate)

#defining what is the original format of your date
newdataage$HatchDate<-as.Date(newdataage$HatchDate,format="%Y-%m-%d")

######now calculate age 

LastAge<-newdataage$LastLiveRecord-newdataage$HatchDate
LastAge<-LastAge/365
##age in years 
LastAge
newdataage<-data.frame(newdataage,LastAge)

##remove values where birds too young 
##misidentifed parentage too young to be parents 
newdataage[-c(7,277,114,338),]
str(newdataage)

autocorr(newdataage)
hist(newdataage$FledgingNumber)
###strong LHS skew 
##lots of 0 potential for zero inflation
ggplot(newdataage,aes(x=Strategy,y=FledgingNumber))+geom_boxplot()
##p is very varied, n mean looks much lower than f or p which look quite similar 
library(plyr)
LFSmean<-ddply(newdataage,~Strategy,summarise,mean=mean(FledgingNumber),sd=sd(FledgingNumber))
LFSmean
##mean N ~0 means f and P over 6
detach(package:plyr)
newdatage<-filter(newdataage,LastAge >= 1.0)


LRSm2<-glmer(FledgingNumber~Strategy+LastAge+(1|Sire)+(1|Cohort),data=newdatage,family="poisson")

summary(LRSm2)
###number of warnings model failed to converge values dont match what would expect/reflect means 
LRSm3<-glmer(FledgingNumber~Strategy*LastAge+(1|Sire)+(1|Cohort),data=newdatage,family="poisson")

summary(LRSm3)
####not an interaction age is just there to account for more opportunity to have chicks
##so doesnt need to be an interaction 

LRSm5<-lmer(FledgingNumber~Strategy+LastAge+(1|Cohort),data=newdatage)
summary(LRSm5)

###remove largest fledging values -potential outliers
newdataage[-c(153,32),]

###run as mcmc glmm so can include sire as random effect 
LFRSM7<-MCMCglmm(FledgingNumber~Strategy+LastAge,random=~Sire+Cohort, family="gaussian",data=newdatage,verbose=TRUE)
summary(LFRSM7)


##########################is last age impacted by lifetime strategy ##########################
hist(newdataage$LastAge)
##looks normal 
ggplot(newdataage,aes(x=Strategy,y=LastAge))+geom_boxplot()
##mean age of N looks much less than F or P, F appears to be oldest but not obvious if
#significantly so 
##look at mean last live age for each category 
lastagemean<-ddply(newdataage,~Strategy,summarise,mean=mean(LastAge),sd=sd(LastAge))
###all last ages similar all between 2 and 3

newdataage$LastAge<-as.numeric(newdataage$LastAge)
str(newdataage)

LAm2<-lmer(LastAge~Strategy+(1|Cohort),data=newdataage)
summary(LAm2)
##include fledging number maybe effort of raising offspring reduces lifespan? or more offspring 
##better quality male lives longer 

LAM3<-lmer(LastAge~Strategy+(1|Cohort)+(1|FledgingNumber),data=newdataage)
summary(LAM3)
##seems like a better model fledging number explains a lot of last age 
##P becomes significant here 
##N becomes older than F though?
####numbers dont fit with means though LAM2 is the better model 
lrtest(LAm2,LAM3)
##lam3 significantly better explains more varaiation 
##i think fledging number should be fixed rather than random 

LAM6<-lmer(LastAge~Strategy+FledgingNumber+(1|Cohort),data=newdataage)
summary(LAM6)
lrtest(LAM3,LAM6)
###lam6 significantly better 

LAM4<-MCMCglmm(LastAge~Strategy+FledgingNumber,random=~Cohort,family="gaussian",data=newdataage,verbose=TRUE)

str(newdataage)
summary(LAM4)

rpt(LastAge ~Strategy+FledgingNumber+(1|Cohort), grname = "Cohort", data = newdataage, datatype = "Gaussian", 
    nboot = 0, npermut = 0)

############strategy and age######################


strategyinfo$Sire<-as.character(strategyinfo$Sire)
names(strategyinfo)[names(strategyinfo) == "Sire"] <- "BirdID"
names(strategyinfo)[names(strategyinfo) == "Cohort"] <- "BreedingYear"

Cohort<-birds.1%>%dplyr::select(BirdID,Year)
Cohort$BirdID<-as.character(Cohort$BirdID)
strategyage<- strategyinfo%>% left_join(Cohort, by=c("BirdID"))
names(strategyage)[names(strategyage) == "Year"] <- "HatchYear"
###rename columns 
#calculate age
str(strategyage)
strategyage$HatchYear<-as.numeric(strategyage$HatchYear)
strategyage$BreedingYear<-as.numeric(strategyage$BreedingYear)
Age<-strategyage$BreedingYear-strategyage$HatchYear
###calculate age 
strategyage<-data.frame(strategyage,Age)
strategyage$Age[which(is.nan(strategyage$Age))] = NA
strategyage[which(strategyage==Inf)] = NA
strategyage<- na.omit(strategyage)
str(strategyage)
sum(is.na(strategyage))
sum(is.finite(strategyage$Strategy))
####remove NAs


hist(males$Age)
##no 0 values 

agemean<-males%>%group_by(Strategy)%>%summarise(mean=mean(Age),sd=sd(Age))
agemean

ggplot(males,aes (x=Strategy,y=Age))+geom_boxplot()
##n appear to be signifcantly younger than F or P 
##F and P look very similar but P has much greater range 



am4<-lmer(Age~Strategy+(1|BirdID)+(1|HatchYear),data=males)
summary(am4)
###maximal model

am3<-lmer(Age~Strategy+(1|BirdID),data=males)
summary(am3)
####

lrtest(am3,am4)
###am4 significantly better 

am5<-MCMCglmm(Age~Strategy,random=~BirdID+HatchYear,family="gaussian",data=males,verbose=TRUE)
###run as mcmcglmm to check results 
summary(am5)

rpt(Age ~Strategy+(1|BirdID)+(1|HatchYear), grname = c("BirdID","HatchYear"), data = males, datatype = "Gaussian", 
    nboot = 0, npermut = 0)



#################looking at visible bib ###############################################
bib<-read.csv("bird.markings.csv")

##remove any females 
bib<-subset(bib,bib$SexEstimate=="1")

##remove N/As in capture date and bird ID
bib<-subset(bib,bib$CaptureDate!="NA")
bib<-subset(bib,bib$BirdID!="NA")
bib<-subset(bib,bib$Observer!="NA")

##now remove unecessary columns, only males so dont need sex column 
bib<-dplyr::select(bib,CaptureDate,BirdID,Mass:Observer)


##add year value to make it easier 
bib$CaptureDate<-as.factor(bib$CaptureDate)
bib$CaptureDate<-strptime(bib$CaptureDate,format="%d/%m/%Y") 
#defining what is the original format of your date
bib$CaptureDate<-as.Date(bib$CaptureDate,format="%Y-%m-%d")
CaptureYear<-substring(bib$CaptureDate,1,4) 
bib<-data.frame(bib,CaptureYear)


malesreduced<-dplyr::select(males,BirdID,BreedingYear,Strategy)
##rename column so they will merge
names(malesreduced)[names(malesreduced) == "BreedingYear"] <- "CaptureYear"
bib$CaptureYear<-as.integer(bib$CaptureYear)
str(bib)
malesreduced$BirdID<-as.character(malesreduced$BirdID)
malesreduced$CaptureYear<-as.integer(malesreduced$CaptureYear)
bib$BirdID<-as.character(bib$BirdID)

bib.1 <- bib%>% left_join(malesreduced, by=c("BirdID","CaptureYear"))
###add straegy to bib data 

##remove anyNAs
bib.1<-subset(bib.1,bib.1$Strategy!="NA")
##only VB males
bib.1<-subset(bib.1,bib.1$Trait=="VB")


##adding seasons to dates 

bib.1$CaptureMonth =format(bib.1$CaptureDate, format="%m")

bib.1$CaptureMonth<-as.integer(bib.1$CaptureMonth)

bib.1<-bib.1%>%
  mutate(
    Season = case_when(
      CaptureMonth %in%  3:8 ~ "Breeding",
      TRUE ~ "Winter"))




#solve duplicate rows get mean of bib estimate 

bib.2<- with(bib.1, aggregate(list(Estimate), by = list(CaptureDate=CaptureDate,BirdID=BirdID), 
                              FUN =function(x) { mon.mean = mean(x, na.rm = TRUE) }))
names(bib.2)
##rename mean column 
names(bib.2)[names(bib.2) == "c.23..23.20000076..23.39999962..54.59999847..53.20000076..54.."] <- "BibEstimate"


##slimline bib.1 
bib.s<-dplyr::select(bib.1,BirdID,CaptureDate,Observer,CaptureYear,Strategy,Season)
##now remove duplicates in bib.s
bib.s <- bib.s[!duplicated(bib.s[c('CaptureDate', 'BirdID')]),] 

##join the 2 datasets 
bib.2 <- bib.s%>% left_join(bib.2, by=c("BirdID","CaptureDate"))
###now has bib estimate 

##include hatch year to get age 
birthyear<-dplyr::select(allmales,BirdID,HatchDate,HatchYear)
str(birthyear)
birthyear$BirdID<-as.character(birthyear$BirdID)

bib.2 <- bib.2%>% left_join(birthyear, by=c("BirdID"))
##add birthyear 

str(bib.2)
bib.2$HatchYear<-as.integer(bib.2$HatchYear)
#calculate age
Age<-bib.2$CaptureDate-bib.2$HatchDate
Age<-Age/365
##give age in years
Age<-as.numeric(Age)

bib.2<-data.frame(bib.2,Age)
##remove any NA 

bib.2<- na.omit(bib.2)

###now change where the year starts and ends 
str(bib.2)
#split capture date into 3 one col for day month and year 

bib.3<- bib.2%>%separate(CaptureDate,c("Year","Month","Day"),sep="-")

###set it so that capture year +1 is used for wintering (septemeber to december)but not breeding that way,
#dates will match up for wintering predicting breeding. 
bib.4= data.frame(matrix(vector(), 0, 3,
                         dimnames=list(c(), c("BirdID", "AdjustedCaptureYear", "Age"))),
                  
                  stringsAsFactors=F)

str(bib.3)
bib.3$CaptureYear<-as.integer(bib.3$CaptureYear)
bib.3$Month<-as.integer(bib.3$Month)

for(i in 1:nrow(bib.3)){ 
  ##each row as i
  
  b=bib.3[i,"Month"]
  c=bib.3[i,"CaptureYear"]
  d=bib.3[i,"Age"]
  e=bib.3[i,"BirdID"]
  if(b >=9){c=c+1} else {c=c}
  
  g<-data.frame(BirdID=e, AdjustedCaptureYear=c,Age=d)
  bib.4<-rbind(bib.4,g)
  ##keep adding to new dataframe
}

##add adjusted capture year to data 
bib.3<-data.frame(bib.3,bib.4$AdjustedCaptureYear)

names(bib.3[9])                         

hist(bib.3$BibEstimate)
###reasonably normal

ggplot(bib.2, aes(x=Strategy, y=BibEstimate, fill=Strategy)) + 
  geom_boxplot()

BE<-lmer(BibEstimate~Strategy+Season+Age+(1|BirdID)+(1|Observer)+(1|bib.4.AdjustedCaptureYear),data=bib.3)
summary(BE)
####maximal model
####all random effect biologiclaly meaningful 

BE2<-MCMCglmm(BibEstimate~Strategy+Season+Age,random=~BirdID+Observer+bib.4.AdjustedCaptureYear,family="gaussian",data=bib.3,verbose=TRUE)
summary(BE2)

rpt(BibEstimate ~Strategy+Season+Age+(1|BirdID)+(1|Observer)+(1|bib.4.AdjustedCaptureYear), grname = c("BirdID","Observer","bib.4.AdjustedCaptureYear"), 
    data = bib.3, datatype = "Gaussian", nboot = 0, npermut = 0)

length(unique(bib.3$BirdID))

###########looking at hidden badge #########################################

bib.11 <- bib%>% left_join(malesreduced, by=c("BirdID","CaptureYear"))
###add straegy to bib data 

##remove anyNAs
bib.11<-subset(bib.11,bib.11$Strategy!="NA")
##only VB males
bib.11<-subset(bib.11,bib.11$Trait=="HB")


##adding seasons to dates 

bib.11$CaptureMonth =format(bib.11$CaptureDate, format="%m")

bib.1$CaptureMonth<-as.integer(bib.1$CaptureMonth)

bib.11<-bib.11%>%
  mutate(
    Season = case_when(
      CaptureMonth %in%  3:8 ~ "Breeding",
      TRUE ~ "Winter"))


#solve duplicate rows get mean of bib estimate 
bib.12<-aggregate(bib.11,Estimate,by=list(c(CaptureDate=CaptureDate,BirdID=BirdID),FUN=mean))
bib.12<-aggregate(bib.11[, 2,5:10], list(bib.11$CaptureDate,bib.11$BirdID), mean)
bib.12<- with(bib.11, aggregate(list(Estimate), by = list(CaptureDate=CaptureDate,BirdID=BirdID), 
                                FUN =function(x) { mon.mean = mean(x, na.rm = TRUE) }))
names(bib.12)
##rename mean column 
names(bib.12)[names(bib.12) == "c.36.59999847..35.09999847..36.40000153..36.40000153..39.20000076.."] <- "BibEstimate"


##slimline bib.1 
bib.1s<-dplyr::select(bib.11,BirdID,CaptureDate,Observer,CaptureYear,Strategy,Season)
##now remove duplicates in bib.s
bib.1s <- bib.s[!duplicated(bib.s[c('CaptureDate', 'BirdID')]),] 

##join the 2 datasets 
bib.12 <- bib.1s%>% left_join(bib.12, by=c("BirdID","CaptureDate"))
###now has bib estimate 

##include hatch year to get age 
birthyear1<-dplyr::select(allmales,BirdID,HatchDate,HatchYear)
str(birthyear1)
birthyear1$BirdID<-as.character(birthyear1$BirdID)

bib.12 <- bib.12%>% left_join(birthyear1, by=c("BirdID"))
##add birthyear 

str(bib.12)
bib.12$HatchYear<-as.integer(bib.12$HatchYear)
#calculate age
Age1<-bib.12$CaptureDate-bib.12$HatchDate
Age1<-Age1/365
##give age in years
Age1<-as.numeric(Age1)

bib.12<-data.frame(bib.12,Age1)
##remove any NA 

bib.12<- na.omit(bib.12)

###now change where the year starts and ends 
str(bib.12)
#split capture date into 3 one col for day month and year 

bib.13<- bib.12%>%separate(CaptureDate,c("Year","Month","Day"),sep="-")

###set it so that capture year +1 is used for wintering (septemeber to december)but not breeding that way,
#dates will match up for wintering predicting breeding. 
bib.14= data.frame(matrix(vector(), 0, 3,
                          dimnames=list(c(), c("BirdID", "AdjustedCaptureYear", "Age1"))),
                   
                   stringsAsFactors=F)

str(bib.13)
bib.13$CaptureYear<-as.integer(bib.13$CaptureYear)
bib.13$Month<-as.integer(bib.13$Month)

for(i in 1:nrow(bib.13)){ 
  ##each row as i
  
  b=bib.13[i,"Month"]
  c=bib.13[i,"CaptureYear"]
  d=bib.13[i,"Age1"]
  e=bib.13[i,"BirdID"]
  if(b >=9){c=c+1} else {c=c}
  
  g<-data.frame(BirdID=e, AdjustedCaptureYear=c,Age1=d)
  bib.14<-rbind(bib.14,g)
  ##keep adding to new dataframe
}

##add adjusted capture year to data 
bib.13<-data.frame(bib.13,bib.14$AdjustedCaptureYear)
####got this far ######
names(bib.13[9])                         

hist(bib.13$BibEstimate)
###very normal
ggplot(bib.13,aes(x=Strategy,y=BibEstimate))+geom_boxplot()
###overlap between all three look v similar

ggplot(bib.12, aes(x=Strategy, y=BibEstimate, fill=Strategy)) + 
  geom_boxplot()

BEE1<-lmer(BibEstimate~Strategy+Season+Age1+(1|BirdID)+(1|Observer)+(1|bib.14.AdjustedCaptureYear),data=bib.13)
summary(BEE1)
#####maximal model 
###all random effects biologically meaningful

BEE2<-MCMCglmm(BibEstimate~Strategy+Season+Age1,random=~BirdID+Observer+bib.14.AdjustedCaptureYear,family="gaussian",data=bib.13,verbose=TRUE)
summary(BEE2)

rpt(BibEstimate ~Strategy+Season+Age1+(1|BirdID)+(1|Observer)+(1|bib.14.AdjustedCaptureYear), grname = c("BirdID","Observer","bib.14.AdjustedCaptureYear"), 
    data = bib.13, datatype = "Gaussian", nboot = 0, npermut = 0)

length(unique(bib.13$BirdID))

#############basic stats #####

length(unique(strategyinfo$Sire))
##525 individual
str(strategyinfo)

(table(strategyinfo$Strategy))
##158 floaters, 81 N 839 P

####work out how many males were exlusively paired only 

strategycondensed= data.frame(matrix(vector(), 0, 2,
                                     dimnames=list(c(), c("BirdID", "Strategy"))),
                              
                              stringsAsFactors=F)
##create empty dataframe
z<-strategyinfo

for( i in unique(z$BirdID)) {  
  a<-subset(z,z$BirdID==i)
  ##set ID as i
  
  b<-a$Strategy
  ##look at strategies used by each sire
  
  if(b %in% "F"){
    
    c = "F"}else{
      
      if(b %in% "N"){
        
        c="N"
        
      }else{
        
        c = "P"
        
      }
    }
  d<-data.frame(BirdID=i,Strategy=c)
  strategycondensed<-rbind(strategycondensed,d)
  ##keep adding to new dataframe
}

###if not p alway through marks as either f or p can see which males were paired throughout
##their life time 

(table(strategycondensed$Strategy))
##86 floated at some point in life 
##54 non R
###385 exclusively paired 