library(plyr) #load necessary libraries
library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)

download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2","StormData.csv.bz2")
StormData_RAW<-read.csv("StormData.csv.bz2") #download and load data
StormData<-StormData_RAW #store RAW data for reference 
str(StormData) #look at data

######################
# Cleaning Event Types
######################
EVTYPE_official<-c("astronomicallowtide","avalanche","blizzard","coastalflood","coldwindchill", #load official EVTYPE list
                   "debrisflow","densefog","densesmoke","drought","dustdevil","duststorm",   #for comparison
                   "excessiveheat","extremecoldwindchill","flashflood","flood","freezingfog",
                   "frostfreeze","funnelcloud","hail","heat","heavyrain","heavysnow","highsurf",
                   "highwind","hurricanetyphoon","icestorm","lakeeffectsnow","lakeshoreflood",
                   "lightning","marinehail","marinehighwind","marinestrongwind","marinethunderstormwind",
                   "ripcurrent","seiche","sleet","stormsurgetide","strongwind","thunderstormwind",
                   "tornado","tropicaldepression","tropicalstorm","tsunami","volcanicash",
                   "waterspout","wildfire","winterstorm","winterweather")

StormData$EVTYPE<-tolower(StormData$EVTYPE) #make EVTYPE lowercase to narrow down options
StormData$EVTYPE<-gsub("[^A-Za-z]","",StormData$EVTYPE) #remove all non alphabetic characters to narrow further
for(i in 1:length(EVTYPE_official)){ #sub in official EVTYPE for all EVTYPES that are similar
    StormData$EVTYPE<-gsub(paste0(EVTYPE_official[i],".*"),paste0(EVTYPE_official[i]),StormData$EVTYPE)
}

check1<-table(StormData$EVTYPE) #create table of all remaining EVTYPEs
check1<-check1[!(names(check1) %in% EVTYPE_official)] #narrow table to only unofficial EVTYPEs
check1<-check1[order(check1,decreasing=TRUE)] #sort table in order of decreasing EVTYPE frequency
print(sum(check1)/nrow(StormData)*100) #see what percent of the EVTYPE data is still uncleaned
print(head(check1,10)) #see what the top 10 uncleaned EVTYPES are, to check clustering
print(sort(names(check1))) #get list of all unofficial EVTYPES remaining

#use data documentation to fit top unofficialEVTYPES (and other similar ones) into official categories
StormData$EVTYPE<-gsub("marinetstm.*","marinethunderstormwind",StormData$EVTYPE)
StormData$EVTYPE<-gsub("tstm.*|thunder.*","thunderstormwind",StormData$EVTYPE)
StormData$EVTYPE<-gsub("urban.*|riverflood.*","flood",StormData$EVTYPE)
StormData$EVTYPE<-gsub("wild.*","wildfire",StormData$EVTYPE) #where we'd cut off with 1000 threshold 
StormData$EVTYPE<-gsub("extremecold$|extremewindchill.*|extendedcold.*","extremecoldwindchill",StormData$EVTYPE)
StormData$EVTYPE<-gsub("^snow.*","heavysnow",StormData$EVTYPE)
StormData$EVTYPE<-gsub("landslide.*|landslump.*","debrisflow",StormData$EVTYPE)
StormData$EVTYPE<-gsub("^fog.*","densefog",StormData$EVTYPE)
StormData$EVTYPE<-gsub("^wind.*","strongwind",StormData$EVTYPE)
StormData$EVTYPE<-gsub("stormsurge.*","stormsurgetide",StormData$EVTYPE)
StormData$EVTYPE<-gsub("freezingrain.*","sleet",StormData$EVTYPE)
StormData$EVTYPE<-gsub("heavysurf.*|.*hightide.*","highsurf",StormData$EVTYPE)
StormData$EVTYPE<-gsub("^drymicroburst.*","highwind",StormData$EVTYPE)
StormData$EVTYPE<-gsub("lightsnow.*|moderatesnow.*|wintrymix.*","winterstorm",StormData$EVTYPE)
StormData$EVTYPE<-gsub("^hurricane.*|^typhoon.*","hurricanetyphoon",StormData$EVTYPE)
StormData$EVTYPE<-gsub("recordw.*|recordh.*","excessiveheat",StormData$EVTYPE)
StormData$EVTYPE<-gsub("unseasonablyw.*|unseasonablyh.*|unusuallyw.*","heat",StormData$EVTYPE)

check2<-table(StormData$EVTYPE) #create table of all remaining EVTYPEs
check2<-check2[!(names(check2) %in% EVTYPE_official)] #narrow table to only unofficial EVTYPEs
check2<-check2[order(check2,decreasing=TRUE)] #sort table in order of decreasing EVTYPE frequency
print(head(check2,10)) #see if the top 10 uncleaned EVTYPES have under 100 records per EVTYPE
print(sum(check2)/nrow(StormData)*100) #see what percent of the EVTYPE data is still uncleaned

###################################
# Cleaning Property and Crop Damage
###################################
print(table(StormData$PROPDMGEXP)) #check the values and distribution of PROPDMGEXP and CROPDMGEXP
print(table(StormData$CROPDMGEXP))

EXP<-function(x){ #create a function to translate PROPDMGEXP and CROPDMGEXP into usable integers
    if(x %in% 0:9) return(as.numeric(paste0("1e",x)))
    else if(x=="h"|x=="H") return(1e2) 
    else if(x=="k"|x=="K") return(1e3)
    else if(x=="m"|x=="M") return(1e6)
    else if(x=="b"|x=="B") return(1e9)
    else return(1)
}
StormData<-mutate(StormData,PROPDMGEXP=as.numeric(sapply(PROPDMGEXP,FUN=EXP)))%>%
    mutate(CROPDMGEXP=as.numeric(sapply(CROPDMGEXP,FUN=EXP))) #run all EXP data through the function

print(table(StormData$PROPDMGEXP)) #re-check the values and distribution of PROPDMGEXP and CROPDMGEXP
print(table(StormData$CROPDMGEXP))

################
# Cleaning Dates
################
StormData$BGN_DATE<-as.Date(StormData$BGN_DATE,format="%m/%d/%Y") #convert dates to date format
StormData$END_DATE<-as.Date(StormData$END_DATE,format="%m/%d/%Y")

print(nrow(StormData[StormData$BGN_DATE<"1996-01-01",])/nrow(StormData)*100) #see what percent of dates are before 1996
print(summary(StormData$BGN_DATE)) #get a summary of the date range and spread

################
# Narrowing Data
################
StormData_Complete<-filter(StormData,EVTYPE %in% EVTYPE_official)%>% #narrow data to only official EVTYPES
    mutate(EVTYPE=as.factor(EVTYPE),PROPDMG=PROPDMG*PROPDMGEXP,CROPDMG=CROPDMG*CROPDMGEXP)%>% #condense damage costs
    select(STATE,EVTYPE,BGN_DATE,FATALITIES,INJURIES,PROPDMG,CROPDMG)%>% #narrow variables of interest
    group_by(EVTYPE) #set factors for later summarization

StormData_Modern<-filter(StormData_Complete,BGN_DATE>="1996-01-01")%>% #create separate data set
    group_by(EVTYPE)                                                   #for just post 1995 data

##################
# Summarizing Data
##################
StormData_Fatalities_Avg<-summarize(StormData_Complete,FATALITIES=mean(FATALITIES))%>%
    arrange(desc(FATALITIES)) #get average fatalities
StormData_Injuries_Avg<-summarize(StormData_Complete,INJURIES=mean(INJURIES))%>%
    arrange(desc(INJURIES)) #get average injuries
StormData_PropDMG_Avg<-summarize(StormData_Complete,PROPDMG=mean(PROPDMG))%>%
    arrange(desc(PROPDMG)) #get average property damage
StormData_CropDMG_Avg<-summarize(StormData_Complete,CROPDMG=mean(CROPDMG))%>%
    arrange(desc(CROPDMG)) #get average crop damage

# use modern data set for totals, so it's a fair comparison
StormData_Fatalities_Tot<-summarize(StormData_Modern,FATALITIES=sum(FATALITIES))%>%
    arrange(desc(FATALITIES)) #get total fatalities
StormData_Injuries_Tot<-summarize(StormData_Modern,INJURIES=sum(INJURIES))%>%
    arrange(desc(INJURIES)) #get total injuries
StormData_PropDMG_Tot<-summarize(StormData_Modern,PROPDMG=sum(PROPDMG))%>%
    arrange(desc(PROPDMG)) #get total property damage
StormData_CropDMG_Tot<-summarize(StormData_Modern,CROPDMG=sum(CROPDMG))%>%
    arrange(desc(CROPDMG)) #get total crop damage

print(head(StormData_Fatalities_Avg)) #take a look at the summary results
print(head(StormData_Fatalities_Tot)) 
print(head(StormData_Injuries_Avg)) #similar distributions of effects for fatalities and injuries
print(head(StormData_Injuries_Tot))
print(head(StormData_PropDMG_Avg))
print(head(StormData_PropDMG_Tot))
print(head(StormData_CropDMG_Avg)) #more prop dmg from droughts, more prop dmg from storm surges
print(head(StormData_CropDMG_Tot))

# Note: the difference in averages is much more extreme than the difference in totals
HumanImpact_Avg<-mutate(StormData_Complete,HealthImpacts=FATALITIES+INJURIES)%>% #condense summaries for emphasis
    summarize(HealthImpacts=mean(HealthImpacts))%>%rename(Disaster_Type=EVTYPE)%>% #make variable nicer for graph
    arrange(desc(HealthImpacts))
HumanImpact_Tot<-merge(StormData_Fatalities_Tot,StormData_Injuries_Tot,all=TRUE)%>%
    mutate(HealthImpacts=FATALITIES+INJURIES)%>%rename(Fatalitities=FATALITIES,Injuries=INJURIES)%>%
    arrange(desc(HealthImpacts))
EconomicCost_Avg<-mutate(StormData_Complete,TotalCost=PROPDMG+CROPDMG)%>%
    summarize(TotalCost=mean(TotalCost)/1000000000)%>%rename(Disaster_Type=EVTYPE)%>% #divide cost by a billion for scaling
    arrange(desc(TotalCost))
EconomicCost_Tot<-merge(StormData_PropDMG_Tot,StormData_CropDMG_Tot,all=TRUE)%>%
    mutate(TotalCost=(PROPDMG+CROPDMG)/1000000000,PROPDMG=PROPDMG/1000000000,CROPDMG=CROPDMG/1000000000)%>%
    rename(Property=PROPDMG,Crop=CROPDMG)%>%arrange(desc(TotalCost))

# What is a representative slice of each category?
print(sum(HumanImpact_Avg$HealthImpacts[1:10])/sum(HumanImpact_Avg$HealthImpacts)*100) #top 10 is not representative
print(sum(HumanImpact_Tot$HealthImpacts[1:10])/sum(HumanImpact_Tot$HealthImpacts)*100) #for human impacts
print(sum(EconomicCost_Avg$TotalCost[1:10])/sum(EconomicCost_Avg$TotalCost)*100) #top 10 is representative
print(sum(EconomicCost_Tot$TotalCost[1:10])/sum(EconomicCost_Tot$TotalCost)*100) #for economic losses

print(sum(HumanImpact_Avg$HealthImpacts[1:20])/sum(HumanImpact_Avg$HealthImpacts)*100) #top 20 is representative
print(sum(HumanImpact_Tot$HealthImpacts[1:20])/sum(HumanImpact_Tot$HealthImpacts)*100) #for human impacts

EconomicCost_Avg<-mutate(StormData_Complete,TotalCost=PROPDMG+CROPDMG)%>% #use log10 instead for avg costs, due to extreme range
    summarize(TotalCost=log10(mean(TotalCost)))%>%rename(Disaster_Type=EVTYPE)%>%
    arrange(desc(TotalCost))

HumanImpactTot_Molten<-melt(HumanImpact_Tot[1:20,])%>%arrange(variable,desc(value)) #make type of impact/cost a factor
HumanImpactTot_Molten<-HumanImpactTot_Molten[1:40,]%>%rename(Types_of_Impact=variable) #remove total damage
EconomicCostTot_Molten<-melt(EconomicCost_Tot[1:10,])%>%arrange(variable,desc(value)) 
EconomicCostTot_Molten<-EconomicCostTot_Molten[1:20,]%>%rename(Types_of_Damage=variable) 

##########
# Graphing
##########
g1<-ggplot(data=HumanImpact_Avg[1:20,],aes(x=Disaster_Type,y=HealthImpacts))+ #create avg aesthetics
    geom_bar(stat="identity",fill="forestgreen")+
    labs(x="Event Type",y="Health Impacts",title="Top 20 Natural Disasters by Avg Health Impact")+
    theme(axis.text.x=element_text(angle=90,hjust=1))

g2<-ggplot(data=EconomicCost_Avg[1:10,],aes(x=Disaster_Type,y=TotalCost))+
    geom_bar(stat="identity",fill="firebrick1")+
    labs(x="Event Type",y="log10(Cost ($))",title="Top 10 Natural Disasters by Avg Economic Cost")+
    theme(axis.text.x=element_text(angle=90,size=12,hjust=1,vjust=-.1),
          axis.title=element_text(size=15),title=element_text(size=15))

png("Graph1_2.png",width=1200,height=480,units="px")
grid.arrange(g1,g2,ncol=2) #print first set of graphs together
dev.off()

g3<-ggplot(data=HumanImpactTot_Molten,aes(x=EVTYPE,y=value,fill=Types_of_Impact))+ #create total impact aesthetic
    geom_bar(stat="identity")+
    labs(x="Event Type",y="Count of Impacts",title="Top 20 Natural Disasters in US by Total Human Impact since 1996")+
    theme(axis.text.x=element_text(angle=90,size=12,hjust=1,vjust=-.1),
          axis.title=element_text(size=15),title=element_text(size=15),legend.text=element_text(size=12))

g4<-ggplot(data=EconomicCostTot_Molten,aes(x=EVTYPE,y=value,fill=Types_of_Damage))+ #create total cost aesthetic
    geom_bar(stat="identity")+
    labs(x="Event Type",y="Cost (billion $)",title="Top 10 Natural Disasters in US by Total Economic Cost since 1996")+
    theme(axis.text.x=element_text(angle=90,size=12,hjust=1,vjust=-.1),
          axis.title=element_text(size=15),title=element_text(size=15),legend.text=element_text(size=12))

png("Graph3.png",width=720,height=480,units="px")
print(g3) #print second set of graphs separately
dev.off()
png("Graph4.png",width=720,height=480,units="px")
print(g4)
dev.off()