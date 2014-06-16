
#download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00275/
#Bike-Sharing-Dataset.zip", "ProjectData/Bike-Sharing-Dataset.zip")


url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

dir("data")

if (!file.exists("data/repdata_data_StormData.cvs.bz2")) {
    download.file(url, "data/repdata_data_StormData.cvs.bz2", method = "curl")  
}


    unzip("data/repdata_data_StormData.cvs.bz2", exdir = "data")
    

SD<- read.table ("repdata_data_StormData.csv" , header=T, sep=",",  
                 stringsAsFactors=F)
    file.remove("repdata_data_StormData.csv")



#################################################################################
# dataset with 902k obs.

str(SD)
names(SD)

##########first operation : choose only relevant columns


rel_cols = c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", 
                  "CROPDMG", "CROPDMGEXP")
SD_clean <- SD[, which(names(SD) %in% rel_cols)]

str(SD_clean)
dim(SD_clean)

##########second operation : recalculate damages using magnifier 


SD_clean$PROPDMGEXP <-as.factor(SD_clean$PROPDMGEXP)
SD_clean$CROPDMGEXP <-as.factor(SD_clean$CROPDMGEXP)

levels(SD_clean$PROPDMGEXP)
levels(SD_clean$CROPDMGEXP)

conversion_factor = list(`0` = c("", "-", "?", "+", "0"),
                    `1` = "1", 
                    `2` = c("2", "h", "H"),
                    `3` = c("3", "K", "k"), 
                    `4` = "4", 
                    `5` = "5", 
                    `6` = c("6", "M", "m"),
                    `7` = "7",
                    `8` = "8",
                    `9` = c("9","B", "b"))

levels(SD_clean$PROPDMGEXP) <- conversion_factor
levels(SD_clean$CROPDMGEXP) <- conversion_factor

table(SD_clean$PROPDMGEXP)
table(SD_clean$CROPDMGEXP)

SD_clean$PROPDMG = SD_clean$PROPDMG * (10^(as.integer(as.character(SD_clean$PROPDMGEXP))))
SD_clean$CROPDMG = SD_clean$CROPDMG * (10^(as.integer(as.character(SD_clean$CROPDMGEXP))))

summary (SD$PROPDMG )
summary (SD_clean$PROPDMG )
summary (SD$CROPDMG )
summary (SD_clean$CROPDMG )

head(SD_clean)


##########third operation : look at event type and partially clean it


head(sort(table(SD_clean$EVTYPE), decreasing=TRUE), n=20)


events<-c("WINDS?", "FLOODS?", "WATERS?", "RAINS?", "WINT", "SNOW", "CLOUDS?",
          "TEMP" , "HAILS?", "LIGHTNINGS?", "FIRES?", "FLD", "TORNADOS?")

nevents<-sapply (events, function(x) length(SD_clean$EVTYPE[grep(x,SD_clean$EVTYPE)]) )

nevents
nomi <- names(nevents)
sum(nevents)

neventsdf <- as.data.frame(nevents)
neventsdf <- cbind(neventsdf, nomi)


names(nevents)
head(neventsdf)

library (ggplot2)

ggplot( neventsdf, aes(x= nomi, y=nevents) ) +
geom_bar(stat="identity") + coord_flip() 
ggsave ("obs_events.png")


levents <- lapply (events, function(x) unique(SD_clean$EVTYPE[grep(x,SD_clean$EVTYPE)]) )
names(levents) <-events

levents

names(nevents)

sumevents <- sapply (events, function(x) sum(length( levents[[x]])))

sum(sumevents)

sum(levents [["WINDS?"]]%in%levents [["FLOODS?"]])
sum(levents [["FLOODS"]]%in%levents [["WATERS?"]])


barplot(nevents)

length(SD_clean$EVTYPE[grep('WINDS?',SD_clean$EVTYPE)])
# 364k obs. = 1/3 of all obs. has word "WIND"

unique(SD_clean$EVTYPE[grep('WINDS?',SD_clean$EVTYPE)])
unique(grep('WINDS?',SD_clean$EVTYPE, value=T))

###########################
length(SD_clean$EVTYPE[grep('FLOODS?',SD_clean$EVTYPE)])
# 82k obs. = 1/3 of all obs. has word "FLOOD"

unique(SD_clean$EVTYPE[grep('FLOODS?',SD_clean$EVTYPE)])

###################################################

length(SD_clean$EVTYPE[grep('WATERS?',SD_clean$EVTYPE)])
# 82k obs. = 1/3 of all obs. has word "FLOOD"

unique(SD_clean$EVTYPE[grep('WATERS?',SD_clean$EVTYPE)])
##########################

length(SD_clean$EVTYPE[grep('RAINS?',SD_clean$EVTYPE)])
# 82k obs. = 1/3 of all obs. has word "FLOOD"

unique(SD_clean$EVTYPE[grep('RAINS?',SD_clean$EVTYPE)])
##########################



length(SD_clean$EVTYPE[grep('FIRES?',SD_clean$EVTYPE)])
# 82k obs. = 1/3 of all obs. has word "FLOOD"

unique(SD_clean$EVTYPE[grep('FIRES?',SD_clean$EVTYPE)])



SD_clean[SD_clean$EVTYPE == "TSTM WIND",]$EVTYPE <- "THUNDERSTORM WIND"
SD_clean[SD_clean$EVTYPE == "THUNDERSTORM WINDS",]$EVTYPE <- "THUNDERSTORM WIND"
SD_clean[SD_clean$EVTYPE == "RIP CURRENT",]$EVTYPE <- "RIP CURRENTS"
SD_clean[SD_clean$EVTYPE == "STRONG WIND",]$EVTYPE <- "HIGH WIND"
SD_clean[SD_clean$EVTYPE == "HIGH WINDS",]$EVTYPE <- "HIGH WIND"
SD_clean[SD_clean$EVTYPE == "MARINE TSTM WIND",]$EVTYPE <- "MARINE THUNDERSTORM WIND"


head(sort(table(SD_clean$EVTYPE), decreasing=TRUE), n=20)


head(SD_clean[c(1:4,6)])

write.table(SD_clean[c(1:4,6)],"data/SD_clean.txt", sep=";")

length(grep('WIND\\w+',SD_clean$EVTYPE))

grep('STR\\w+\\s+WI*ND',d2$EVTYPE)

unique(SD_clean$EVTYPE[grep('WIND\\w+',SD_clean$EVTYPE)])
unique(SD_clean$EVTYPE[grep('WIND*',SD_clean$EVTYPE)])

unique(SD_clean$EVTYPE[grep('^HIGH WINDS? \\d',SD_clean$EVTYPE)])
length(SD_clean$EVTYPE[grep('^HIGH WINDS?',SD_clean$EVTYPE)])

unique(SD_clean$EVTYPE[grep('^HIGH WINDS? \\d',SD_clean$EVTYPE)])

length(SD_clean$EVTYPE[grep('^HIGH WINDS? \\d',SD_clean$EVTYPE)])

######################################################################


SD_clean<- read.table ("data/SD_clean.txt" , header=T, sep=";",  
                 stringsAsFactors=F)

str(SD_clean)

# there are 980 different EVENT TYPE 
length(unique(SD_clean$EVTYPE))

main_EV<-head(sort(table(SD_clean$EVTYPE), decreasing=TRUE), n=20)

barplot(main_EV[main_EV>10000])













# there are 985 different EVENT TYPE 

str(fEV)
max(levels(fEV))

levels(fEV)[1:10]
fEV[1:10]
levels(fEV)

levels(fEV)[table(fEV)>5000]

table(SD$FATALITIES, SD$EVTYPE)

hist(log(SD$FATALITIES))

table(SD$PROPDMGEXP)
summary(SD$PROPDMG)
summary(SD$CROPDMG)



library (plyr)

SDET<-ddply(SD, .(EVTYPE), summarise,
            qFA= sum(FATALITIES),
            qIN= sum(INJURIES),
            qPR= sum(PROPDMG),
            qCR= sum(CROPDMG)    
            )
str(SDET)

summary(SDET$qFA)
summary(SDET$qIN)
summary(SDET$qPR)
summary(SDET$qCR)

###################
plot(SDET$qFA,SDET$qIN)

SDETT<-SDET[SDET$EVTYPE!= "TORNADO",]

plot(SDETT$qFA,SDETT$qIN)

SDETm<-SDET[SDET$qFA>400,]

SDETm<-SDETm[order(SDETm$qFA),]

SDETm

SDETm<-SDETm[order(SDETm$qIN),]

SDETm


plot(SDETm$qFA,SDETm$qIN)

SDETmT<-SDETm[SDETm$EVTYPE!= "TORNADO",]

plot(SDETmT$qFA,SDETmT$qIN)

############################

plot(SDET$qPR,SDET$qCR)

SDETp<-SDET[SDET$qPR>500000,]
SDETp<-SDETp[order(SDETp$qPR),]
SDETp

plot(SDETp$qPR,SDETp$qCR)




############################### DA VEDERE POI ####################################
sapply(conversion_factor,  function(x) "k"%in%x)

exp_a <- names(conversion_factor) [sapply(conversion_factor,function(x) ""%in%x)]

dimSD<- dim(SD_clean)[1] 

for (i in 1:dimSD ){
    exp<-SD_clean$PROPDMGEXP[i]
    exp_a <- names(conversion_factor) [sapply(conversion_factor,function(x) exp%in%x)]
    SD_clean$PROPDMGEXP[i]<-exp_a
    
    exc<-SD_clean$CROPDMGEXP[i]
    exc_a <- names(conversion_factor) [sapply(conversion_factor,function(x) exc%in%x)]
    SD_clean$CROPDMGEXP[i] <- exc_a
}

