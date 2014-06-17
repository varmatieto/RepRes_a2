
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


rel_cols = c("EVTYPE", "BGN_DATE", "STATE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", 
                  "CROPDMG", "CROPDMGEXP")
SD_clean <- SD[, which(names(SD) %in% rel_cols)]

str(SD_clean)
dimclean<-dim(SD_clean)[1]

##########and take year only 


SD_clean$BGN_DATE <- as.Date(SD_clean$BGN_DATE, "%m/%d/%Y ")

SD_clean$BGN_DATE <- format (SD_clean$BGN_DATE, "%Y")

table(SD_clean$BGN_DATE )

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

str(SD_clean)
head(SD_clean)[c(-7,-9)]

SD_clean<-SD_clean[c(-7,-9)]

##########third operation : look at event type and partially clean it
# there are 985different EVENT TYPE 

length(unique(SD$EVTYPE))

main_EV<-head(sort(table(SD$EVTYPE), decreasing=TRUE), n=20)

barplot(main_EV[main_EV>10000])


headevents<-head(sort(table(SD_clean$EVTYPE), decreasing=TRUE), n=20)
kevents<-names(headevents)
barplot(headevents)

sumh<-sum(headevents)
(dimclean-sumh)/dimclean # 97% obs in first 20 labels



wind<- c(2,3,7,8,18)
flood<-c(5,6,17,19)
winter<-c(12,13)
marinestorm <-c(15,16)

kevents[wind]<-"strong wind"
kevents[flood]<-"flood"
kevents[winter]<-"winter weather"
kevents[marinestorm]<-"marinestorm"
mywind<-names(headevents)[wind]

length(SD_clean[SD_clean$EVTYPE %in% names(headevents)[marinestorm],]$EVTYPE) 

unique(kevents)

kevent_tranf <- list("wind"= c(2,3,7,8,18),
            "flood"=c(5,6,17,19),
            "winter"=c(12,13),
            "marinestorm" =c(15,16))

names(kevent_tranf)

# two key operations:
# set as other for the 3% which is not in the 20 key events
# simplify list of key events

SD_clean$EVTYPE[!(SD_clean$EVTYPE %in% kevents)]<-c("mixevents") 

for (i in 1:4){
    
    ivents<-kevents[kevent_tranf[[i]]]
    SD_clean$EVTYPE [SD_clean$EVTYPE %in% ivents ]<-names (kevent_tranf[i])
}

headevents<-head(sort(table(SD_clean$EVTYPE), decreasing=TRUE), n=20)

sum(headevents)
str(SD_clean)


write.table(SD_clean,"data/SD_clean.txt", sep=";")

######################################################################


SD_clean<- read.table ("data/SD_clean.txt" , header=T, sep=";",  
                 stringsAsFactors=F)

str(SD_clean)

library (plyr)

SDET<-ddply(SD_clean, .(EVTYPE,BGN_DATE,STATE), summarise,
            nobs= length(STATE),
            qFA= sum(FATALITIES),
            qIN= sum(INJURIES),
            qPR= sum(PROPDMG),
            qCR= sum(CROPDMG)    
            )
str(SDET)
head(SDET)

summary(SDET$qFA)
summary(SDET$qIN)
summary(SDET$qPR)
summary(SDET$qCR)

write.table(SDET,"data/SDET.txt", sep=";")

table(SDET$EVTYPE) # 1120/14572 cioe' 7%
table(SD_clean$EVTYPE) # 35838/902297 cioe' 4%

##############   PRINTING   #####

SDET<- read.table ("data/SDET.txt" , header=T, sep=";",  
                       stringsAsFactors=F)


str(SDET)
colnames(SDET)

SDET <- SDET[order(SDET$BGN_DATE,SDET$EVTYPE ),]
fix(SDET)




# "EVTYPE"   "BGN_DATE" "STATE"    "nobs"   "qFA"      "qIN"  "qPR"  "qCR"   

library (ggplot2)

ggplot( SDET, aes(x= EVTYPE, fill=EVTYPE) ) +
  geom_bar( show_guide = F) + coord_flip() +
  ggtitle("total obs. by event") 
#  xlab("Date") + ylab( "n. steps")

ggsave ("plot/SDET_obs.png")


SDET <- SDET[order(SDET$BGN_DATE),]

ggplot( SDET, aes(x= EVTYPE, y=nobs/1000, fill=EVTYPE) ) +
  geom_bar(stat="identity", show_guide = F ) + coord_flip() +
  ggtitle("total events by event") 

ggsave ("plot/SDET_events.png")


ggplot( SDET, aes(x=BGN_DATE, y=nobs/1000, fill=EVTYPE ) ) +
  geom_bar(stat="identity") +
  ggtitle("total events by year") 

ggsave ("plot/obs_year.png")

SDET <- SDET[order(SDET$STATE,SDET$EVTYPE ),]
ggplot( SDET, aes(x=STATE, y=nobs/1000, fill=EVTYPE ) ) +
  geom_bar(stat="identity") + coord_flip() +
  ggtitle("total events by state") 

ggsave ("plot/obs_state.png")


ggplot( SDET, aes(x=EVTYPE, y=qIN, fill=EVTYPE ) ) +
  geom_bar(stat="identity", show_guide = F ) + coord_flip() +
  ggtitle("total injured by event") 

ggsave ("plot/injured_ev.png")



ggplot( SDET, aes(x=EVTYPE, y=qFA, fill=EVTYPE ) ) +
  geom_bar(stat="identity", show_guide = F ) + coord_flip() +
  ggtitle("total casualties by event") 

ggsave ("plot/casualties.png")



ggplot( SDET, aes(x=EVTYPE, y=qPR, fill=EVTYPE ) ) +
  geom_bar(stat="identity", show_guide = F ) + coord_flip() +
  ggtitle("total damages by event") 

ggsave ("plot/damages.png")


ggplot( SDET, aes(x=EVTYPE, y=qCR, fill=EVTYPE ) ) +
  geom_bar(stat="identity", show_guide = F ) + coord_flip() +
  ggtitle("total crops by event") 

ggsave ("plot/crops.png")


################################
str(SDETx)

SDETx <- SDET[SDET$EVTYPE=="mixevents",]

ggplot( SDETx, aes(x=BGN_DATE, y=nobs/1000, fill="#FF0000" ) ) +
  geom_bar(stat="identity") +
  ggtitle("events mix by year") 

ggplot( SDETx, aes(x=BGN_DATE, y=qFA) ) +
  geom_bar(stat="identity", fill="#FF0000", show_guide = F  ) +
  ggtitle("casualties mix by year") 

ggplot( SDETx, aes(x=BGN_DATE, y=qIN) ) +
  geom_bar(stat="identity", fill="#FF00FF", show_guide = F  ) +
  ggtitle("injured mix by year") 

ggplot( SDETx, aes(x=BGN_DATE, y=qPR) ) +
  geom_bar(stat="identity", fill="#FF00FF", show_guide = F  ) +
  ggtitle("damages mix by year") 

ggplot( SDETx, aes(x=BGN_DATE, y=qCR) ) +
  geom_bar(stat="identity", fill="#FF00FF", show_guide = F  ) +
  ggtitle("crops mix by year") 


# problems in damages 2005 crops 1994 1995

table(SDETx$BGN_DATE)

SDETx05 <- SDETx[SDETx$BGN_DATE==2005,]
fix (SDETx05)
##################################################################################


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

ggsave (plot/"obs_events.png")


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


head(SD_clean[c(1:6,8)])


length(grep('WIND\\w+',SD_clean$EVTYPE))

grep('STR\\w+\\s+WI*ND',d2$EVTYPE)

unique(SD_clean$EVTYPE[grep('WIND\\w+',SD_clean$EVTYPE)])
unique(SD_clean$EVTYPE[grep('WIND*',SD_clean$EVTYPE)])

unique(SD_clean$EVTYPE[grep('^HIGH WINDS? \\d',SD_clean$EVTYPE)])
length(SD_clean$EVTYPE[grep('^HIGH WINDS?',SD_clean$EVTYPE)])

unique(SD_clean$EVTYPE[grep('^HIGH WINDS? \\d',SD_clean$EVTYPE)])

length(SD_clean$EVTYPE[grep('^HIGH WINDS? \\d',SD_clean$EVTYPE)])

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
