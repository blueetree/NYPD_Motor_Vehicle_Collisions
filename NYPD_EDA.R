#Explore trends and causes
library(stringr)
library(ggplot2)
library(changepoint)
library(scales)
library(dplyr)
library(tidyr)

#library(chron)
nydata<-read.csv("NYPD_Motor_Vehicle_Collisions.csv", stringsAsFactors = FALSE, na.strings = '')

#"""check for data quality"""

##Names of char columns
#colnames((nydata[,sapply(nydata, is.numeric)==FALSE]))

##summary of numeric data
#summary((nydata[,sapply(nydata, is.numeric)]))

#############   CONTRIBUTING FACTORS (uncomment)    #####################
contfact1<- arrange(data.frame(summarise(group_by(nydata,CONTRIBUTING.FACTOR.VEHICLE.1), 
                                         inj = sum(NUMBER.OF.PERSONS.INJURED, na.rm = TRUE), 
                                         kill =  sum(NUMBER.OF.PERSONS.KILLED, na.rm = TRUE),
                                         incidents = n_distinct(UNIQUE.KEY))), 
                    desc(inj))

write.csv(contfact1, "contfact1.csv")

#contfact1<- arrange(data.frame(summarise(group_by(nydata,CONTRIBUTING.FACTOR.VEHICLE.1), 
#                                         inj = sum(NUMBER.OF.PERSONS.INJURED, na.rm = TRUE), kill =  sum(NUMBER.OF.PERSONS.KILLED, na.rm = TRUE))), desc(inj))
    
#contfact<- data.frame(summarise(group_by(nydata, CONTRIBUTING.FACTOR.VEHICLE.1, CONTRIBUTING.FACTOR.VEHICLE.2), 
#                                 inj = sum(NUMBER.OF.PERSONS.INJURED), kill =  sum(NUMBER.OF.PERSONS.KILLED)))

###########################################################################################
###############          summary by time of day(uncomment)           ######################
###########################################################################################

#extracting hour in another column
nydata$hour<- as.integer(str_split_fixed(nydata$TIME,":",2)[,1])

#extracting year in another column
nydata$year<- as.integer(str_split_fixed(nydata$DATE,"/",3)[,3])

#hourSummary <-# arrange(
#  data.frame(summarise(group_by(nydata,hour), 
#                      tot.inj = sum(NUMBER.OF.PERSONS.INJURED, na.rm = TRUE), 
#                      tot.kill =  sum(NUMBER.OF.PERSONS.KILLED, na.rm = TRUE),
#                      pedestrians.inj =  sum(NUMBER.OF.PEDESTRIANS.INJURED, na.rm = TRUE),
#                      pedestrians.kill =  sum(NUMBER.OF.PEDESTRIANS.KILLED, na.rm = TRUE),
#                      motorists.inj =  sum(NUMBER.OF.MOTORIST.INJURED, na.rm = TRUE),
#                      motorists.kill =  sum(NUMBER.OF.MOTORIST.KILLED, na.rm = TRUE)
#                      ))
                      
                      #as.numeric(TIME))

#sample hourly time series
#ggplot(hourSummary, aes(x = hour, y= tot.inj))+
#    geom_line()+
#    scale_x_time()+scale_y_continuous()

#removing rows with injured discrepancy
#nydata <- nydata[(nydata$NUMBER.OF.PERSONS.INJURED==
#                    nydata$NUMBER.OF.CYCLIST.INJURED+
#                    nydata$NUMBER.OF.MOTORIST.INJURED+
#                    nydata$NUMBER.OF.PEDESTRIANS.INJURED),]

###########################################################################################
############                work on final summarized set             ######################
###########################################################################################
nydata$tag <- ifelse(nydata$Weekday %in% 
                                       c("Monday","Tuesday","Wednesday","Thursday","Friday"), 
                                     "Weekday", "Weekend")

#key-value to convert injured columns->rows
nydata_injury <- filter(gather(nydata[,c(1,19,33,30,13,15,17,24)] , 
                    key = "Fatal.Category", 
                    value = "Injured", 
                    NUMBER.OF.PEDESTRIANS.INJURED,
                    NUMBER.OF.CYCLIST.INJURED, 
                    NUMBER.OF.MOTORIST.INJURED), is.na(Injured) == FALSE)

#key-value to convert killed columns->rows
nydata_killed <- filter(gather(nydata[,c(1,19,33,30,14,16,18,24)] , 
                   key = "Fatal.Category",
                   value = "Killed",
                   NUMBER.OF.PEDESTRIANS.KILLED,
                   NUMBER.OF.CYCLIST.KILLED,
                   NUMBER.OF.MOTORIST.KILLED), is.na(Killed) == FALSE)

#summarizing measures for required parameters
nysummary_injury <- nydata_injury %>%
                      group_by(CONTRIBUTING.FACTOR.VEHICLE.1,
                                hour, tag, Fatal.Category) %>%
                                summarise(TOTAL.injured = sum(Injured, na.rm = TRUE),
                                Day.Count = n_distinct(DATE, na.rm = TRUE),
                                Accident.count = n_distinct(UNIQUE.KEY, na.rm = TRUE))

nysummary_killed <- nydata_killed %>%
                    group_by(CONTRIBUTING.FACTOR.VEHICLE.1,
                             hour, tag, Fatal.Category) %>%
                             summarise(TOTAL.killed = sum(Killed, na.rm = TRUE),
                             Day.Count = n_distinct(DATE, na.rm = TRUE),
                             Accident.count = n_distinct(UNIQUE.KEY, na.rm = TRUE))


nysummary_injury_final <- nydata_injury %>%
  group_by(CONTRIBUTING.FACTOR.VEHICLE.1,
           hour) %>%
            summarise(TOTAL.injured = sum(Injured, na.rm = TRUE),
            Day.Count = n_distinct(DATE, na.rm = TRUE),
            Accident.count = n_distinct(UNIQUE.KEY, na.rm = TRUE))

nysummary_killed_final <- nydata_killed %>%
  group_by(CONTRIBUTING.FACTOR.VEHICLE.1,
           hour) %>%
            summarise(TOTAL.killed = sum(Killed, na.rm = TRUE),
            Day.Count = n_distinct(DATE, na.rm = TRUE),
            Accident.count = n_distinct(UNIQUE.KEY, na.rm = TRUE))

#replacing fatal categories for consistency before merge
nysummary_injury$Fatal.Category <- str_split_fixed(nysummary_injury$Fatal.Category, "\\.",4)[,3]
nysummary_killed$Fatal.Category <- str_split_fixed(nysummary_killed$Fatal.Category, "\\.",4)[,3]

#creating final dataset here
nysummary<- merge(x= nysummary_injury, 
                   y= nysummary_killed, 
                   by =  c("CONTRIBUTING.FACTOR.VEHICLE.1",
                           "hour", "tag", "Fatal.Category"),
                   all = TRUE)

nysummary_final<- merge(x= nysummary_injury_final, 
                  y= nysummary_killed_final, 
                  by =  c("CONTRIBUTING.FACTOR.VEHICLE.1",
                          "hour"),
                  all = TRUE)

nysummary_final <- arrange(nysummary_final, desc(TOTAL.injured))

write.csv(nysummary_final, "nysummary_final.csv")


#Adding measure (count of accidents) to summary from original dataset
#nyaccidents<-
#nysummary$accidents <- merge(x= nysummary, y = nydata, )


#bidding goodbye to our intermediate sets to free up memory
#rm(nydata, nysummary_injury, nysummary_killed, nydata_injury, nydata_killed)
rm(nysummary_injury, nysummary_killed, nydata_injury, nydata_killed)

#############################################################################
#############     Calculating change points in daily grain  #################
#############################################################################


write.csv(nysummary, "nysummary.csv")


df2<- summarise(group_by(nysummary, hour), Injured = sum(TOTAL.injured, na.rm = TRUE))

ts1<-as.ts(df2[,2])

library(changepoint)
plot(cpt.mean(ts1, method="BinSeg", Q=3), xlab="hour", ylab="Injured")

df1<-summarise(group_by(nydata, TIME), sum(nydata$NUMBER.OF.PERSONS.INJURED, na.rm = TRUE))

colnames(df1)[2]<-"Injured"

df1$TIME<-as.integer(paste0((str_split_fixed(df1$TIME,":",2))[,1],(str_split_fixed(df1$TIME,":",2))[,2]))
                     





################################
#Weekday weekend split with general count and sums
nydata$Weekday<- weekdays(as.Date(nydata$DATE, "%m/%d/%Y"))

nydata_weekday_summary<- data.frame(summarise(group_by(nydata, Weekday), Injuries = sum(NUMBER.OF.PERSONS.INJURED, na.rm = TRUE),
                                   Deaths = sum(NUMBER.OF.PERSONS.INJURED, na.rm = TRUE),
                                   Incidents = n_distinct(UNIQUE.KEY, na.rm = TRUE),
                                   Days = n_distinct(DATE, na.rm = TRUE)))

nydata_weekday_summary<-nydata_weekday_summary[(is.na(nydata_weekday_summary$Weekday)==FALSE),]

ggplot(nydata_weekday_summary, aes(x= Weekday, y= Incidents))+geom_bar(stat = "Identity")

nydata_weekday_summary$IncidentsFreq<-nydata_weekday_summary$Incidents/nydata_weekday_summary$Days

nydata_weekday_summary$tag <- ifelse(nydata_weekday_summary$Weekday %in% 
                                       c("Monday","Tuesday","Wednesday","Thursday","Friday"), 
                                     "Weekday", "Weekend")

nydata_weekday_plot <- data.frame(summarise(group_by(nydata_weekday_summary, tag), 
                                            InjuriesFreq = sum(Injuries)/sum(Days),
                                            DeathsFreq = sum(Deaths)/sum(Days),
                                            IncidentsFreq = sum(Incidents)/sum(Days)
                                            ))
  
ggplot(nydata_weekday_plot, aes(x= tag, y= IncidentsFreq ))+
  geom_bar(fill = "blue", color = "grey40", width = .5, alpha = .8, stat = "Identity")+
  geom_text(aes(label = as.integer(IncidentsFreq),nudge_x = -2))+
  theme_minimal()


###########################################################
#weekday weekend hourly Histogram

nysummary_incidents <- nysummary[(nysummary$Fatal.Category=="CYCLIST"),]

ggplot(nysummary_incidents, aes(x= hour, y= Accident.count.x))+
  geom_histogram(stat = "identity")+facet_grid(tag ~ .)








nysummary_final$cont <- nysummary_final$Accident.count.x/sum(nysummary_final$Accident.count.x)

nysummary_final$cont <- ifelse(nysummary_final$CONTRIBUTING.FACTOR.VEHICLE.1 %in%
                                 c("Driver Inattention/Distraction",
                                   "Failure to Yield Right-of-Way",
                                   "Following Too Closely",
                                   "Backing Unsafely",
                                   "Fatigued/Drowsy",
                                   "Other Vehicular",
                                   "Turning Improperly",
                                   "Passing or Lane Usage Improper",
                                   "Passing Too Closely",
                                   "Unsafe Lane Changing",
                                   "Traffic Control Disregarded",
                                   "Driver Inexperience",
                                   "Lost Consciousness",
                                   "Prescription Medication",
                                   "Pavement Slippery",
                                   "Alcohol Involvement",
                                   "Outside Car Distraction",
                                   "Reaction to Uninvolved Vehicle",
                                   "Unsafe Speed"), nysummary_final$CONTRIBUTING.FACTOR.VEHICLE.1, "Others")

# || (nysummary_final$cont!="Others") && (is.na(nysummary_final$cont)!=TRUE))
nysummary_final_sub <- nysummary_final[(nysummary_final$CONTRIBUTING.FACTOR.VEHICLE.1!="Unspecified"),]
nysummary_final_sub <- nysummary_final_sub[(is.na(nysummary_final$cont)!=TRUE),]




contmap<- read.csv("cont.csv", stringsAsFactors = FALSE)


nysummary_final_red <- merge(x=nysummary_final_sub, y= contmap, by=("CONTRIBUTING.FACTOR.VEHICLE.1"), all.x = TRUE)


write.csv(nysummary_area_Plot[,1:4], "nysummary_area_plot.csv")
