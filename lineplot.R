NYPD <- read.csv("~/Downloads/NYPD_Motor_Vehicle_Collisions.csv", stringsAsFactors=FALSE)
# map
require(ggmap)
locs <-  NYPD[c(5,6)]
register_google(key = "AIzaSyCLFqGoa-g_cytqBGovpVtr-yuTPf031yM", account_type = "standard")
nyc_locs <- get_map(location = "New York City", maptype = 'roadmap')
ggmap(nyc_locs) + geom_point(data = locs, aes(x = LONGITUDE, y = LATITUDE))
counts <- as.data.frame(table(round(locs$LONGITUDE,2), round(locs$LATITUDE,2)))
counts$Long <- as.numeric(as.character(counts$Var1))
counts$Lat <- as.numeric(as.character(counts$Var2))
counts2 <- subset(counts, Freq > 0)
ggmap(nyc_locs) + geom_tile(data = counts2, aes(x = Long, y = Lat, alpha = Freq), fill = "red")

# study time and data only
df <- NYPD[c(1,2,13:18)]
# test a small amount of data
# split the date and time column
test_df <- df
test_df$DATE <- strptime(as.character(test_df$DATE), "%m/%d/%Y")
test_df$Year <- as.numeric(format(test_df$DATE, format = "%Y"))
test_df$Month <- as.numeric(format(test_df$DATE, format = "%m"))
test_df$Day <- as.numeric(format(test_df$DATE, format = "%d"))
test_df$Time <- strptime(as.character(test_df$TIME), "%H:%M") 
test_df$Hour <- as.numeric(format(test_df$Time, format = "%H"))
test_df$Minute <- as.numeric(format(test_df$Time, format = "%M"))
test_df$Time <- NULL

# solution1: convert to categorical data
# divide months according to the tempature, daylight and snowfall of NYC
# p1:1,2,3,12 p2:4,5,10,11 p3:6,7,8,9
p1 <- c(1,2,3,12)
p2 <- c(4,5,10,11)
p3 <- c(6,7,8,9)
test_df$Part[test_df$Month %in% p1] <- 1
test_df$Part[test_df$Month %in% p2] <- 2
test_df$Part[test_df$Month %in% p3] <- 3
# create different frames
splitlist <- split(test_df, test_df$Part)
# loop
require(plyr)
require(reshape2)
require(lattice)
col <- c("red","green","blue")
#vertical line
divide <- c(4,6,8,9,17,18,20)
#set legend
#plot.new()
#legend(x = "top",inset = 0,
#       legend =c("1,2,3,12","4,5,10,11","6,7,8,9"), 
#       col=col, lwd=1, cex=.5, horiz = TRUE)

# plot by number
# avoid y axis changes
lmi<-list(c(20,2250),c(0,31),c(40,1200),c(0,6),c(860,7100),c(0,25))
for(i in 1:3){
  # create different frames
  P <- splitlist[[i]]
  # sum by hour
  P_df <- P[c(3:8,12)]
  P_df <- ddply(P_df, "Hour", numcolwise(sum))
  #plot
  mm <- melt(subset(P_df,select=c(
    Hour,NUMBER.OF.PEDESTRIANS.INJURED,NUMBER.OF.PEDESTRIANS.KILLED,NUMBER.OF.CYCLIST.INJURED,NUMBER.OF.CYCLIST.KILLED,
    NUMBER.OF.MOTORIST.INJURED,NUMBER.OF.MOTORIST.KILLED)),id.var="Hour")
  plot <- xyplot(value~Hour|variable,data=mm,type="l",col=col[i],
                 scales=list(y=list(relation="free",limits=lmi), x=list(at=c(0:23))),
                 par.settings = list(superpose.line = list(lwd=20)),
                 layout=c(1,6),
                 panel = function( x,y,...) {
                   panel.abline( v=x[ which(x %in% divide) ], lty = "dotted", col = "black")
                   panel.xyplot( x,y,...)
                 },
                 key=list(space="top",columns=3,text=list(lab=c("1,2,3,12","4,5,10,11","6,7,8,9")),
                          lines=list(lwt=2,col=col))
                 )
  var_name <- paste("plot", i, sep="_")
  assign(var_name, plot, env=.GlobalEnv)
}
require(RColorBrewer)
require(latticeExtra)
plot_1+plot_2+plot_3
# plot by rate
lmi<-list(c(0.94,1),c(0,0.06),c(0.965,1),c(0,0.035),c(0.9875,1),c(0,0.0125))
for(i in 1:3){
  # create different frames
  P <- splitlist[[i]]
  # sum by hour
  P_df <- P[c(3:8,12)]
  P_df <- ddply(P_df, "Hour", numcolwise(sum))
  # create injured/accident + killed/accident rates
  P_df$NUMBER.OF.PEDESTRIANS <- P_df$NUMBER.OF.PEDESTRIANS.INJURED + P_df$NUMBER.OF.PEDESTRIANS.KILLED
  P_df$NUMBER.OF.CYCLIST <- P_df$NUMBER.OF.CYCLIST.INJURED + P_df$NUMBER.OF.CYCLIST.KILLED
  P_df$NUMBER.OF.MOTORIST <- P_df$NUMBER.OF.MOTORIST.INJURED + P_df$NUMBER.OF.MOTORIST.KILLED
  P_df$PEDESTRIANS.INJURED.RATE <- P_df$NUMBER.OF.PEDESTRIANS.INJURED / P_df$NUMBER.OF.PEDESTRIANS
  P_df$PEDESTRIANS.KILLED.RATE <- P_df$NUMBER.OF.PEDESTRIANS.KILLED / P_df$NUMBER.OF.PEDESTRIANS
  P_df$CYCLIST.INJURED.RATE <- P_df$NUMBER.OF.CYCLIST.INJURED / P_df$NUMBER.OF.CYCLIST
  P_df$CYCLIST.KILLED.RATE <- P_df$NUMBER.OF.CYCLIST.KILLED / P_df$NUMBER.OF.CYCLIST
  P_df$MOTORIST.INJURED.RATE <- P_df$NUMBER.OF.MOTORIST.INJURED / P_df$NUMBER.OF.MOTORIST
  P_df$MOTORIST.KILLED.RATE <- P_df$NUMBER.OF.MOTORIST.KILLED / P_df$NUMBER.OF.MOTORIST
  #plot
  mm <- melt(subset(P_df,select=c(
    Hour,PEDESTRIANS.INJURED.RATE,PEDESTRIANS.KILLED.RATE,CYCLIST.INJURED.RATE,CYCLIST.KILLED.RATE,
    MOTORIST.INJURED.RATE,MOTORIST.KILLED.RATE)),id.var="Hour")
  plot <- xyplot(value~Hour|variable,data=mm,type="l",col=col[i],
                 scales=list(y=list(relation="free",limits=lmi), x=list(at=c(0:23))),
                 par.settings = list(superpose.line = list(lwd=20)),
                 layout=c(1,6),
                 panel = function( x,y,...) {
                   panel.abline( v=x[ which(x %in% divide) ], lty = "dotted", col = "black")
                   panel.xyplot( x,y,...)},
                 key=list(space="top",columns=3,text=list(lab=c("1,2,3,12","4,5,10,11","6,7,8,9")),
                          lines=list(lwt=2,col=col))
                 )
  var_name <- paste("plot", i, sep="_")
  assign(var_name, plot, env=.GlobalEnv)
}
require(RColorBrewer)
require(latticeExtra)
plot_1+plot_2+plot_3


