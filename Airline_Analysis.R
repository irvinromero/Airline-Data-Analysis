# Author: Irvin Romero
# Airport Data Analysis 
# Initiate Script

setwd("~/Documents/TestData/")
library(ggplot2)
rm(list = ls())

# Read Data
# Loop through the files name, read.csv and combine them
getAirlineFiles <- list.files("../airlinedata", full.names = TRUE)
AirlineDataList <- lapply(getAirlineFiles, read.csv, stringsAsFactors = FALSE)
airlineData <- do.call(rbind, AirlineDataList)
airlineData <- airlineData[,!grepl("div", names(airlineData), ignore.case = TRUE)]
head(airlineData)


# Create Variables
# Day of week
dowNames <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
airlineData$down <- dowNames[airlineData$DAY_OF_WEEK]

# numerical to boolean if delay is greater than 15 mins
airlineData$DEP_DEL15  <- as.logical(airlineData$DEP_DEL15)
airlineData$ARR_DEL15  <- as.logical(airlineData$ARR_DEL15)

# Date Variables
airlineData$dateval <- as.Date(airlineData$FL_DATE, "%Y-%m-%d")
airlineData$week <- as.numeric(format(airlineData$dateval, "%U"))

# General Airline Data Statistics 
head(airlineData)
summary(airlineData)




# Seeing the top 10 by departure airline - of course ATL
# distribution decreases pretty fast, which might need to be taken to consideration when comparing data
topOriginAirports <- sort(table(airlineData$ORIGIN), decreasing = TRUE)[1:100]
barplot(topOriginAirports[1:10])


# Average Dest time delay by Origin airport (top 10) - but first I filter for the 100 top airports 
# (top is determined by number of reported trips desc in the 2012 - 2013 year)
getMeanDEP <- aggregate(DEP_DELAY ~ ORIGIN, 
                        data = airlineData[airlineData$ORIGIN %in% names(topOriginAirports), ], 
                        FUN = mean)
orderDEPMean <- order(getMeanDEP$DEP_DELAY, decreasing = TRUE)
head(getMeanDEP[orderDEPMean, ], 10)


# EWR - Newark Liberty International Airport has the worst average depareture delay delay at 17.039 mins followed by ORD





# Top 10 Worst Airports percentages (based on ORIGIN delay) from the busiest 100 airports
# Taking a look at the probability/ratio in which an airport will be delayed 
# (DEL15 = TRUE when it's at least 15 mins departure delay)
originDepRatio <- aggregate(DEP_DEL15 ~ ORIGIN, 
                            data = airlineData[airlineData$ORIGIN %in% names(topOriginAirports),], 
                            FUN= mean)

rankOriDelay<- order(originDepRatio$DEP_DEL15, decreasing = TRUE)
originDepRatio <- originDepRatio[rankOriDelay,][1:10,]
originDepRatio$DEP_DEL15 <- round(originDepRatio$DEP_DEL15*100, 2)
originDepRatio



# EWR - Newark not only ranked highest average delay, but also second mostly likley airport to be delayed in the first place
# From the 100 busiest airports we see that MDW has a average of 28.10% chance of having a departure delay. 
# MDW has the highest percentage of departure delays from the 100 busiest airport origins.



# Daily Origin Delay percent (Monday, Tuesday.. etc) for 10 Worst Airport Delays

# 10 highest origin airport delays
originAirports <- originDepRatio$ORIGIN
dailyDelay <- aggregate(DEP_DEL15 ~ down, 
                        data = airlineData[airlineData$ORIGIN %in% names(topOriginAirports),],
                        FUN= mean)

rankDailyDelay<- order(dailyDelay$DEP_DEL15, decreasing = TRUE)
dailyDelay <- dailyDelay[rankDailyDelay,][1:7,]
dailyDelay$DEP_DEL15 <- round(dailyDelay$DEP_DEL15*100, 2)
dailyDelay


# We see the highest daily departure delay percentage is on Wednesday and Thursday at 27.85%  and 27.03%. 
#On Wednesday and Thursday we are more likely to experience a departure delay compared to Friday which experiences a dep delay of 16.49%.


# Monthly origin delay percentage (Jan, Feb.. etc)

monthlyOriginDelayRatio <- aggregate(DEP_DEL15 ~ MONTH + down, data = airlineData, FUN = mean)
monthlyRanks<- order(monthlyOriginDelayRatio$DEP_DEL15, decreasing = TRUE)
head(monthlyOriginDelayRatio[monthlyRanks,], 10)

graphMonthlyDelay <- ggplot(monthlyOriginDelayRatio, aes(x=MONTH,y=DEP_DEL15, color = down))
graphMonthlyDelay + geom_line(stat= "identity") + labs(title = 'Monthly Day of Week Scatterplot', x = "Month", y = "Departure Delay > 15")


# The month of June experiences the highest departure delays with the worst days landing on Sunday. 
# We can see that the month of June has a high average of daily departure delays. 

# Day of week delay Ratio
dailyDelay2 <- aggregate(ORIGIN ~ DEP_DEL15+down, 
                         data = airlineData[airlineData$ORIGIN %in% originAirports,],
                         FUN = length)
dailyDelay2
g <- ggplot(dailyDelay2, aes(x = down, y = ORIGIN, fill = DEP_DEL15))
g + geom_bar(stat = "identity")+ labs(title = "Day of Week Departure Delay Ratio", x = "Day of Week", y = "Departure Delay > 15")


# Wednesdays and Thursdays we have a higher count of departure delays. 
# Colored bar plot showing worst average departure delays are Wednesdays and Thursdays


aggr2 <- aggregate(ARR_DEL15 ~ week + ORIGIN, data = airlineData, FUN = mean)
g4 <- ggplot(aggr2, aes(x = factor(week), y = ARR_DEL15, fill = factor(week)))
g4+geom_boxplot(outlier.shape = NA) + guides(fill=FALSE) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title = "Weekly Trend for Airline Departure Delay", x = "Week", y = "Airline Delay > 15 mins")


# From this box plot we can see that the weeks of June and December have the the highest average of departure delays, 
# specifically weeks 26 and 53 have a high number of variance




# Make a line plot for the top 10 airlines (you decide what determines top 10 airline).
# Then plot their % of delay by each month

monthlyDelay2 <- aggregate(ARR_DEL15  ~ dateval, data = airlineData, FUN = mean)
g3 <- ggplot(monthlyDelay2, aes(x = dateval, y = ARR_DEL15))
g3 + geom_line()   + labs(title = "Airline Delay Trend", x = "Date", y = "Departure Delay > 15")



# From this plot we can see our highest average departure delay occurs during the month of December.


# Cool little plot that I played around with to see distribution of delay for each Day of Week
newGG <- aggregate(DEP_DEL15 ~ ORIGIN + down+ MONTH, 
                   data = airlineData[airlineData$ORIGIN %in% originAirports,],
                   FUN = mean)

newGG$DEP_DEL15<- newGG$DEP_DEL15*100
rankNewS<- order(newGG$DEP_DEL15, decreasing = TRUE)
rankedFlights<- newGG[rankNewS, ][1:10,]
head(newGG)

graphNew<- ggplot(newGG, aes(DEP_DEL15))
graphNew + geom_density(aes(fill= factor(down)), alpha= .8)


# From this plot we can see that most of our data lies around 20% 


#Box plot 
#Origin by Avg Dep Delay
graphNew2 <- ggplot(newGG, aes(ORIGIN, DEP_DEL15))
graphNew2 + geom_boxplot(fill="plum") +
  labs(title= "Box Plot",
       subtitle= "Origin by Avg Dep Delay",
       x="ORIGIN",
       y="Average Dep Delay Percentage")


# From this box plot we are able to analyze the average departure delays are roughly a bit over 20%
# For the top 10 worst airline departures. 

### Freq plot - great plot look at the data density for 2-dims

Top10Dest<- sort(table(airlineData$DEST), decreasing = TRUE)[1:10]
Top10Origin<- sort(table(airlineData$ORIGIN), decreasing = TRUE)[1:10]

top10Data<- airlineData[airlineData$DEST %in% names(Top10Dest) & 
                          airlineData$ORIGIN %in% names(Top10Origin),]

head(top10Data)
top10Matrix<- data.frame(table(top10Data[, c("ORIGIN", "DEST")]))

tenMatrix<- ggplot(top10Matrix, aes(x= ORIGIN, y= DEST, fill = Freq ))
tenMatrix + geom_raster() + scale_fill_gradient(low="grey90", high="red")


# The highest count of flights occur from SFO and LAX. 



# Here I wanted to take a look at the data on airports I would travel from and see what I can find from this data
bayArea <- c('SFO', 'OAK', 'SJC')
bayAreaOrigin <- airlineData[airlineData$ORIGIN %in% bayArea &
                               airlineData$DEST == 'LAX', ]
head(bayAreaOrigin)

bayAreaMeans<- aggregate(DEP_DEL15 ~ ORIGIN + DEST + MONTH + QUARTER + YEAR,
                              data= bayAreaOrigin, 
                              FUN= mean)
rankBayMeans <- order(bayAreaMeans$DEP_DEL15, decreasing= TRUE)
bayAreaMeans<- bayAreaMeans[rankBayMeans,]
bayAreaMeans$DEP_DEL_Perc <- round(bayAreaMeans$DEP_DEL15*100, 2)
head(bayAreaMeans, 10)
bayAreaMeans$M2 <- as.Date(paste(bayAreaMeans$MONTH,"01", bayAreaMeans$YEAR, sep = "-"), "%m-%d-%Y")

BayAreaLinePlot <- ggplot(bayAreaMeans, aes(M2, DEP_DEL_Perc, color = ORIGIN))
BayAreaLinePlot + geom_line() + labs(title = "OAK, SFO, SJC Departure Delay % Overtime", x = "Date", y = "Departure Delay > 15 Percent")
# When comparing the departure locations from the Bay Area to LAX. 
# We see that during the months of July - Oct SFO's departure delay percentage is almost twice as much than SJC


bayAreaMean<- aggregate(DEP_DELAY ~ ORIGIN + DEST + + MONTH + QUARTER + YEAR, 
                        data= bayAreaOrigin[bayAreaOrigin$DEP_DEL15 == TRUE,],
                         FUN= mean)

rankBayMean <- order(bayAreaMean$DEP_DELAY, decreasing = TRUE)
bayAreaMean <- bayAreaMean[rankBayMean,]
head(bayAreaMean, 10)
lapply(split(bayAreaMean, bayAreaMean$ORIGIN), head, 3)


# ggplot box plot per qtr
bayAreaMean$QTR_YEAR = factor(paste(bayAreaMean$QUARTER, bayAreaMean$YEAR, sep = "_"),
                              levels = c("3_2012", "4_2012", "1_2013", "2_2013"))
ggplot(bayAreaMean, aes(x=QTR_YEAR, y= DEP_DELAY, fill= ORIGIN))  + geom_boxplot() +
  labs(title = "Quarterly Depareture Delay for SFO, SJC, OAK", y = "Departure Delay")

# Analyzing quarterly data we can see that SJC and OAK have dep delays at a mean of 40 wiith siginificantly higher delays from SFO



