library(ggplot2)
library(scales)
library(plyr)
library(vioplot)
library(zoo)
library(treemapify)
library(tibble)
library(dplyr)
library(ggpubr)

#load the data:

data2004 <- read.csv("C:\\Users\\Lenovo\\OneDrive\\Υπολογιστής\\BSc_BusinessAnalytics\\Analytical Practicum 2\\2004.csv")
View(data2004)

data2005 <- read.csv("C:\\Users\\Lenovo\\OneDrive\\Υπολογιστής\\BSc_BusinessAnalytics\\Analytical Practicum 2\\2005.csv")
View(data2005)

data <- rbind(data2004, data2005)
View(data)

remove(data2004)
remove(data2005)

str(data)

data$Date <- as.Date(with(data, paste(DayOfWeek, Month, Year,sep="-")), "%d-%m-%Y")
data$Date


data$Month_Year <- as.yearmon(paste(data$Month, data$Year), "%m %Y")
data$Month_Year

sapply(data, function(x) sum(is.na(x))) #count the NA

max(data$ArrDelay, na.rm = TRUE) #find out the max value for the column ArrDelay
min(data$ArrDelay, na.rm = TRUE) #find out the min value for the column ArrDelay

max(data$DepDelay, na.rm = TRUE)
min(data$DepDelay, na.rm = TRUE)

max(data$ActualElapsedTime, na.rm = TRUE)
min(data$ActualElapsedTime, na.rm = TRUE)



#Replace "" to NA in CancellationCode column
data$CancellationCode[data$CancellationCode==""] <- NA
unique(data$CancellationCode)

#Change the type of CancellationCode column
data$CancellationCode <- as.factor(data$CancellationCode)
unique(data$CancellationCode)

#Change the type of Cancelled column
data$Cancelled <- as.factor(data$Cancelled)

#Replace the values in ArrDelay column that are smaller than -60 or that are greater than 1700
data$ArrDelay <- ifelse(data$ArrDelay < -60 | data$ArrDelay > 1700, NA, data$ArrDelay)   

#Replace the values in DepDelay column that are smaller than -5 or greater than 1700
data$DepDelay <- ifelse(data$DepDelay < -5 | data$DepDelay > 1700, NA, data$DepDelay)

#change the type of some variables
data$Year <- as.factor(data$Year) #change the type of Year column

data$Month <- as.factor(data$Month) #change the type of Month column and arrange the levels
levels(data$Month) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

data$DayofMonth <- as.factor(data$DayofMonth) #change the type of DayofMonth column

data$DayOfWeek <- as.factor(data$DayOfWeek)  #change the type of DayofWeek column and arrange the levels
levels(data$DayOfWeek) <- c("Monday", "Tuesday", "Wednesday", 
                            "Thursday", "Friday", "Saturday", "Sunday")

#######################################################################################################
######################################     GRAPHS     #################################################
#######################################################################################################

#1st graph
#Total Flights for each pair of Months per Year

Y2004 <- subset(data, data$Year==2004)
Y2005 <- subset(data, data$Year==2005)

Freq_flights_per_month2004 <- plyr::count(Y2004$Month)
Freq_flights_per_month2004 <- as.data.frame(Freq_flights_per_month2004)

Freq_flights_per_month2005 <- plyr::count(Y2005$Month)
Freq_flights_per_month2005 <- as.data.frame(Freq_flights_per_month2005)


Freq_flights_per_month2004 <- Freq_flights_per_month2004 %>%
  add_column(Year = 2004)
head(Freq_flights_per_month2004)

Freq_flights_per_month2005 <- Freq_flights_per_month2005 %>%
  add_column(Year = 2005)
View(Freq_flights_per_month2005)


Freq_flights_per_month<-rbind(Freq_flights_per_month2004, Freq_flights_per_month2005)
View(Freq_flights_per_month)


Freq_flights_per_month <- Freq_flights_per_month %>%
  mutate(paired = rep(1:(n()/2),each=2),
         year=factor(Year))

View(Freq_flights_per_month)

Freq_flights_per_month %>% 
  ggplot(aes(x= freq, y= x)) +
  geom_line(aes(group = paired))+
  geom_point(aes(color=as.factor(Year))) +
  labs(color = "Year", x="Total Flights", y="Months", title = "Total Flights For Each Pair Of Months Per Year")

#2nd graph
# Total Flights for Each Month Per Year

Freq_flights_per_month <- Freq_flights_per_month[order(Freq_flights_per_month$x),]
rownames(Freq_flights_per_month) <- 1:nrow(Freq_flights_per_month)

Freq_flights_per_month <- Freq_flights_per_month %>%
  mutate(paired = rep(1:(n()/2),each=2),
         year=factor(Year))

Freq_flights_per_month %>% 
  ggplot(aes(x= freq, y= x)) +
  geom_line(aes(group = paired))+
  geom_point(aes(color=as.factor(Year))) +
  labs(color = "Year", x="Total Flights", y="Months", title = "Total Flights Each Month Per Year")

#3rd graph
#Total Flights from and to each Airport

Freq_flights_Origin2004 <- plyr::count(Y2004$Origin)
Freq_flights_Origin2004 <- as.data.frame(Freq_flights_Origin2004)

Freq_flights_Origin2005 <- plyr::count(Y2005$Origin)
Freq_flights_Origin2005 <- as.data.frame(Freq_flights_Origin2005)

Freq_flights_Dest2004 <- plyr::count(Y2004$Dest)
Freq_flights_Dest2004 <- as.data.frame(Freq_flights_Dest2004)

Freq_flights_Dest2005 <- plyr::count(Y2005$Dest)
Freq_flights_Dest2005 <- as.data.frame(Freq_flights_Dest2005)

Freq_flights_Origin2004 <- Freq_flights_Origin2004 %>%
  add_column(Year = 2004)
head(Freq_flights_Origin2004)

Freq_flights_Origin2005 <- Freq_flights_Origin2005 %>%
  add_column(Year = 2005)
head(Freq_flights_Origin2005)

Freq_flights_Dest2004 <- Freq_flights_Dest2004 %>%
  add_column(Year = 2004)
View(Freq_flights_Dest2004)

Freq_flights_Dest2005 <- Freq_flights_Dest2005 %>%
  add_column(Year = 2005)
View(Freq_flights_Dest2005)


Freq_flights_Origin<-rbind(Freq_flights_Origin2004, Freq_flights_Origin2005)
View(Freq_flights_Origin)

Freq_flights_Dest<-rbind(Freq_flights_Dest2004, Freq_flights_Dest2005)
View(Freq_flights_Dest)


Freq_flights_Origin <- Freq_flights_Origin[order(Freq_flights_Origin$x),]
rownames(Freq_flights_Origin) <- 1:nrow(Freq_flights_Origin)
View(Freq_flights_Origin)

Freq_flights_Dest <- Freq_flights_Dest[order(Freq_flights_Dest$x),]
rownames(Freq_flights_Dest) <- 1:nrow(Freq_flights_Dest)


Freq_flights_Origin_plot <- ggplot(Freq_flights_Origin, aes(label = x, area = freq, fill = freq)) +
  geom_treemap() + geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                                     grow = FALSE) + labs(fill = "Total Flights") + scale_fill_viridis_c() 

Freq_flights_Origin_plot

Freq_flights_Dest_plot <- ggplot(Freq_flights_Dest, aes(label = x, area = freq, fill = freq)) +
  geom_treemap() + geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                                     grow = FALSE) + labs(fill = "Total Flights") + scale_fill_viridis_c()

Freq_flights_Dest_plot

figure <- ggarrange(Freq_flights_Origin_plot, Freq_flights_Dest_plot,labels = c("Origin", "Destination"), ncol = 1, nrow = 2) + labs(color = "Total Flights")
figure

#4th graph
#Total Flights for each Airline per Year

Freq_per_Airline2004 <- plyr::count(Y2004$UniqueCarrier)
Freq_per_Airline2004 <- as.data.frame(Freq_per_Airline2004)

flights_2004 <- ggplot(Freq_per_Airline2004, aes(label = x, area = freq, fill = freq)) +
  geom_treemap() + geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                                     grow = FALSE) + labs(fill = "Total Flights") + scale_fill_viridis_c() 

flights_2004


flights_2005 <- Freq_per_Airline2005 <- plyr::count(Y2005$UniqueCarrier)
Freq_per_Airline2005 <- as.data.frame(Freq_per_Airline2005)

flights_2005 <- ggplot(Freq_per_Airline2005, aes(label = x, area = freq, fill = freq)) +
  geom_treemap() + geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                                     grow = FALSE) + labs(fill = "Total Flights") + scale_fill_viridis_c() 

flights_2005

figure <- ggarrange(flights_2004, flights_2005, labels = c("2004", "2005"),ncol = 1, nrow = 2) + labs(color = "Total Flights")
figure 

#5th graph
# Average Airline Delay for each Airline per Year

ggplot(aes(x = UniqueCarrier, y = CarrierDelay, fill=as.factor(Year)), data = data) +
  stat_summary(fun=mean, geom="bar", position = "dodge") + xlab("Airlines") + ylab("Average Airline Delay in minutes") + ggtitle("Average Airline Delay for each Airline per Year") + 
  labs(fill = "Year")

#Or with dots
#ggplot(aes(x = reorder(UniqueCarrier, CarrierDelay, FUN=mean), y = CarrierDelay, fill=UniqueCarrier), data = data) +
#  geom_boxplot() + ylim(0,7)


#6th graph
#Average Arrival Delay for each Airline

ggplot(data, aes(x = factor(UniqueCarrier), y = ArrDelay, color=UniqueCarrier, fill=UniqueCarrier)) + 
  geom_violin(trim = TRUE, draw_quantiles = c(0.25, 0.5, 0.75), color="black") + ylim(-40,60) + xlab("Airlines") + ylab("Average Airrival Delay in minutes") + ggtitle("Average Arrival Delay for each Airline") + 
  labs(fill = "Airline")


#7th graph
#Average Departure Delay per Airline

net_flights <- subset(data, data$Cancelled==0)
ggplot(subset(net_flights, DepDelay>=0), aes(x=reorder(UniqueCarrier, DepDelay, FUN=mean), y=DepDelay, fill=UniqueCarrier)) +
  geom_boxplot() +
  coord_cartesian(ylim=c(0,50)) +
  labs(x = 'Airline', y = 'Departure Delay in minutes',
       title = 'Average Departure Delay per Airline') + labs(fill = "Airline")

#8th graph
#Average Departure Delay per Month

ggplot(aes(x = Month, y = DepDelay, fill=as.factor(Year)), data = data) +
  stat_summary(fun=mean, geom="bar", position = "dodge") + xlab("Months") + ylab("Average Departure Delay in minutes") + ggtitle("Average Departure Delay for each Month Per Year") + 
  labs(fill = "Year")


#9th graph
#Cancellation Rate per Airline per Year

Y2004 <- subset(data, data$Year==2004)
Y2005 <- subset(data, data$Year==2005)

cancelled_flights2004 <- subset(Y2004, Y2004$Cancelled==1)
cancelled_flights2005 <- subset(Y2005, Y2005$Cancelled==1)

temp2004 <- subset(cancelled_flights2004, factor(cancelled_flights2004$CancellationCode)=='A')
temp2005 <- subset(cancelled_flights2005, factor(cancelled_flights2005$CancellationCode)=='A')

net_flights2004 <- subset(Y2004, Y2004$Cancelled==0)
net_flights2005 <- subset(Y2005, Y2005$Cancelled==0)

Cancellation_A_per_Airline2004 <- plyr::count(temp2004$UniqueCarrier)
Cancellation_A_per_Airline2004
Cancellation_A_per_Airline2005 <- plyr::count(temp2005$UniqueCarrier)
Cancellation_A_per_Airline2005

Non_Cancelled_flights_per_Airline2004 <- plyr::count(net_flights2004$UniqueCarrier)
Non_Cancelled_flights_per_Airline2004
Non_Cancelled_flights_per_Airline2005 <- plyr::count(net_flights2005$UniqueCarrier)
Non_Cancelled_flights_per_Airline2005


canc_noncanc_df2004 <- data.frame(Non_Cancelled_flights_per_Airline2004$x, Non_Cancelled_flights_per_Airline2004$freq, Cancellation_A_per_Airline2004$freq)
View(canc_noncanc_df2004)
canc_noncanc_df2005 <- data.frame(Non_Cancelled_flights_per_Airline2005$x, Non_Cancelled_flights_per_Airline2005$freq, Cancellation_A_per_Airline2005$freq)
View(canc_noncanc_df2005)

canc_noncanc_df2004$cancellation_rate2004 <- as.numeric(canc_noncanc_df2004$Cancellation_A_per_Airline2004.freq)/as.numeric(canc_noncanc_df2004$Non_Cancelled_flights_per_Airline2004.freq)
View(canc_noncanc_df2004)
canc_noncanc_df2005$cancellation_rate2005 <- as.numeric(canc_noncanc_df2005$Cancellation_A_per_Airline2005.freq)/as.numeric(canc_noncanc_df2005$Non_Cancelled_flights_per_Airline2005.freq)
View(canc_noncanc_df2005)

canc_noncanc_df2004 <- canc_noncanc_df2004 %>%
  add_column(Year = 2004)
head(canc_noncanc_df2004)

canc_noncanc_df2005 <- canc_noncanc_df2005 %>%
  add_column(Year = 2005)
View(canc_noncanc_df2005)

colnames(canc_noncanc_df2004) <- c('Airline_Name', 'Non_Cancelled_flights_per_Airline', 'Cancellation_A_per_Airline', 'cancellation_rate', 'Year')
colnames(canc_noncanc_df2005) <- c('Airline_Name', 'Non_Cancelled_flights_per_Airline', 'Cancellation_A_per_Airline', 'cancellation_rate', 'Year')

canc_noncanc_df<-rbind(canc_noncanc_df2004, canc_noncanc_df2005)
View(canc_noncanc_df)
str(canc_noncanc_df)

remove(Y2004)
remove(Y2005)
remove(temp2004)
remove(temp2005)

ggplot(canc_noncanc_df, aes(x = Airline_Name, y = cancellation_rate, colour = as.factor(Year))) + 
  geom_segment(aes(x = Airline_Name, y = 0.00, xend = Airline_Name, yend = cancellation_rate), color = "grey50", size = 0.15) +
  geom_point(size = 3.5) + xlab("Airlines") + ylab("% Cancelation Rate") + ggtitle("Cancellation Rate for each Airline Per Year") + 
  labs(color = "Year")

#10th graph
#Total Cancelled Flights per Cancellation Code Per Year

CancelledSubset<-subset(data, (!is.na(data$CancellationCode)))
ggplot(aes(x = Year, fill = as.factor(CancellationCode)), data = CancelledSubset) + geom_bar(position = "dodge" ) + geom_text(aes(label = ..count..),position = position_dodge(width = .9), stat = "count", colour = "black") +
  xlab("Year") + ylab("Total Cancelled Flights per Cancellation Code") + ggtitle("Total Cancelled Flights per Cancellation Code Per Year") + 
  labs(fill = "Cancellation Code")


#11th graph
#Total Flights per Airline Per Year

canc_noncanc_df %>% 
  ggplot(aes(x = Airline_Name, y = Non_Cancelled_flights_per_Airline, color = as.factor(Year))) + 
  geom_point() + 
  geom_line(aes(group = Year)) +
  xlab("Airline") + ylab("Total Flights per Airline") + ggtitle("Total Flights per Airline Per Year") + 
  labs(color = "Year")


#12th graph
#Total Cancelled Flights per Airline Per Year

canc_noncanc_df %>% 
  ggplot(aes(x = Airline_Name, y = Cancellation_A_per_Airline, color = as.factor(Year))) + 
  geom_point() + 
  geom_line(aes(group = Year)) +
  xlab("Airline") + ylab("Total Cancelled Flights per Airline") + ggtitle("Total Cancelled Flights per Airline Per Year") + 
  labs(color = "Year")
