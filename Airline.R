###################################################################################
# Module: Interpretation of Data II
# Assignment 1: Analyse Data collected for US Air records for the  first 2 weeks of 
# July 2015.
# Student Name: Joanne White
# Student ID:   A00268096
# Date:         10/02/2020
###################################################################################

# Import Libraries to manipulate the data
library(tidyr)
library(dplyr)
library(stringr)
library(gapminder)
library(ggplot2)
library(dslabs)
library(ggthemes)
library(ggrepel)

# Import the data files 3 csv files into 3 dataframes
df_airlines <-read.csv("US_airlines.csv",header =TRUE)
df_airports <-read.csv("US_airports.csv",header =TRUE)
df_airrecords <-read.csv("US_airrecords14.csv",header =TRUE)

###################################################################################
# Explore the data
###################################################################################

# Displays first 6 recordsof each file
print(head(df_airlines))
print(head(df_airports))
print(head(df_airrecords))

# Displays Statistics for each column
print(summary(df_airlines))
print(summary(df_airports))
print(summary(df_airrecords))
# Displays the names of the columns
print(colnames(df_airlines))
print(colnames(df_airports))
print(colnames(df_airrecords))
# Displays the structure
print(str(df_airlines))
print(str(df_airports))
print(str(df_airrecords))

######################################
# Remove irrevelant colums
###################################

# Having looked at the data in the 3 files I decided to removed 3 columns in airports
# This also removes some blanks Latitude and Longitude
df_airports <- df_airports %>%
  select(-c("COUNTRY","LATITUDE","LONGITUDE"))
# Check the columns in the df to make sure they are removed
print(colnames(df_airports))

df_airrecords <- df_airrecords %>%
  select(-c("TAXI_OUT","WHEELS_OFF","WHEELS_ON","TAXI_IN","ELAPSED_TIME"))
# Check the columns in the df to make sure they are removed
print(colnames(df_airrecords))


###################################################################################
# Change name of columns to make more sense
###################################################################################

# Change the name of the "day" column in airrecords to be day_date is actual date 
colnames(df_airrecords)[colnames(df_airrecords) == 'DAY'] <- 'DAY_DATE'
print(colnames(df_airrecords))

###################################################################################
# Transform variables - Airline, Aiport origin and destination
###################################################################################

# Transform Airline to full name
df_airrecords <- merge(df_airrecords, df_airlines, by.x = "AIRLINE", by.y = "IATA_CODE")
# delete the old AIRLINE column,
cols.dont.want <- "AIRLINE"
df_airrecords <- df_airrecords[, ! names(df_airrecords) %in% cols.dont.want, drop = F]
# and rename the new AIRLINE.y column
colnames(df_airrecords)[colnames(df_airrecords) == 'AIRLINE.y'] <- 'AIRLINE'

# Transform Aiport origin to full name
 df_airrecords <- merge(df_airrecords, df_airports, by.x = "ORIGIN_AIRPORT", by.y =  "IATA_CODE")
# Transform Aiport destination to full name
df_airrecords <- merge(df_airrecords, df_airports, by.x = "DESTINATION_AIRPORT", by.y =  "IATA_CODE")

###################################################################################
# Transform week days from numbers to days
###################################################################################
# Change decision to a factor
df_airrecords$DAY_OF_WEEK <-as.factor(df_airrecords$DAY_OF_WEEK)
print(str(df_airrecords))

#############################
# Rename the label of the factor
################################

df_airrecords$DAY_OF_WEEK <- factor(df_airrecords$DAY_OF_WEEK,
                            levels = c("1","2","3","4","5","6","7"),
                            labels = c("Mon", "Tues","Wed","Thurs","Fri","Sat","Sun"))

###################################################################################
# Clean up the data
###################################################################################
#####################################
# Check for NA's and deal with  
#####################################
# Displays if any missing values
print(table(is.na(df_airrecords)))

# Displays rows with nulls only
print (df_airrecords[!complete.cases(df_airrecords),])


#####################################
# Check for blanks NANS and deal with
#####################################
# Having looked at the data the NA's are valid in these cases
# There are number of Tail numbers missing but we shall leave these as is for the moment 


##########################################################
#
# Visualising  the data
#
##########################################################

df_delay <- df_airrecords %>%
  select(ARRIVAL_DELAY) %>%
  drop_na(ARRIVAL_DELAY)%>%
  mutate(ARRIVAL_DELAY = case_when(ARRIVAL_DELAY > 0 ~ "Late", ARRIVAL_DELAY == 0 ~ "On Time", ARRIVAL_DELAY < 0 ~ "Early")) %>%
  group_by(ARRIVAL_DELAY) %>%
  count()

# Plot to see the departure delays 
qplot(x = ARRIVAL_DELAY,weight = n, data = df_delay, geom = "bar",fill= ARRIVAL_DELAY, xlab = "Arrival Status", ylab = "Number of flights",main = "Breakdown of flight arrival times")


# Total flights delayed
print(paste(c("Flights delayed were ", df_delay[2,]$n, " out of ", nrow(df_airrecords)), collapse = ""))

# Total flights delayed 
print(paste(c("Flights that arrived early were ", df_delay[1,]$n, " out of ", nrow(df_airrecords)), collapse = ""))

# Total flights delayed 
print(paste(c("Flights that arrived on time were ", df_delay[3,]$n, " out of ", nrow(df_airrecords)), collapse = ""))

flights_depart_del<- df_airrecords %>%
  # creating a new variable to classify if a flight is on time or delayed
  select(c(AIRLINE,DEPARTURE_DELAY))%>%
  drop_na(DEPARTURE_DELAY)

# Plot to see the departure delays 
qplot(x = DEPARTURE_DELAY, data = flights_depart_del, geom = "histogram", xlab = "Departure delay in minutes", ylab = "Number of flights",main = "Number of departure delays", binwidth = 30)


########################################
# Flights delayed by departure time
########################################
flights <- df_airrecords %>%
  # creating a new variable to classify if a flight is on time or delayed
  mutate(dep_type = ifelse(DEPARTURE_DELAY < 2, "on time", "delayed"))
# plot a bar chart by day
print("plot 1 flights delayed by dep time")
qplot(x = DAY_OF_WEEK, fill = dep_type, data = flights, geom = "bar",xlab = "Day of the Week", ylab = "Departure Delay Numbers" ,main = "Delayed vs On Time departures by Day")

#########################################
# Flights arrival_delay vs on time
#########################################
flights <- df_airrecords %>%
  # creating a new variable to classify if a flight is on time or delayed
  mutate(flight.type = ifelse(ARRIVAL_DELAY < 1, "on time", "delayed"))
print("plot 2 flights delayed by arrival time")
# plot a bar chart by day
qplot(x = DAY_OF_WEEK, fill = flight.type, data = flights, geom = "bar", ylab="Number of flights",xlab ="Day of the Week", main = "Delayed arrivals vs On Time arrivals by Day")

#########################################
# Flights cancelled per day
#########################################
flights_cancelled <- df_airrecords %>%
  # creating a new variable to classify if a flight is on time or delayed
  mutate(cancelled = ifelse(CANCELLED == 1, "flight cancelled", "flight not cancelled"))
print("plot 3 flights cancelled per day")
# plot a bar chart by day
# Sort out the DAys in the x axis 
qplot(x = DAY_OF_WEEK, fill = cancelled, data = flights_cancelled, geom = "bar",xlab="Day of the Week ", main = "Flights cancelled per day")

####################################################################################
# take a 1% random sample of the flights data to make the plot readable days of week 
# arrival delay
####################################################################################
print("plot 4 sample of flight data for days of week ")
data <- df_airrecords %>% sample_frac(.01)%>%
  drop_na(ARRIVAL_DELAY)
# add a trend line to the plot
geom_smooth(span = 0.1)
ggplot(data, aes(x=DAY_OF_WEEK, y= ARRIVAL_DELAY)) + 
  geom_point() +
  geom_smooth(span = 1)+
  xlab("Day of Week")+
  ylab("Delay")+
  ggtitle("1% sample of flight data ")

#########################################################################################
# take a 1% random sample of the flights data to make the plot readable days of fortnight
# arrival delay
#########################################################################################
print("plot 4 sample of flight data for 2 weeks ")
data <- df_airrecords %>% sample_frac(.01)%>%
  drop_na(ARRIVAL_DELAY)
  #add a trend line to the plot
 geom_smooth(span = 0.1)
ggplot(data, aes(x=DAY_DATE, y= ARRIVAL_DELAY)) + 
  geom_point() +
  geom_smooth(span = 1)+ 
  ylab("Delay in Arrival time")+
  xlab("Date in July")+
  ggtitle("Delay in arrival time by day")
 
#############################################################
# Diverted flights per day as portion of all flights that day
#############################################################    
flights_diverted <- df_airrecords %>%
  # creating a new variable to classify if a flight is on time or delayed
  mutate(diverted_txt = ifelse(DIVERTED == 1, "flight diverted", "flight not diverted"))

#plot 5###################
# plot a bar chart by day
##########################
  # Sort out the DAys in the x axis 
  qplot(x = DAY_OF_WEEK, fill = diverted_txt, data = flights_diverted, geom = "bar", ylab = "",xlab= "Day of the Week", main = "Flights diverted per day")

# plot ###################
# Just flights diverted 
##########################
  # Sort out the DAys in the x axis 
p<- ggplot(flights_diverted,aes(DAY_OF_WEEK,DIVERTED,fill=DAY_OF_WEEK))+
  geom_bar(stat="identity",color="black", width = 0.5)+
  theme(legend.position="none")+
  xlab("Day of the week")+
  ylab("Number of flights Diverted")+
  ggtitle("Flights diverted per day")
print(p)

# Print state and flight count
flights_state <- df_airrecords %>%
  # creating a new variable to classify if a flight is on time or delayed
  select(c(DAY_DATE,STATE.x,ARRIVAL_DELAY))%>%
  drop_na(ARRIVAL_DELAY)%>%
  mutate(ARRIVAL_DELAY = ifelse(ARRIVAL_DELAY >=0, 1,0))%>%
  group_by(STATE.x)%>%
  summarize(perc_delayed = round(mean(ARRIVAL_DELAY)*100,digits = 2))%>%
  arrange(desc(perc_delayed))

p<- ggplot(flights_state,aes(STATE.x ,perc_delayed, fill = STATE.x))+
  geom_bar(stat="identity",color="black", width = 0.5)+
  theme(legend.position="none")+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("Date")+
  ylab("Number of flights ")+
  ggtitle("Flights from each state")
print(p)

# Plot to see the departure delays 
qplot(x = STATE.x, data = df_airrecords,geom = "bar",fill= "no.of flights",xlab = "State",
        ylab="No.Of flights",main = "No. Of flights to/from a state")

#####################################
# Airlines with the most delays
#####################################

# Got the airline and arrival delay, removed NA's as can't estimate this,
# created column that was delayed (1) or (0) = not delay or on time.
# Then calculating what percentage that airline has late


df_mostdelays <- df_airrecords%>%
  select(c(AIRLINE,ARRIVAL_DELAY))%>%
  drop_na(ARRIVAL_DELAY)%>%
  mutate(ARRIVAL_DELAY = ifelse(ARRIVAL_DELAY >=0, 1,0))%>%
  group_by(AIRLINE)%>%
  summarize(percent_delayed = round(mean(ARRIVAL_DELAY)*100,digits = 2))%>%
  arrange(desc(percent_delayed))

# relabel the axis

p<- ggplot(df_mostdelays,mapping = aes(reorder(AIRLINE, -percent_delayed), percent_delayed))+ 
  geom_bar(stat = "identity")+
  xlab("Airline")+
  ylab("Percentage Delay")+
  ggtitle("Average Delay of Airline")+
  theme(axis.text.x = element_text(angle = 90))

print(p)

#########################################
# Average delay per flight by airline
#########################################
df_mostdelayed <- df_airrecords%>%
  select(c(AIRLINE,ARRIVAL_DELAY))%>%
  drop_na(ARRIVAL_DELAY)%>%
  group_by(AIRLINE)%>%
  summarize(average_delay = round(mean(ARRIVAL_DELAY),digits = 2))%>%
  arrange(desc(average_delay))

# relabel the axis
p<- ggplot(df_mostdelayed,mapping = aes(x=reorder(AIRLINE, -average_delay),average_delay, fill=AIRLINE, color=AIRLINE))+ 
  geom_bar(stat = "identity")+
  scale_fill_hue("AIRLINE") +
  xlab("Airline")+
  ylab("Average delay")+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Average delay per flight by Airline")

print(p)


#####################################
# Busiest Routes
#####################################

# Create new column called route made out of origin airport and destination and then see which 
# has the most flights

df_routes <- df_airrecords%>%
  select(c(AIRLINE,ORIGIN_AIRPORT, DESTINATION_AIRPORT))%>%
  #drop_na(ARRIVAL_DEL)%>%
  unite(route, c(ORIGIN_AIRPORT, DESTINATION_AIRPORT), sep = "-", remove = TRUE) %>%
  group_by(route) %>%
  count(route,sort=TRUE,name="Total.flights.per.route")%>%
  head(10)
 
# plot the results
q<- ggplot(df_routes,mapping = aes(x=reorder(route, -Total.flights.per.route),Total.flights.per.route, fill=route, color=route))+ 
  geom_bar(stat = "identity", width = 0.5)+
  scale_fill_hue("route") +
  theme(legend.position="none")+
  xlab("Route")+
  ylab("Total Filghts per route")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
  ggtitle("Busiest Routes")

print(q)

#####################################
# Relationship between distance between airports and flying time
#####################################

# Set up the data, use distance for distance between the 2 airports and air_time 
# for the actual flight time
df_distance_airtime <- df_airrecords%>%
  select(c(DISTANCE,AIR_TIME)) %>%
  drop_na(DISTANCE,AIR_TIME)
x <- as.vector(df_distance_airtime$DISTANCE)
y <- as.vector(df_distance_airtime$AIR_TIME)

# See if there is a relationship print this our to rmarkdown
cor(x,y)

# cor = 0.99 14567 there is strong positive correlation between distance between airports and flying time.
# Plot to visualise
plot(x,y,
main="Relationship bewteen airport distance and flight time",
ylab="Flying Time",
xlab = "Distance")

# Put data into linear model
model1 <- lm(y~x)

# shows summary data, significance test for linear regression
summary(model1)

 #plot the residual models and test 4 assumptions
layout(matrix(1:4,2,2))
plot(model1)

#######################################
# Predict flying times
#######################################

# Predict the flying time of distance 1000
coeffs = coefficients(model1);coeffs
# distance, x = 1000, y = flying time
x=1000
duration = coeffs[1] + coeffs[2]*x
# flying time for distance of 1000
duration


#Confidence interval for linear regression
# Create new data frame to hold duration
newdata = data.frame(duration=1000)

# Predict the time and the 95% confidence intervals for distance of x
# in this case it is 1000

predict(model1,newdata,interval = "confidence")

# The confidence interval 
#   fit      lwr      upr
# 1 133.075 133.0348 133.1153


# Predict the time with prediction intervals
predict(model1,newdata,interval = "predict")

#       fit      lwr      upr
# 1 133.075 114.4093 151.7407



