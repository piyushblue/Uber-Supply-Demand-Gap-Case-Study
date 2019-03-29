#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("sqldf")
#install.packages("stringr")
#install.packages("stats")
#install.packages("scales")
#install.packages("titanic")
#install.packages("swirl")
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("lubridate")
library(lubridate)
library(ggplot2)
library(swirl)
library(titanic)
library(tidyr)
library(dplyr)
library(sqldf)
library(stringr)
library(stats)
library(scales)
library(gridExtra)
uber_df<-read.csv("Uber Request Data.csv",header = TRUE,stringsAsFactors = TRUE) # UPLOADING DATA with string as factor True
str(uber_df)#finding the unique values for each column.pickup point is unique with 2 values , status is unique with 3 values
uber_df$Request.timestamp_new<-parse_date_time(uber_df$Request.timestamp,orders = c("dmy_HMS","dmy_HM"))#formating request
#timestamp into new columns with common format
uber_df$Drop.timestamp_new<-parse_date_time(uber_df$Drop.timestamp,orders = c("dmy_HMS","dmy_HM")) # formating drop
#timestamp into new columns with common format
uber_df$Request.date <- as.Date(uber_df$Request.timestamp_new)#Splitting into request date and time
uber_df$Request.time <- format(uber_df$Request.timestamp_new,"%H:%M:%S")
uber_df$Drop.date <- as.Date(uber_df$Drop.timestamp_new)#Splitting into drop date and time
uber_df$Drop.time <- format(uber_df$Drop.timestamp_new,"%H:%M:%S")
uber_df$Journey.time<-round(uber_df$Drop.timestamp_new-uber_df$Request.timestamp_new)
sum(duplicated(uber_df$Request.id))#checking for duplicate 0 results found
#function to allocate time slots
allocate_slots <- function(p)
{
   if(p >= "04:00:00" && p < "08:00:00")
  {
    timeslot <- "04-08"
    
    
  } else if(p >= "08:00:00" &&  p < "12:00:00")
  {
    timeslot <- "08-12"
    
  } 
  else if(p >= "12:00:00" && p < "15:00:00")
  {
    timeslot <- "12-15"
    
  } 
  else if(p >= "15:00:00" && p < "17:00:00")
  {
    timeslot <- "15-17"
    
  } 
  else if(p >= "17:00:00" && p < "20:00:00")
  {
    timeslot <- "17-20"
    
  } 
  else if(p >= "20:00:00" && p < "22:00:00")
  {
    timeslot <- "20-22"
    
  } 
  else if(p >= "22:00:00" && p < "24:00:00")
  {
    timeslot <- "22-24"
    
  } 
  else if(p >= "00:00:00" && p < "4:00:00")
  {
    timeslot <- "00-04"
    
  } 
  else
  {
    timeslot = "No timeslots"
  }
  return(timeslot)
}
 

uber_df$request.slot<-sapply(uber_df$Request.time, function(x) allocate_slots(x) )# using sapply to create new columns for time slot


#plot to display status of request ; majority of request more then 50% is cancelled or no cars available
ggplot(uber_df,aes(x=uber_df$Status,fill=uber_df$Pickup.point))+geom_bar(stat='count',position="dodge")+ggtitle("Cab Demand per Slot")+labs(x="Status", y="Number of Cabs Requested") +labs(fill="Pickup Point")


#plotting the stacked bar chart to show request status against the time slots
#volumns of cancelled and no cars available is high during morning and evening respectively
plot_status<-ggplot(uber_df,aes(x=uber_df$request.slot,fill=uber_df$Status))+geom_bar(position="stack") +ggtitle("Cab Demand per Slot")+labs(x="Time slots", y="Number of Cabs Requested")+labs(fill="Trip Status")
plot_status


#plotting the  bar chart to display pickup point counts against time slots
plot_pickup<-ggplot(uber_df,aes(x=uber_df$request.slot,fill=uber_df$Pickup.point))+geom_bar(stat='count',position="dodge")+ggtitle("Cab Demand per Slot")+labs(x="Time slots", y="Number of Cabs Requested") +labs(fill="Pickup Point")
plot_pickup

grid.arrange(plot_status,plot_pickup, ncol =2)
#morning and evening the demand is at peak.Cancellation is high in morning;'no cars are available' is high in evening


#creating new df for trip completed cases to calculate the average journey time.
#there is not much diff in the mean journey time per slots
trips_completed_df <-uber_df %>% filter(Status == "Trip Completed")
Journey.time_perslot <- aggregate(trips_completed_df$Journey.time~trips_completed_df$request.slot, data = trips_completed_df, mean)

#plotting box plot ; the median journey time from city to airpot is highest for 04-08 hours
#however the median for other slots have not much difference.
#median journey time not conclusive 
ggplot(trips_completed_df, mapping = aes(y = trips_completed_df$Journey.time, x = trips_completed_df$request.slot, fill = trips_completed_df$Pickup.point))+geom_boxplot()
+ggtitle("Cab Demand per Slot")+labs(x="Time slots", y="Journey Time") +labs(fill="Pickup Point")
#subsetting 4 columns 


#creating new df as for peak hours 04-08 , 08-12,17-20,20-22
uber_peakhr <- uber_df %>% filter(request.slot %in% c("04-08","08-12","17-20","20-22"))
peakhr_cab_supply <- aggregate(Driver.id~Pickup.point + Status,uber_peakhr,length)



#calculating wait/idle time for the driver
#creating data frame for completed trips 
completed_trip <- uber_df %>% filter(!is.na(Driver.id) & Status =="Trip Completed") %>% 
  group_by(Driver.id)

#index for driver id
trip_count <- data.frame(table(completed_trip$Driver.id))[,2]

#creating empty vectors
waittime = vector()
driver_id = vector()
temp = vector()
rownum = vector()
index =0

#creating function to calculate the idle time at airport

idle_time <-function(dt, pickup_point,drop_point) {
  
  for (i in 1:length(trip_count)) {
    
    for(j in 1:trip_count[i]) {
      k = sum(index,j)
      
      if(dt$Pickup.point[k] == pickup_point & dt$Pickup.point[k+1] == drop_point) {
        
        timediff = difftime(dt$Request.timestamp_new[k+1], dt$Drop.timestamp_new[k], units = "hours") 
        idletime_mins =  str_extract_all(timediff,"\\d+", simplify = TRUE)[,1]
       
        waittime[length(waittime)+1] = idletime_mins
        driver_id[length(driver_id)+1] = i
        rownum[length(rownum)+1]= index+j
      }
    }
    temp[i] = trip_count[i]
    index = sum(temp)
    } 
  
  dat_frame = data.frame(rownum, driver_id,waittime)
  return(dat_frame)
}


idle_airport <- idle_time(completed_trip,"City","Airport")

idle_airport$waittime <- as.numeric(as.character(idle_airport$waittime))
# ASSUMPTION: Max. Waiting time taken as 14 Hrs
idle_airport <-idle_airport[which(idle_airport$waittime <= 14),]
mean_idle_airport <- round(aggregate(waittime~driver_id, idle_airport,mean),1)

rowindex <-idle_airport$rownum
df_waittime_airport <- completed_trip[rowindex,]
df_waittime_airport$idle_time <- idle_airport$waittime

#plotting using boxplot to display median time . Waiting time/idle time seems to be higher 
#at airpot which is discouraging drivers to accept the request
ggplot(df_waittime_airport, aes(x = df_waittime_airport$request.slot, 
                                y = df_waittime_airport$idle_time)) +geom_boxplot()


write.csv(df_waittime_airport, file = "df_waittime_airport.csv") # exporting it for tableau analysis 

