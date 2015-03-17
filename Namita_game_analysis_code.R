library("ggmap")
library("lubridate")
library("plyr") 
library("dplyr")
library("forecast")
#if(file.exists("/home/namita/DataEngineerProject/file_name"))
#  load("/home/namita/DataEngineerProject/file_name")
setwd("/home/namita/DataEngineerProject")
raw_data<-read.csv("AA-1_2014-17-08_2014-19-08.csv", header=TRUE,sep = ",",as.is =TRUE,stringsAsFactors=FALSE)
names(raw_data)
```
Then I check for total NA's in the raw data and further found columns having NA's from summary of data.
```{r, echo=F, warning = F, message = F}
na <- sum(is.na(raw_data))
cat ("Total NA's : " , na) 
summary(raw_data[, c(1,4,5,9)])
```
It is clear that the data has 11 NA's in "userId" and 45 NA's in "attempt" columns. "attempt" column also has some negative values. Last four columns looks optional as they have fewer entries and the purpose of putting them in data is also not clear from labels. First I fix the problem of NA's in "userId". As it is clearly visible from 

```{r, echo=F, warning = F, message = F}
raw_data[c(1,2), c(4,5,6)]
#excluding last 4 columns
new_data<-raw_data[ ,-c(13,14,15,16)]
```
"userId" is first part of the "deviceId" so I replaced them with correct userId after parsing and splitting "deviceId". 
```{r, warning = F, message = F}
#finding index of NA's of userId and replacing them with correct values.
nas_in_userId <- which(is.na(new_data$userId))
for (i in nas_in_userId)
{
  new_data[i,5]<-(strsplit(new_data[i,6], "_")[1])
}
```
The data also has duplicate values. 
```{r, echo = F, warning = F, message = F}
du <- sum(duplicated(new_data))
cat ("Number of duplicate rows : " , du) 
```
I found that rows 2954 and 2955 are identical. I used unique command to remove duplicate rows. 
```{r, echo = F, warning = F, message = F}
#new_data[duplicated(new_data), ]
# 2954 and 2955 are identical.
#removing duplicate entry
new_data<-unique(new_data)
```
For NA's and negative values in attempt column. First I analyzed data for missing or NA values of attempt. 
```{r, echo = F, warning = F, message = F, fig.width=10.5, fig.height=10, fig.align='center'}
#finding index of NA values of attempt
nas_in_attempt <- which(is.na(new_data$attempt))
na_attempt_data <- new_data[nas_in_attempt, c(9,11)] # all data entries {} and mostly has action Release_lever
na_attempt_data$action <- as.factor(na_attempt_data$action)
na_attempt <- as.data.frame(table(na_attempt_data$action ))
names(na_attempt) <- c("Action", "Count")
# Plot
base_g <- ggplot(na_attempt, aes(x = Action, y = Count, fill = Action)) + geom_bar(stat="identity")
base_g + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="none") + ggtitle("Distribution of Action \nfor missing values of attempt")
```
It is clear from the above graph that most missing values for attempt are in action category- "Release_lever" and their values in "data" column are also missing. I stored data for negative attempts in sepearate dataframe for further analysis.

```{r, echo = F, warning = F, message = F}
#only removing NA's of attempt column
new_data<-new_data[complete.cases(new_data[,9]),]
#summary(new_data)
#Getting negative attempts
negative_attempt_data<-subset(new_data, new_data$attempt<0)
#dim(negative_attempt_data) # [1] 834  12
```
### Problem 1: Using one or more statistical tool, preferably coded in some language like R. Extract the number of times students have performed an action ( eg: ‘fused core’ ). 

#### Analysis: The list of actions is :

```{r, echo = F, warning = F, message = F, fig.width=5, fig.height=8, fig.align='center'}
pb1_data<- ddply(new_data, .(userId, activityId, action, attempt), summarize, Total_time_played = sum (totalTimePlayed))
unique_actions <-unique(pb1_data$action)
pb1 <- new_data[which(new_data$attempt>=0), c(9,11)]
pb1_table <- as.data.frame(table(as.factor(pb1$action)))
names(pb1_table) <- c("Action", "Count_of_events")
pb1_table
```

```{r, echo = F, warning = F, message = F, fig.width=10.5, fig.height=10, fig.align='center'}
ggplot(pb1_table, aes(x = Action, y = Count_of_events, fill = Action )) + geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="none") + ggtitle("Distribution of events\n for all actions")

```
As some actions were not visible in above graph, though they represent actual numbers. There is lot of variation in attempt column as the minimum value is 1 and maximum reaches to 45. I made the bar graph again using log
```{r, echo = F, warning = F, message = F, fig.width=10.5, fig.height=10, fig.align='center'}
ggplot(pb1_table, aes(x = Action, y = log(Count_of_events)+1, fill = Action )) + geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="none")+ ggtitle("Distribution of events\n for all actions on log scale")
```
Code for extracting number of times students have performed an action i.e. "Fuse core"
```{r, warning = F, message = F, fig.width=10.5, fig.height=10, fig.align='center'}
count <- pb1_table[(pb1_table$Action == "Fuse_core"), 2]
cat("Number of attempts for Fused core :", count)
``` 
### Problem 2 & 3. Determine some patterns on session open & close. Provide the algorithm to do so and any/all code used to demonstration marking open/close of session. 
####Analysis : For the analysis of session I analysed the data in virtual and real time. The virtual time analysis is based on "clienttimestamp" and real time time analysis is based on "totaltimeplayed".

#### Graphs in real time

```{r, echo = F, warning = F, message = F, fig.width=8, fig.height=9, fig.align='center'}
#THat's how I analysed as dates
ud<-as.data.frame(sapply(new_data,gsub,pattern="/46",replacement="/"))
add_secs_from_start<-function(df) {
  sec_from_start <- vector(mode = "numeric", length = 0)
  for (i in 1:dim(df)[1]) 
  {
    new_column<-difftime(strptime(as.character(df$clientTimeStamp[i]), "%m/%d/%Y %H:%M:%S"), strptime(as.character(df$clientTimeStamp[1]), "%m/%d/%Y %H:%M:%S"),unit="secs")
    sec_from_start <- c(sec_from_start,new_column)
  }
  return(cbind(df,sec_from_start))
}
ud<-add_secs_from_start(ud)
#str(ud)
conversion_numeric <- function(f){as.numeric(levels(f))[f]}
ud$totalTimePlayed <- conversion_numeric(ud$totalTimePlayed)
df.list <- with(ud, split(ud, list(userId), drop=TRUE))
# Calculating for user "6707", it can be extended for all users
d <-df.list[['6707']]
prev<-100000;
prev_sess<-"nnnnnnnnnnnnnnnn"
for (i in 1:dim(d)[1]) {
  if (as.numeric(d$totalTimePlayed[i]) >= prev ) {
    d$time_event[i] <- (as.numeric(d$totalTimePlayed[i]) - prev)
    prev <- (d$totalTimePlayed[i])
    d$session_start_time[i] <- 0      # session start flag based on time
  }
  if (as.numeric(d$totalTimePlayed[i]) < prev ) { 
    d$session_start_time[i] <- 1 
    d$time_event[i]<- 0
    prev <- as.numeric(d$totalTimePlayed[i])
  }
  if (grepl(prev_sess, as.character(d$sessionId[i]))) {
    d$session_start[i]<- 0       # session start flag based on session id
    prev_sess <-  as.character(d$sessionId[i])
    #session_start <- c(session_start,0)      
  }
  else {
    d$session_start[i]<- 1
    #session_start <- c(session_start,1)
    prev_sess <-  as.character(d$sessionId[i])
  }
}

newd <-d[,c(4,5,7,8,11,14,15,16)]
total_time_activity <- ddply(newd, .(activityId), summarise, total_time = sum(time_event))
total_time_action <- ddply(newd, .(action), summarise, total_time = sum(time_event))

#Bar graph for time spent in different activity of user "6707"
total_time_activity
ggplot(total_time_activity, aes(x = activityId, y = total_time, fill = activityId )) + geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="none")+ ggtitle("Bar graph for time spent\n in different activity by user-6707")
```
From the above graph it is clear that user-6707 mostly spent his time in Exploration activity and he also tried "Backing generator" which other users did not try. 
```{r, echo = F, warning = F, message = F, fig.width=12, fig.height=11, fig.align='center'}
total_time_action
#Bar graph for time spent in different action of user "6707"
ggplot(total_time_action, aes(x = action, y = total_time, fill = action )) + geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="none")+ ggtitle("Bar graph for time spent\n in different action by user 6707")

```
In the above graph missing bars represent that the user did not perform those actions. This analysis can be extended for other users also.

#### Graphs in virtual time
```{r, echo = F, warning = F, message = F, fig.width=15, fig.height=10, fig.align='center'}

#graph for first user
user1 <-df.list[['6707']]
user2<-df.list[['6721']]
user3<-df.list[['6593']]
user1_data<-user1[, c(7,8,13)]
user2_data<-user2[, c(7,8,13)]
#plot for session userId = '6707', played mostly Exploration
user1_dat <-subset(user1_data, user1$sessionId == "f3fc62f0-265f-11e4-9600-69986cc0e309")
ggplot(data=user1_dat, aes(x=sec_from_start, y=activityId,group=1)) + geom_line() + geom_point()+ ggtitle("Sequence of activities\n from most active user (userId = '6707')")
#plot for session userId = '6711', played in only one session
user2_dat <-subset(user2_data, user2$sessionId == "ee76b450-27dd-11e4-9845-175fcf1a3e72")
ggplot(data=user2_dat, aes(x=sec_from_start, y=activityId,group=1)) + geom_line() + geom_point()+ ggtitle("Sequence of activities\n from user who played only one session (userId = '6721')")

```
From graph it is clear that user "6707" is very active and he tried every game except botbox. 
From graph it is also clear that "Exploration" is most popular among all users of data. This is how I found pattern in sessions using time and sessionId column.

### Problem 4. Find one or more other interesting pattern in the data.
### Analysis part 1   
In this section I first analysed data for negative "attempt". "attempt" column has only -1, negative value for 834 rows. It is interesting to know that maximaum negative values occur for map activity and acess_map, enter_room, leave_room actions.
```{r, echo = F, warning = F, message = F, fig.width=8, fig.height=9, fig.align='center'}
neg_data<- negative_attempt_data[ , c(7,11,9)]
#dim(neg_data)
#[1] 834   3
table_activity <- as.data.frame(table(as.factor(neg_data$activityId)))
names(table_activity) <- c("Activity", "Total_number_of_negative_attempts")
ggplot(table_activity, aes(x = Activity, y = Total_number_of_negative_attempts, fill = Activity )) + geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="none")

table_action <- as.data.frame(table(as.factor(neg_data$action)))
names(table_action) <- c("Action", "Total_number_of_negative_attempts")
ggplot(table_action, aes(x = Action, y = Total_number_of_negative_attempts, fill = Action )) + geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="none")
```
### Analysis part 2   
In this part I analysed data for "totalTimePlayed" having 0 values. All those values occurred only for Exploration activity and for chapter_start and quest_start actions.
```{r, echo = F, warning = F, message = F, fig.width=8, fig.height=9, fig.align='center'}
ud<-as.data.frame(sapply(new_data,gsub,pattern="/46",replacement="/"))
add_secs_from_start<-function(df) {
  sec_from_start <- vector(mode = "numeric", length = 0)
  for (i in 1:dim(df)[1]) 
  {
    new_column<-difftime(strptime(as.character(df$clientTimeStamp[i]), "%m/%d/%Y %H:%M:%S"), strptime(as.character(df$clientTimeStamp[1]), "%m/%d/%Y %H:%M:%S"),unit="secs")
    sec_from_start <- c(sec_from_start,new_column)
  }
  return(cbind(df,sec_from_start))
}
ud<-add_secs_from_start(ud)
#str(ud)
conversion_numeric <- function(f){as.numeric(levels(f))[f]}
ud$totalTimePlayed <- conversion_numeric(ud$totalTimePlayed)
pb2_data<-ud[ ,c(5,4,7,8,9,10,11,13)]
taotal_zero<-sum(pb2_data$totalTimePlayed == 0)
time_zero<-pb2_data[which(pb2_data$totalTimePlayed == 0), ]
time_zero
ggplot(time_zero, aes(x = action, y = log(sec_from_start), color= userId )) + geom_point()
```
### Analysis part 3   
There are six devices. Same device is used by multiple users, this can be infer from following table:
  ```{r, echo = F, warning = F, message = F, fig.width=8, fig.height=9, fig.align='center'}
device_data<-new_data
for (i in 1:14097)
{
  device_data[i,13]<-unlist(strsplit(device_data[i,6], "_"))[2]
}
colnames(device_data)[13] <- "only_device_id"
#names(device_data)
unique(device_data$only_device_id)
final_device_data <- device_data[, c(5,13)]
final<-unique(final_device_data) # list of userIds and corresponding deviceIDs
final