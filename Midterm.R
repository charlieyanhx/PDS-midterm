#PDS Midterm
#Charlie Yan

#Read data
data<-read.csv('http://politicaldatascience.com/PDS/Datasets/Pres_Approval.csv', stringsAsFactors = F)

#library packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(tokenizers)

#1 Data cleaning
#Rename the variable “Trump” to be “Name”
data<-rename(data, Name=Trump)

#Make a new variable that called “Year” that just includes the year of the survey
date<-separate(data,Date,c("date"),sep=" ") #use seperate to help me store 'Year' in date temporarily
data<-mutate(data, Year=date$date) #add the data from 'date' to 'data'
remove(date) #date is useless after the last step, so I removed it

#Remove the variable “No Opinion”
data<-select(data,-No.Opinion) 

#Make a new variable called “NetApproval” that represents Percent Approve - Percent Disapprove
data<-mutate(data, NetApproval=Percent.Approve-Percent.Disapprove)

#2 Some basic visualization
#Create a plot that shows approval by year.
ggplot(data=data)+
  geom_point(mapping = aes(x=Year, y=Percent.Approve))

#Now alter the plot to separate out this visualization by president.
data<-group_by(data, Year, Name)%>%mutate(Average.Yearly=mean(Percent.Approve))#avaerage of approval rate in a year

ggplot(data=data)+
  geom_point(mapping = aes(x=Year, y=Percent.Approve, color=Name))+
  geom_point(mapping = aes(x=Year, y=Average.Yearly, col='Average'))+ #graph the variable I created earlier
  labs(title="Presidential Approval",x ="Year", y = "Approval (%)")+
  theme_minimal() #I choose to create a dot plot becasue I thought it is the most straight 
#forward way to show approval rating by year, especially many years includes data from two presidents

#3 Making data play nice with other data
#Read data
data2<-read_csv('https://politicaldatascience.com/PDS/Datasets/State_Union.csv')

#Using the year and the name of the president, you are going to merge in data into the state of the
#union data. For each speech, you will be be adding in variables that are the mean level of Percent Approve and mean 
#NetApproval. So NetApproval would be the mean level of NetApproval for that president in the year the speech was given. 
#Be careful here. Is this join going correctly? Check (and show me how you checked.)
data3<-rename(data, President=Name) #rename for join
data2$Date<-as.Date(data2$Date, "%d-%b-%y") #convert date format
date<-separate(data2,Date,c("date"),sep="-") #splite date to create year
data2<-mutate(data2, Year=date$date)#add new column year in data2
remove(date)#remove date

#method 1 will produce a data frame 876 obs and 9 variables as method 2 will produce a data frame contains only 24 obs and 6 vars

#method 1
data4<-data3 %>% inner_join(data2, by=c("Year","President")) #remove the observations made in years do not have state of union by using inner join
union(names(data2),names(data3))
names(data4)
#join correctly since the last tww line produce almost identical output except "date", becasue we did not joing by the variable date

data4<-group_by(data4,Year, President)%>%mutate(Average.Yearly=mean(Percent.Approve)) # average for approval
data4<-group_by(data4,Year, President)%>%mutate(NetAverage.Yearly=mean(NetApproval)) # average for net approval


#method 2
#I just thought this is cleaner, to run this code you need to run the code before method 1 again
data3<-group_by(data3, Year)%>%mutate(NetAverage.Yearly=mean(NetApproval))#add new column avg net approval
data3<-select(data3,c("Average.Yearly", "NetAverage.Yearly", "President"))#trim the data set to only variables useful for merge 
data3<-distinct(data3) # remove repeated observations
mergeData<-data3 %>% inner_join(data2, by=c("Year","President")) #proceed join


#4 Text
# I used data frame produced by method 2 for this problem
#Create variables that represents 
#(a) the number of words in each speech,
data4<-mutate(data4,Number.Words=lengths(str_split(Text, " "))) #split string first then count the length of the list
#(b) the number of words that in in ly (a rough way to approximate the number of adverbs), and 
data4<-mutate(data4,Number.Adverbs=str_count(Text,"ly"))
#(c) references to the economy/economic growth/jobs. This is a speech-level dataset.
data4<-mutate(data4,Econ.Words=(str_count(Text,"economy")+str_count(Text,"economic")+str_count(Text,"jobs")+str_count(Text,"growth")))

#Create a plot that shows the by-year relationship between NetApproval and mentions of jobs/economy.
ggplot(data=data4)+ 
  geom_point(mapping = aes(x=Year, y=NetAverage.Yearly, color = "Net Approval"))+ #graph Net Approval vs year
  geom_point(mapping = aes(x=Year, y=Econ.Words, color = "mentions of jobs/economy")) #graph Net Approval vs mentions

#Make a new dataset so each row now represents a single sentence. It should still contain the polling
#data you merged in before.
data5<-data4 %>% 
  separate_rows(Text, sep = "[.]" , convert = FALSE)

#For each sentence, make a variable that indicates whether or not it references economy/economic
#growth/jobs.
data5<-mutate(data5,Contain=(str_detect(Text,"economy")||str_detect(Text,"economic")||str_detect(Text,"growth")||str_detect(Text,"jobs")))
#using three logical operator between four str detect to determine whether the sentence references economy

#Now add back into the speech-level dataset a variable that indicates the proportion of sentences that
#are about the economy.
#code below is not working
data5%>%group_by(Year)%>% #group by year first, and count how many sentences have true in Contain
data4<-mutate(data4, Sentences=length(tokenize_sentences(Text))) #devide the amount above to the total sentence to receive the rate

