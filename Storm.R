library(data.table)
library(R.utils)
library(ggplot2)

#Data loading and Data processing
Zip_name="Data/repdata%2Fdata%2FStormData.csv.bz2"
file_name="Data/dataset.csv"

bunzip2(Zip_name, file_name, remove = FALSE, skip = TRUE)

colsNeeded<-c("STATE__","BGN_DATE","STATE","EVTYPE","FATALITIES","INJURIES",
              "PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")

originaldata<-fread(file_name, header=TRUE, select=colsNeeded, verbose=TRUE)

data<-originaldata

data$BGN_DATE<-as.Date(data$BGN_DATE,"%m/%d/%Y")
data$BGN_YEAR<-as.numeric(format(data$BGN_DATE, "%Y"))

#Getting Health data
data<-as.data.frame(data)

healthdata <- data %>%
  select(EVTYPE, FATALITIES, INJURIES) %>%
  filter(!grepl("Summary",EVTYPE,ignore.case=TRUE))%>%
  group_by(EVTYPE) %>%
    summarize(TOTAL_FATS = sum(FATALITIES),
              TOTAL_INJS = sum(INJURIES))%>%
  filter(TOTAL_FATS>0 & TOTAL_INJS>0)

healthdataFatality<-healthdata[order(-healthdata$TOTAL_FATS),]
healthdataInjury<-healthdata[order(-healthdata$TOTAL_INJS),]

p1<-ggplot(head(healthdataFatality,10), aes(x=reorder(EVTYPE, -TOTAL_FATS),y = TOTAL_FATS)) +
          geom_bar(stat="identity",fill="#0072B2")+
          labs(x = "Event Types",y="Total Fatalities",title="Top 10 Most Fatatity Causing Events")+
          theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, hjust = 1),
                panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank())

p2<-ggplot(head(healthdataInjury,10), aes(x=reorder(EVTYPE, -TOTAL_INJS),y = TOTAL_INJS)) +
  geom_bar(stat="identity",fill="#0072B2")+
  labs(x = "Event Types",y="Total Injuries",title="Top 10 Most Injury Causing Events")+
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

library(gridExtra)
library(grid)
grid.arrange(p1, p2, ncol=2,bottom = textGrob("Fig. 1",gp=gpar(fontface = 'bold')))



ryan_data_2 <- data %>%
  select(EVTYPE, FATALITIES, INJURIES) %>%
  group_by(EVTYPE) %>%
  summarize(TOTAL_FATS = sum(FATALITIES),
            TOTAL_INJS = sum(INJURIES))