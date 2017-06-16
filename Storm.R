library(data.table)
library(R.utils)

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

healthdata<-data[c("EVTYPE","FATALITIES","INJURIES")]

healthdata<-healthdata[!grepl("Summary", healthdata$EVTYPE,ignore.case=TRUE),]
TotHealth<-aggregate(cbind(FATALITIES,INJURIES)~EVTYPE,data=healthdata,FUN=sum, na.rm=TRUE)

TotHealth<-TotHealth[TotHealth$FATALITIES!=0 & TotHealth$INJURIES!=0,]

ryan_data <- data %>%
  select(EVTYPE, FATALITIES, INJURIES) %>%
  group_by(EVTYPE) %>%
  summarize(TOTAL_FATS = sum(FATALITIES),
            TOTAL_INJS = sum(INJURIES))

ryan_data_2 <- data %>%
  select(EVTYPE, FATALITIES, INJURIES) %>%
  group_by(EVTYPE) %>%
  summarize(TOTAL_FATS = sum(FATALITIES),
            TOTAL_INJS = sum(INJURIES))