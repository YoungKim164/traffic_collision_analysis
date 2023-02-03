library(opendatatoronto)
library(dplyr)
library(tidyverse)
library(janitor)
library(data.table)
library(zoo)
library(chron)
library(lubridate)
# get package
package <- show_package("ec53f7b2-769b-4914-91fe-a37ee27a90b3")
package

# get all resources for this package
resources <- list_package_resources("ec53f7b2-769b-4914-91fe-a37ee27a90b3")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# load the first datastore resource as a sample
data <- filter(datastore_resources, row_number()==1) %>% get_resource()
data

#################chart1############################

#read data
df=read.csv(file = "inputs/data/raw_data.csv")


#count per category
INJ_count<-0
FRT_count<-0
PD_count<-0

for (x in df[["Injury_Collisions"]]) {
  if (x=="YES"){
    INJ_count=INJ_count+1
  }
}

for (x in df[["FTR_Collisions"]]) {
  if (x=="YES"){
    FRT_count=FRT_count+1
  }
}

for (x in df[["PD_Collisions"]]) {
  if (x=="YES"){
    PD_count=PD_count+1
  }
}

#create array of count
a<-c("INJ_count", "FRT_count", "PD_count")
b<-c(INJ_count/553780*100, FRT_count/553780*100, PD_count/553780*100)
acctype <- data.frame(a, b)

#bargraph of category
ggplot(data=acctype, aes(x=a, y=b)) + 
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "accident case", y = "percentile") + 
  theme_minimal(base_size = 14) + 
  ylim(0, 100)

#########chart2######################

#reorganize date and time
dtparts = as.data.frame(strsplit(df$OccurrenceDate,"T"))
row.names(dtparts) = NULL
thetimes = chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))

#count by hours
occurrencedate<-as.Date.character(dtparts[1,])
hours<-hms(as.character(dtparts[,2]))
Hour_count<-df %>% group_by(Hour) %>% fill(Hour) %>% count()
Hour_count[24,1]=24

#bargraph of hourly
ggplot(data=Hour_count, aes(x=Hour,y=n)) + 
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "accident hour", y = "cases") + 
  ylim(0, 50000) +
  scale_x_continuous(expand=expand_scale(mult=c(0.01, 0.01)))

###############chart3#######################

#count by month
month_count<-df %>%
  group_by(month = lubridate::floor_date(occurrencedate, 'month')) %>% count()

#line graph
plot(month_count$month, month_count$n, type = "l")
