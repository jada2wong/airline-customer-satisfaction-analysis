setwd("~/Desktop")
#IST 687 Project
###################################
#Read the raw data of Satisfaction Survey.
install.packages("readr")
library(readr)
Satisfaction_Survey <- read_csv("Survey.csv")
####################################
#Count and omit the rows with NA.
Satisfaction_Survey_2 <-na.omit(Satisfaction_Survey)
print(paste("Rows with NA =",121040-118755))
####################################
#Summary descriptive of the raw data
library(dplyr)
glimpse(Satisfaction_Survey_2)
summary <- Satisfaction_Survey_2 %>% 
  summarise(mean=mean(Satisfaction),median=median(Satisfaction),max=max(Satisfaction),min=min((Satisfaction)),
            sd=sd(Satisfaction),percentile_95=quantile(Satisfaction,0.95))
summary
####################################
#Make histogram and boxplot plots.
library(ggplot2)
ggplot(Satisfaction_Survey_2,aes(x=Satisfaction))+
  geom_histogram(bins=5,fill="white",color="black")
ggplot(Satisfaction_Survey_2,aes(x=factor(0),y=Satisfaction))+
  geom_boxplot()
####################################
#Make correlation plot of numeric variables.
install.packages("ggcorrplot")
library(ggcorrplot)
num <-select_if(Satisfaction_Survey_2,is.numeric)
corr <- cor(num)
ggcorrplot(corr)
####################################
#Do the map
library(ggplot2)
library(ggmap)
library(dplyr)
colnames(Satisfaction_Survey_2) <- gsub(" ","",colnames(Satisfaction_Survey_2))
colnames(Satisfaction_Survey_2) <- gsub("%","",colnames(Satisfaction_Survey_2))

survey_map_origin <- Satisfaction_Survey_2[c(1,19)]
survey_map_origin <- survey_map_origin %>% 
  group_by(OriginState) %>% 
  summarise(average_sat=mean(Satisfaction))
survey_map_origin <-survey_map_origin[-45,]

state_data <- data.frame(state.area , state.center , state.name)
names(state_data)[names(state_data)=="state.name"] <- "OriginState"
survey_map_origin <- merge(survey_map_origin , state_data )

us <- map_data("state")
colnames(survey_map_origin)[1] <- 'state'
colnames(survey_map_origin)[4] <- 'lon'
colnames(survey_map_origin)[5] <- 'lat'
survey_map_origin$state <- tolower(survey_map_origin$state)

map.sat <- ggplot(survey_map_origin, aes (map_id = state)) +
  geom_map(map = us , aes(fill = average_sat))+
  expand_limits(x = us$long , y = us$lat)+
  coord_map()+
  ggtitle("Average Satisfaction of Origin States")
map.sat 

survey_map_des <- Satisfaction_Survey_2[c(1,21)]
survey_map_des <- survey_map_des %>% 
  group_by(DestinationState) %>% 
  summarise(average_sat=mean(Satisfaction))

survey_map_des <- merge(survey_map_des , state_data )

us <- map_data("state")
colnames(survey_map_des)[1] <- 'state'
colnames(survey_map_des)[4] <- 'lon'
colnames(survey_map_des)[5] <- 'lat'
survey_map_des$state <- tolower(survey_map_des$state)

map.sat <- ggplot(survey_map_des, aes (map_id = state)) +
  geom_map(map = us , aes(fill = average_sat))+
  expand_limits(x = us$long , y = us$lat)+
  coord_map()+
  ggtitle("Average Satisfaction of Destination States")
map.sat 

####################################
category_1 <- function(vec){
  q <- quantile(vec, c(0.4, 0.6))
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec <= q[1]] <- "Low"
  vBuckets[vec > q[2]] <- "High"
  vBuckets
}
survey_category_1 <- sapply(Satisfaction_Survey_2[c(3,6,7,11,12,22,23)],category_1)

category_2 <- function(vec){
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec >= 3.5] <- "High"
  vBuckets[vec < 3.5] <- "Low"
  vBuckets
} 
survey_category_2 <- sapply(Satisfaction_Survey_2[c(1,5)],category_2)
survey_category_3 <- sapply(Satisfaction_Survey_2[c(2,4,9,13,25,28)],as.factor)
survey_category <- data.frame(cbind(survey_category_1,survey_category_2,survey_category_3))
library(arules)
library(arulesViz)
survey_categoryX <- as(survey_category,"transactions")
itemFrequencyPlot(survey_categoryX) 
rules <- apriori(survey_categoryX,parameter = list(supp=0.2, conf=0.85),appearance = list(rhs="Satisfaction=Low",default="lhs"))
plot(rules)
inspect(rules)
rules.sorted <- sort(rules, decreasing=TRUE, by="confidence")
inspect(rules.sorted)
