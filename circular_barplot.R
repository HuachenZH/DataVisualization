## load library
library(readr)
library(ggplot2)
library(gganimate)
library(rgdal)
library(RColorBrewer)
library(dplyr)
library(circlize)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(caTools)
thisPageData <- read_csv("C:/Users/eziod/Pictures/S7/DataVisualisation/Project/thisPageData.csv")
names(thisPageData)[9]<-"year"
names(thisPageData)[10]<-"value"

## circular barplot ----------------------------------------
barcir<-thisPageData[thisPageData$year==2019,]
barcir<-data.frame(barcir$geo,barcir$year,barcir$value,barcir$full_name,barcir$region)
colnames(barcir)<-c("geo","year","value","full_name","region")
# Set a number of 'empty bar' to add at the end of each group
emptyBar<-3
toAdd<-data.frame(matrix(NA,emptyBar*nlevels(as.factor(barcir$region)),ncol(barcir)))
colnames(toAdd)<-colnames(barcir) #si je change pas le nom :Error in match.names(clabs, names(xi)) : 名字同原来已有的名字不相对
toAdd$region<-rep(levels(as.factor(barcir$region)),each=emptyBar)
# add toAdd into barcir, with row bind
barcir<-rbind(barcir, toAdd)
# grouper par groupe
barcir <- barcir %>% arrange(region,value) # désordre sans "value"
barcir$id<-seq(1,nrow(barcir))
# prepare a dataframe for base lines
base_data <- barcir %>% 
  group_by(region) %>% 
  summarize(start=min(id), end=max(id) - emptyBar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))
# get the name and the y position of each label
label_barcir<-barcir
number_of_bar<-nrow(label_barcir)
angle<-90-360*(label_barcir$id-0.5)/number_of_bar
label_barcir$hjust<-ifelse(angle< -90,1,0)
label_barcir$angle<-ifelse(angle< -90, angle+180, angle) #attention, il faut angle< -90, sinon erreur avec angle<-90
# make the plot
p_barcir<-ggplot(barcir, aes(x = as.factor(id),y=value, fill=region))+
  geom_bar(stat="identity",alpha=0.5)+
  ylim(-10,20)+
  theme_minimal()+
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")
  )+
  coord_polar()+
  geom_text(data=label_barcir, aes(x=id, y=value+2,label=paste(full_name, value, sep=" , "), hjust=hjust), color="black",fontface="bold", alpha=0.6, size=4, angle= label_barcir$angle, inherit.aes = FALSE)+
  geom_segment(data=base_data, aes(x = start, y = -1, xend = end, yend =-1), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -1.5, label=region), hjust=c(1,1,1,0,0), colour = "black", alpha=0.8, size=2, fontface="bold", inherit.aes = FALSE)


p_barcir



