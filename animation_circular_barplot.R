## load library
library(readr)
library(ggplot2)
library(gganimate)
library(rgdal)
library(RColorBrewer)
library(dplyr)
library(circlize)
library(tidyverse)
# animation circular barplot
# animation circular barplot
# animation circular barplot
# animation circular barplot
# animation circular barplot
# animation circular barplot
# animation circular barplot
# animation circular barplot
# animation circular barplot
# animation circular barplot
# animation circular barplot
# animation circular barplot
# animation circular barplot
# pas dans ordre, sans Luxembourg-------------------------------------------------------
thisPageData <- read_csv("C:/Users/eziod/Pictures/S7/DataVisualisation/Project/thisPageData.csv")
names(thisPageData)[9]<-"year"
names(thisPageData)[10]<-"value"

barcir<-thisPageData
barcir<-data.frame(barcir$geo,barcir$year,barcir$value,barcir$full_name,barcir$region)
colnames(barcir)<-c("geo","year","value","full_name","region")
# enlever Luxembourg
barcir<-barcir[-which(barcir$geo=="LU"),]

# Set a number of 'empty bar' to add at the end of each group
emptyBar<-5
toAdd<-data.frame(matrix(NA,emptyBar*nlevels(as.factor(barcir$region)),ncol(barcir)))
colnames(toAdd)<-colnames(barcir) #si je change pas le nom :Error in match.names(clabs, names(xi)) : 名字同原来已有的名字不相对
toAdd$region<-rep(levels(as.factor(barcir$region)),each=emptyBar)
toAdd$geo<-rep(levels(as.factor(barcir$region)),each=emptyBar)

# add toAdd into barcir, with row bind
barcir<-rbind(barcir, toAdd)
# grouper par groupe
barcir <- barcir %>% arrange(region) 
barcir$id<-seq(1,nrow(barcir))
# barcir$xaxis<-rep(1:nlevels(as.factor(barcir$geo)), each=length(2011:2019))

# create id2
j=1
for(i in 2:nrow(barcir)){
  if(barcir[i,1]==barcir[i-1,1]){
    barcir[i-1,7]<-j
  }else{
    barcir[i-1,7]<-j
    j<-j+1
    barcir[i,7]<-j
  }
}
barcir[nrow(barcir),7]<-j
colnames(barcir)[7]<-"id2"

# prepare a dataframe for base lines
base_data <- barcir %>% 
  group_by(region) %>% 
  summarize(start=min(id2), end=max(id2)-1   ) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))



# get the name and the y position of each label
label_barcir<-barcir
# label_barcir<-barcir[barcir$year==2011,]
# number_of_bar<-nrow(label_barcir)
number_of_bar<-nlevels(as.factor(barcir$id2))
angle<-90-360*(label_barcir$id2-0.5)/number_of_bar

angle2<-unique(angle)

label_barcir$hjust<-ifelse(angle< -90,1,0)
label_barcir$angle<-ifelse(angle< -90, angle+180, angle) #attention, il faut angle< -90, sinon erreur avec angle<-90
# make the plot
p_barcir<-ggplot(barcir, aes(x = as.factor( id2  ),y=value, fill=region))+
  geom_bar(stat="identity",alpha=0.5)+
  ylim(-7,10)+
  theme_minimal()+
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")
  )+
  coord_polar()+
  geom_text(data=label_barcir, aes(x=id2, y=value+2,label=paste(full_name, value, sep=" , "), hjust=hjust), color="black",fontface="bold", alpha=0.6, size=4, angle= label_barcir$angle, inherit.aes = FALSE)+
  
  geom_segment(data=base_data, aes(x = start, y = -1, xend = end, yend =-1), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -1.5, label=region), hjust=c(1,1,1,0,0), colour = "black", alpha=0.8, size=2, fontface="bold", inherit.aes = FALSE)+
  
  # gganimate staff
  transition_states(
    year,
    transition_length=2,
    state_length = 1
  )+
  ease_aes('sine-in-out')

animate(p_barcir, duration=5, fps=30, width=700, height=700, renderer=gifski_renderer())
# Save at gif:
anim_save("288-animated-barplot-transition.gif")

p_barcir
