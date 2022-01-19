# Assembly

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


# import dataset ---------------------------------------
thisPageData <- read_csv("C:/Users/eziod/Pictures/S7/DataVisualisation/Project/thisPageData.csv")
names(thisPageData)[9]<-"year"
names(thisPageData)[10]<-"value"



## lineplot luxembourg ----------------------------------
#create a dataset for luxembourg
year<-2011:2019
temp<-thisPageData$geo=="LU"
population<-as.vector(unlist(thisPageData[temp,10]))
luxembourg<-data.frame(year,population)
#time to ggplot
ggplot(luxembourg,aes(x=year,y=population))+
  geom_line(color="#69b3a2", size=2, alpha=1, linetype=1)+
  geom_text(label=population)+
  labs(title="Immigration change of Luxembourg",caption="values are in percentage of total population",y="% of population")+
  scale_x_continuous(limits=c(2011,2019), breaks=seq(from=2011, to=2019, by=1))



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




# choropleth tentative 2 ------------------------------------------------------
#as usual, load the data
choro<-thisPageData[thisPageData$year==2019,]
# the average value of 2011 to 2019
# disadvantage of aggregate, it will generate two columns, one for type, one for value
tmp<-aggregate(thisPageData$value, by=list(type=thisPageData$geo),mean)
choro$value<-tmp$x
# library(rgdal)
#load the map
my_spdf <- readOGR( 
  dsn= "C:/Users/eziod/Pictures/S7/DataVisualisation/TD3 lab3 4/q2/choropleth/TM_WORLD_BORDERS_SIMPL-0.3.shp",
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)
europeMap <- my_spdf[my_spdf@data$REGION==150&my_spdf@data$UN!=643, ]
# plot(europeMap)

colorMat<-data.frame(choro$geo,choro$value)
colnames(colorMat)<-c("geo","value")
colorMat[which(colorMat$geo=="EL"),1]<-"GR"
colorMat[which(colorMat$geo=="UK"),1]<-"GB"

frameMat<-matrix(nrow=dim(europeMap@data)[1],ncol=2)
frameMat[,1]<-europeMap@data$ISO2

# find which countries in colorMat havent been used
compare<-rep(NA,2)
for (i in 1:dim(frameMat)[1]){
  for (j in 1:dim(colorMat)[1]){
    if (colorMat[j,1]==frameMat[i,1]){
      frameMat[i,2]<-colorMat[j,2]
      compare[i]<-colorMat[j,1]
    }
  }
}
compare<-na.exclude(compare)
compare<-compare[1:31]
setdiff(union(compare, colorMat$geo), intersect(compare, colorMat$geo))
rm(i)
rm(j)



frameMat<-data.frame(frameMat)
colnames(frameMat)<-c("geo","value")
summary(is.na(frameMat[,2])) #find how many value in colorMat
# is put in frameMat 
# only 30. The result is weird

#make color
library(RColorBrewer)
my_colors <- brewer.pal(11, "Spectral")
my_colors <- colorRampPalette(my_colors)(70)
#data to give color
# choroCorrection <- read_csv("C:/Users/eziod/Pictures/S7/DataVisualisation/Project/choro/choroCorrectioncsv.csv")
class_of_country <- cut(as.numeric(frameMat$value), 70)
my_colors <- my_colors[as.numeric(class_of_country)]

# Make the plot
par(mar=c(0,0,0.8,0) )
plot(europeMap , col=my_colors ,  bg = "#A6CAE0",
     main="Europe immigration average value from 2011 to 2019") #normally it's correct

# plot a colorbar.... Tous nos colorbars sont fait à la maison !
my_colors2<-unique(sort(my_colors))
tmp2<-my_colors2[1]
my_colors2[1:length(my_colors2)-1]<-my_colors2[2:length(my_colors2)]
my_colors2[length(my_colors2)]<-tmp2
my_colors3<-data.frame(c(seq(0.1,5.5,5.4/10),10),1:length(my_colors2),rep(1,length(my_colors2)))
colnames(my_colors3)<-c("xaxis","xforggplot","one")
ggplot(my_colors3,aes(x=xforggplot,y=one))+
  geom_bar(stat="identity",fill=my_colors2,width=1)+
  theme_minimal()+
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid = element_blank(),
  )+
  scale_x_continuous("Homemade colorbar",breaks=my_colors3$xforggplot,labels=my_colors3$xaxis)






# anime_barcir ------------------------------------------------------------------------------------------
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






## anime_line --------------------------------------------------------------------------------------------
# get three countries, from 2011 to 2019
anime_line<-thisPageData[c(which(thisPageData$geo=="LU"),which(thisPageData$geo=="BE"),which(thisPageData$geo=="CH")),]

anime_line %>%
  ggplot(aes(x=year, y=value,group=geo,color=geo))+
  geom_line()+
  geom_point()+
  scale_color_viridis(discrete = TRUE)+
  ggtitle("Population of immigrants of BE, CH and LU")+
  theme_ipsum()+
  ylab("Percentage of total population")+
  scale_x_continuous("Year", breaks=anime_line$year, labels=anime_line$year )+
  transition_reveal(year)































