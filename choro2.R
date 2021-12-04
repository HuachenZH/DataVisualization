## load library
library(readr)
library(ggplot2)
library(gganimate)
library(rgdal)
library(RColorBrewer)
library(dplyr)
library(circlize)
library(tidyverse)
thisPageData <- read_csv("C:/Users/eziod/Pictures/S7/DataVisualisation/Project/thisPageData.csv")
names(thisPageData)[9]<-"year"
names(thisPageData)[10]<-"value"



## choropleth tentative 2 ------------------------------------------------------
#as usual, load the data
choro<-thisPageData[thisPageData$year==2011,]
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







# read gif
read.gif("gif.gif")






