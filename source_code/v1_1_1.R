#first try, use the excel thisPageData
## load library
library(readr)
library(ggplot2)
library(gganimate)
library(rgdal)
library(RColorBrewer)
library(dplyr)
library(circlize)
library(tidyverse)
library(caTools)
thisPageData <- read_csv("C:/Users/eziod/Pictures/S7/DataVisualisation/Project/thisPageData.csv")
names(thisPageData)[9]<-"year"
names(thisPageData)[10]<-"value"










## lineplot luxembourg
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








# !!!!!! pie chart is not ok for this kind of values
## pie chart luxembourg from 2011 to 2019
# sample data frame
# year<-2011:2019
# temp<-thisPageData$geo=="LU"
# population<-as.vector(unlist(thisPageData[temp,10]))
# luxembourg<-data.frame(year,population)
# 
# # basic pie chart
# pie<-ggplot(luxembourg,aes(x="",y=population,fill=population))+
#   geom_bar(stat="identity",width=1)+
#   coord_polar("y",start=0)
# pie

# chord diagram
# chord diagram
# chord diagram
# chord diagram
# chord diagram
# chord diagram
# chord diagram
# chord diagram
# chord diagram
# chord diagram
## chord diagram
library(circlize)
thisPageData <- read_csv("C:/Users/eziod/Pictures/S7/DataVisualisation/Project/thisPageData.csv")
names(thisPageData)[9]<-"year"
names(thisPageData)[10]<-"value"
#take four western countries from 2011 to 2019
chord<-thisPageData[thisPageData$geo=="FR"|thisPageData$geo=="DE"|thisPageData$geo=="CZ"|thisPageData$geo=="ES"|thisPageData$geo=="IT"|thisPageData$geo=="EL",]
chord<-data.frame(chord$geo,chord$year,chord$value)
colnames(chord)<-c("geo","year","value")

# I plot the frame and section
#step I.1 initialize the chart giving factor and x-axis
circos.initialize( factors=chord$geo, x=chord$year )
#step I.2 build the regions
circos.trackPlotRegion(factors = chord$geo, y = chord$value, panel.fun = function(geo, value) {
  circos.axis()
})
#step I.3 add points
circos.trackPoints(chord$geo, chord$year, chord$value, col = "blue", pch = 16, cex = 0.5) 

# II customize the chart
# step II.0 General Customization:
par(
  mar = c(1, 1, 1, 1),           # Margin around chart
  bg = rgb(0.4,0.1,0.7,0.05)     # background color
) 
circos.par("track.height" = 0.6) # track hight, 0.6 = 60% of total height
circos.trackPoints(chord$geo, chord$year, chord$value, col = "blue", pch = 16, cex = 0.5) 
# Step II.2: Build regions
circos.trackPlotRegion(sectors = chord$geo,x=chord$year, y = chord$value, panel.fun = function(year, value) {
  circos.axis(
    h="top",                   # x axis on the inner or outer part of the track?
    labels=TRUE,               # show the labels of the axis?
    major.tick=FALSE           # show ticks?
)
})






# circular barplot
# circular barplot
# circular barplot
# circular barplot
# circular barplot
# circular barplot
# circular barplot
# circular barplot
# circular barplot
# circular barplot
# circular barplot
library(tidyverse)
# prepare the data frame ------------------------------------------------------------------
barcir<-thisPageData[thisPageData$year==2019,]
barcir<-data.frame(barcir$geo,barcir$year,barcir$value)
colnames(barcir)<-c("geo","year","value")
# enlever les noms cheloux, pour ne rester que des pays individuels
tmp<-c("EA19","EU15","EU27_2020","EU28")
for (i in tmp){
  barcir<-barcir[-which(barcir==i),]
}
rm(tmp)
rm(i)
barcir$id<-1:nrow(barcir)
# as R recommands, make geo, value as single column
geo<-barcir$geo
year<-barcir$year
value<-barcir$value

# basic plot -------------------------------------------------------------------------------------------------------------
# p_barcir<-ggplot(barcir,aes(x=as.factor(id),y=value))+ # Note that x is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(stat="identity",fill=alpha("red",0.3))+
#   ylim(-2,20)+ 
#   theme_minimal() +
#   theme(
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     panel.grid = element_blank(),
#     plot.margin = unit(rep(-2,4), "cm")     # This remove unnecessary margin around plot
#   )+
#   coord_polar(start = 0)
# p_barcir


# add label by geom_text()-------------------------------------------------------------------------------------------------
label_barcir<-barcir
# calculate the ANGLE of the labels
number_of_bar <- nrow(label_barcir)
angle <-  90 - 360 * (1:nrow(label_barcir)-0.5) /number_of_bar     # the original author substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_barcir$hjust<-ifelse( angle < -90, 1, 0) # ifelse(test, yes, no)
# flip angle BY to make them readable
label_barcir$angle<-ifelse(angle < -90, angle+180, angle)
# ----- ------------------------------------------- ---- #


# Start the plot
p_barcir<-ggplot(barcir,aes(x=as.factor(geo),y=value))+ # Note that x is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity",fill=alpha("red",0.3))+
  ylim(-2,20)+ 
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm")     # This remove unnecessary margin around plot
  )+
  coord_polar(start = 0)+  #This makes the coordinate polar instead of cartesian.
  # Add the labels, using the label_barcir dataframe that we have created before
  geom_text(data=label_barcir, aes(x=geo, y=value+0.5,label=geo, hjust=hjust), color="black", fontface="bold",alpha=0.4, size=2.5, angle= label_barcir$angle, inherit.aes = FALSE ) 

p_barcir




# group and space between groups ---------------------------------
# preparation of dataframe
thisPageData <- read_csv("C:/Users/eziod/Pictures/S7/DataVisualisation/Project/thisPageData.csv")
names(thisPageData)[9]<-"year"
names(thisPageData)[10]<-"value"
barcir<-thisPageData[thisPageData$year==2019,]
barcir<-data.frame(barcir$geo,barcir$year,barcir$value,barcir$full_name,barcir$region)
colnames(barcir)<-c("geo","year","value","full_name","region")

# # as R recommands, make geo, value as single column
# geo<-barcir$geo
# year<-barcir$year
# value<-barcir$value
# full_name<-barcir$full_name
# region<-barcir$region


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

d






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
emptyBar<-3
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
  summarize(start=min(id2), end=max(id2) - emptyBar) %>% 
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
  geom_text(data=base_data, aes(x = title, y = -1.5, label=region), hjust=c(1,1,1,0,0), colour = "black", alpha=0.8, size=2, fontface="bold", inherit.aes = FALSE)
  
#   # gganimate staff
#   transition_states(
#     year,
#     transition_length=2,
#     state_length = 1
#   )+
#   ease_aes('sine-in-out')
# 
# animate(p_barcir, duration=5, fps=20, width=500, height=500, renderer=gifski_renderer())

p_barcir




barcir$year <- as.numeric(barcir$year)
testt<-ggplot(barcir, aes(x = as.factor( id2  ),y=value, fill=region))+
  geom_bar(stat="identity",alpha=0.5)+coord_polar() + ylim(-7,10)+
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")
)+
  # gganimate staff
  transition_time(
    year,
    transition_length=2,
    state_length = 1
  )+
  ease_aes('sine-in-out')

animate(testt, duration=5, fps=20, width=500, height=500, renderer=gifski_renderer())


testt











## choropleth map of 2019
#create a dataframe of just 2019
choro2019<-thisPageData[thisPageData$year==2019,]
library(rgdal)
my_spdf <- readOGR( 
  dsn= "C:/Users/eziod/Pictures/S7/DataVisualisation/TD3 lab3 4/q2/choropleth/TM_WORLD_BORDERS_SIMPL-0.3.shp",
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)
#to inspect the part data of my_spdf
# tmp<-data.frame(my_spdf@data)
#select europe only
europeMap <- my_spdf[my_spdf@data$REGION==150&my_spdf@data$UN!=643, ]
plot(europeMap)
#make color
library(RColorBrewer)
my_colors <- brewer.pal(5, "Spectral") 
my_colors <- colorRampPalette(my_colors)(38)
#data to give color
choro2019Correction <- read_csv("C:/Users/eziod/Pictures/S7/DataVisualisation/Project/choro/choro2019Correctioncsv.csv")
class_of_country <- cut(choro2019Correction$value, 38)
my_colors <- my_colors[as.numeric(class_of_country)]

# Make the plot
plot(europeMap , col=my_colors ,  bg = "#A6CAE0",
     main="europe immigration 2019") #normally it's correct

View(europeMap@data)
dim(europeMap@data)










## choropleth tentative 2 ---------------
#as usual, load the data
thisPageData <- read_csv("C:/Users/eziod/Pictures/S7/DataVisualisation/Project/thisPageData.csv")
names(thisPageData)[9]<-"year"
names(thisPageData)[10]<-"value"
choro2019<-thisPageData[thisPageData$year==2019,]
library(rgdal)
#load the map
my_spdf <- readOGR( 
  dsn= "C:/Users/eziod/Pictures/S7/DataVisualisation/TD3 lab3 4/q2/choropleth/TM_WORLD_BORDERS_SIMPL-0.3.shp",
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)
europeMap <- my_spdf[my_spdf@data$REGION==150&my_spdf@data$UN!=643, ]
plot(europeMap)

colorMat<-data.frame(choro2019$geo,choro2019$value)
colnames(colorMat)<-c("geo","value")
frameMat<-matrix(nrow=dim(europeMap@data)[1],ncol=2)
frameMat[,1]<-europeMap@data$ISO2


for (i in 1:dim(frameMat)[1]){
  for (j in 1:dim(colorMat)[1]){
    if (colorMat[j,1]==frameMat[i,1]){
       frameMat[i,2]<-colorMat[j,2]
    }
  }
}
rm(i)
rm(j)
frameMat<-data.frame(frameMat)
colnames(frameMat)<-c("geo","value")
summary(is.na(frameMat[,2])) #find how many value in colorMat
# is put in frameMat 
# only 23. The result is weird

#make color
library(RColorBrewer)
my_colors <- brewer.pal(5, "Spectral") 
my_colors <- colorRampPalette(my_colors)(38)
#data to give color
# choro2019Correction <- read_csv("C:/Users/eziod/Pictures/S7/DataVisualisation/Project/choro/choro2019Correctioncsv.csv")
class_of_country <- cut(as.numeric(frameMat$value), 38)
my_colors <- my_colors[as.numeric(class_of_country)]

# Make the plot
plot(europeMap , col=my_colors ,  bg = "#A6CAE0",
     main="europe immigration 2019") #normally it's correct













##Test --------------
write.csv(europeMap@data,file="C:/Users/eziod/Pictures/S7/DataVisualisation/Project/europeMap.csv",quote=F,row.names = T)
write.csv(choro2019,file="C:/Users/eziod/Pictures/S7/DataVisualisation/Project/choro/choro2019.csv",quote=F,row.names = T)
europeMap@data<-arrange(europeMap@data, NAME) #this does not work

df <- data.frame(2:11, 1:10, c("B","A","G","S","O","X","P","A","K","J"))
colnames(df) <- c("ID", "string", "delta")



