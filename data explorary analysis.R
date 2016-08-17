### Marketing Project - Data Explorary Analysis

library(caret)
library(car)
library(cluster)
library(fpc)

plotClust = function(km,discPlot=FALSE){
      nc = length(km$size)
      if(discPlot){par(mfrow=c(2,2))}
      else {par(mfrow=c(3,1))}
      percsize = paste(1:nc," = ",format(km$size/sum(km$size)*100,digits=2),"%",sep="")
      pie(km$size,labels=percsize,col=1:nc)
      
      clusplot(toclust, km$cluster, color=TRUE, shade=TRUE,
               labels=2, lines=0,col.clus=1:nc); #plot clusters against principal components
      
      if(discPlot){
            plotcluster(toclust, km$cluster,col=km$cluster); #plot against discriminant functions ()
      }
      rng = range(km$centers)
      dist = rng[2]-rng[1]
      locs = km$centers+.05*dist*ifelse(km$centers>0,1,-1)
      bm = barplot(km$centers,beside=TRUE,col=1:nc,main="Cluster Means",ylim=rng+dist*c(-.1,.1))
      text(bm,locs,formatC(km$centers,format="f",digits=1))
}

## 1.load data and create a combined sample
data.customer <- read.csv(file = 'Customer Database - Auto Membership_CEXAppend.csv')
data.customer[is.na(data.customer)] = 0
data.rd <- read.csv(file = 'Random Database Sample - Auto Membership_CEXAppend.csv')
data.rd[is.na(data.rd)] = 0
rdSample = sample(1:80000,20000)
data.all <- rbind(data.customer, data.rd[rdSample,])

## 2.convert categorical variables into numerical variables
data.customer$gender = recode(data.customer$gender, "'M'=1; 'F'=0; ''=0")
data.customer$gender = as.numeric(as.character(data.customer$gender))
data.customer$marital_status = recode(data.customer$marital_status, "'M'=1; 'S'=0; ''=0")
data.customer$marital_status = as.numeric(as.character(data.customer$marital_status))
data.customer$income = recode(data.customer$income, "''=1; 'A'=1; 'B'=2; 'C'=3; 'D'=4; 'E'=5; 'F'=6; 'G'=7;
                         'H'=8; 'I'=9; 'J'=10; 'K'=11; 'L'=12; 'M'=13; 'N'=14; 'O'=15")
data.customer$income = as.numeric(as.character(data.customer$income))
data.customer$great_outdoors = as.numeric(as.character(data.customer$great_outdoors))
data.customer$sporting_life = as.numeric(as.character(data.customer$sporting_life))
data.customer$health_fitness = as.numeric(as.character(data.customer$health_fitness))
data.customer$luxury_life = as.numeric(as.character(data.customer$luxury_life))
data.customer$doit_yourselfer = as.numeric(as.character(data.customer$doit_yourselfer))



## 3.pick feature
data.picked = data.customer[,c('gender', 'marital_status', 'income', 'great_outdoors', 'sporting_life',
                               'health_fitness', 'luxury_life', 'doit_yourselfer', 'num_in_hhld',
                               'num_of_adults', 'num_of_children', 'children', 'truck_owner', 'motor_cycle',
                               'sports', 'entertainment_enth', 'hobbyists', 'avid_readers', 'collectors',
                               'travel', 'pets', 'music', 'toys', 'art_craft', 'gardening', 'family', 'food',
                               'cars')]
data.picked = data.customer[,c('marital_status', 'income', 'num_in_hhld', 'luxury_life', 'num_of_children')]
data.picked = data.customer[,c('marital_status', 'income', 'num_in_hhld', 'luxury_life', 'family', 'num_of_children')]

toclust = as.matrix(data.picked)

# 3
km.3 = kmeans(toclust,3,iter.max = 20, nstart=2)
paste(1:3," = ",format(km.3$size/sum(km.3$size)*100,digits=2),"%",sep="")
km.3$centers
clusplot(toclust, km.3$cluster, color=TRUE, plotchar = F, labels = 0, shade=F, lines=0)

# 4
km.4 = kmeans(toclust,4,iter.max = 20, nstart=2)
paste(1:4," = ",format(km.4$size/sum(km.4$size)*100,digits=2),"%",sep="")
km.4$centers
clusplot(toclust, km.4$cluster, color=TRUE, plotchar = F, labels = 0, shade=F, lines=0)

# 5
km.5 = kmeans(toclust,5,iter.max = 20, nstart=2)
paste(1:5," = ",format(km.5$size/sum(km.5$size)*100,digits=2),"%",sep="")
km.5$centers
clusplot(toclust, km.5$cluster, color=TRUE, plotchar = F, labels = 0, shade=F, lines=0)


segment4 = which(km.4$cluster == 1)
segment4 = data.customer[segment4,]
segment3 = which(km.4$cluster == 4)
segment3 = data.customer[segment3,]
segment2 = which(km.4$cluster == 2)
segment2 = data.customer[segment2,]
segment1 = which(km.4$cluster == 3)
segment1 = data.customer[segment1,]
compare = data.frame(segment1 = sapply(segment1, mean),
                      segment2 = sapply(segment2, mean),
                      segment3 = sapply(segment3, mean),
                      segment4 = sapply(segment4, mean), 
                      random = sapply(data.rd, mean))
save(compare, file = 'compare.Rda')
write.csv(compare, file = 'compare.csv')

## 2.convert categorical variables into numerical variables
data.all$gender = recode(data.all$gender, "'M'=1; 'F'=0; ''=0")
data.all$gender = as.numeric(as.character(data.all$gender))
data.all$marital_status = recode(data.all$marital_status, "'M'=1; 'S'=0; ''=0")
data.all$marital_status = as.numeric(as.character(data.all$marital_status))
data.all$income = recode(data.all$income, "''=1; 'A'=1; 'B'=2; 'C'=3; 'D'=4; 'E'=5; 'F'=6; 'G'=7;
                         'H'=8; 'I'=9; 'J'=10; 'K'=11; 'L'=12; 'M'=13; 'N'=14; 'O'=15")
data.all$income = as.numeric(as.character(data.all$income))
