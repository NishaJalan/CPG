# library(readr)
# AnalysisDF_NA_Replaced <- read_csv("C:/Users/jalan/Desktop/IRI/analysis/AnalysisDF_NA Replaced.csv")
# View(AnalysisDF_NA_Replaced)
# beve.data<-AnalysisDF_NA_Replaced[,-c(40:114,116,117,157:181,182,183)] #Removing total dollars,units, beer/carb category

library(readxl)
Analysis_DF <- read_excel("C:/Users/jalan/Desktop/IRI/Analysis Folder/Analysis_DF_Cluster Solution.xlsx")
View(Analysis_DF)
#beve.data<-Analysis_DF[,-c(116,117,182,183,201,202,203)]
#beve.data<-Analysis_DF[,-c(116,117,201,202,203)]
library(dplyr)
Only_CB<-Analysis_DF %>%
  filter(BEER_CATEGORY==0 & CARBBEV_CATEGORY>0)

#Elbow method
# Check for the optimal number of clusters given the data
mydata <- Only_CB
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

set.seed(1234)
kfit <- kmeans(Only_CB[,-c(116,117,201,202,203)], 2)
stdData1 <- data.frame(cbind(Only_CB, as.factor(kfit$cluster)))
new.frame<-stdData1[,c(1,205,2:204)]
table(new.frame[,2])
table(new.frame$as.factor.kfit.cluster)

library(ggplot2)
ggplot(as.data.frame(beve.data), aes(x=stdData1[,101], y=stdData1[,100], color=as.factor(stdData1[,197]))) + 
  geom_point(size=5) + theme_bw() + 
  ggtitle('First 2 prinipal components clustering w/ kmeans') + 
  xlab("Principal Component 1") + ylab("Principal Component 2")

aggregate(BEER.DOLLARS~as.factor.kfit.cluster.,data=new.frame, FUN=mean)
aggregate(CARBBEV.TOT.DOLLARS~as.factor.kfit.cluster.,data=new.frame, FUN=mean)
aggregate(Income.Level~as.factor.kfit.cluster.,data=new.frame, FUN=mean)

setwd('C:/Users/jalan/Desktop/IRI/Analysis Folder')
write.csv(new.frame, file = paste('Only_CB.csv',sep = '\t'), row.names = T)

