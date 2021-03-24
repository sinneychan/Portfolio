pdf("pamKPlots.pdf")

# install.packages(fpc)
# install.packages("caret")
# install.packages("EMT")
# install.packages("cluster")
# install.packages
# install.packages("car")
library(fpc)
library(kernlab)
library(caret)
library(EMT)
library(cluster)
library(e1071)
library(car)
library(corrplot)

# dataset is made by Lisu, contains information about population ofover 200 countries, their growth rate,
# pop density, and the percentage of their population that is college educated.

country.data <- read.csv(file="CountryDatav4.1.csv", header = TRUE, sep=",",na.strings=c(" ",""),colClasses=c("numeric","character",
                        "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                        "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                        "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))

##############################################################################
# Decided to try to scale the population to see if it would change the       # 
# clustering.                                                                #
##############################################################################

####country.data <- transform(country.data, pop17scaled = scale(country.data$pop17))

country.data <- transform(country.data, 
                          H1B1415 = (country.data$H.1B.count14/pop17),
                          H1B1516 = (country.data$H.1B.count15/pop17),
                          H1B1617 = (country.data$H.1B.count16/pop17),
                           intlstu1415 = (country.data$intl.students.14.15/pop17),
                           intlstu1516 = (country.data$intl.students.15.16/pop17),
                           intlstu1617 = (country.data$intl.students.16.17/pop17),
                           LPR14 = (country.data$LPRTotal14/pop17),
                           LPREmp14 = (country.data$EmpBased14/LPRTotal14),               # This will give the % of all LPR's from that country
                           LPR15 = (country.data$LPRTotal15/pop17),
                           LPREmp15 = (country.data$EmpBased15/LPRTotal15),               # This will give the % of all LPR's from that country  
                           LPR16 = (country.data$LPRTotal16/pop17),
                           LPREmp16 = (country.data$EmpBased16/LPRTotal16))               # This will give the % of all LPR's from that country
####country.data <- transform(country.data, pop17scaled = scale(country.data$pop17))

# make a new dataset by dropping some of the columns you don't want.

myvars <- names(country.data) %in% c("Rank","rate.of.change","growthrate","pop17","H1.B.count","rate.of.change","medage18",
                                     "intl.students.14.15","intl.students.15.16","intl.students.16.17",
                                    "H.1B.count15","H.1B.count16","H.1B.count14",
                                     "LPRTotal14","LPRTotal15","LPRTotal16","EmpBased14","EmpBased15","EmpBased16",
                                     "RankPublished","Citations.per.document",
                                     "gdppercapitaPPP","Citations",
                                     "popdensity18","Documents","Citable.documents","Self.citations")
newdata <- country.data[!myvars]

# we need to remove the rows where any of the fields are blank

newdataX <- na.omit(newdata)

newdata1 <- scale(newdataX[,2:15])


########################
# added 101618
checkcorr <- na.omit(country.data)
corrplot.mixed(cor(checkcorr[,3:28]), upper = "ellipse", order="original",lower.col = "black", number.cex = .7, tl.col="black",  tl.cex = .7)
corrplot.mixed(cor(checkcorr[,15:40]), upper = "ellipse", order="original",lower.col = "black", number.cex = .7, tl.col="black",  tl.cex = .7)

######################
# correlation matrix #
######################

cor(newdata1)

## Fancy correlation plot

#library(corrplot)

# corrplot(cor(newdata1), type = "upper", order="hclust", tl.col="black", tl.srt = 45)
# corrplot.mixed(cor(newdata1), order="hclust", tl.col="black", tl.srt = 45)

#68 countries 2v9.2 - plot 1
corrplot.mixed(cor(newdata1), upper = "ellipse", order="original",lower.col = "black", number.cex = .7, tl.col="black",  tl.cex = .7)

#edpop <- newdata1[,1:14]


#library(kernlab)
#install.packages(kernlab)
#library(kernlab)
#(no country name here)
topdata <- as.matrix(newdata1[,1:14],header=T)

pamk.result <- pamk(topdata)

# how many clusters?
pamk.result$nc

table(pamk.result$pamobject$clustering, newdataX[,1])   # THIS GIVES YOU THE RESULTS BY COUNTRY NAME.

#68 countries 2v9.2 - plot 2
plot(pamk.result$pamobject,main="Cluster by k-medoid method")


layout(matrix(1))

library(cluster)

#install.packages("factoextra")                 
library(factoextra)
# plot 4 optimal number of clusters
fviz_nbclust(topdata, pam, method = "silhouette")+
  theme_classic()

# now try pam with k=2 AGAIN!
pam.result <- pam(topdata, 5)
print(pam.result)
table( newdataX[,1], pam.result$clustering)

#plot 5 - cluster plot
fviz_cluster(pam.result, newdataX[,-1],geom = c("point","text"),
             #palette = c("#00AFBB", "#FC4E07","#D0BFAA", "#B0BFAA","00AB0C7"), # color palette
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             pointsize = 1.5, labelsize = 10,
             ggtheme = theme_classic()
)

# add cluster assignment to dataset
newdataX <- transform(newdataX, cluster = pam.result$clustering)

#try a scatterplot
newdataX$cluster <- as.factor(newdataX$cluster)
plot(newdataX$intlstu1415 ~ newdataX$cluster )

#plot 6 - tertiary ed level
plot(newdataX$educpct15 ~ newdataX$cluster, data =newdataX,
     main="Tertiary Education Level for Each Cluster",
     xlab="Cluster Number",
     ylab="% of Country's population w/Bachelors or >",log="y")   # interesting?

#plot 7 - H-index for each cluster
plot(newdataX$H.index ~ newdataX$cluster, data = newdataX,
     main="H-Index For Each Cluster",
     xlab="Cluster Number",
     ylab="H-Index (Research Quality/Quantity)",log="y")  # interesting?

print(newdataX$cluster)
cluster1 <- newdataX[newdataX$cluster == '1',]
cluster2 <- newdataX[newdataX$cluster == '2',]
cluster3 <- newdataX[newdataX$cluster == '3',]
cluster3a <- newdataX[newdataX$cluster == '3',c("Country","LPR15","intlstu1516")]
cluster4 <- newdataX[newdataX$cluster == '4',]
cluster5 <- newdataX[newdataX$cluster == '5',]

#realdata1 <- country.data[cluster1$Country,]

cluster1$Country

cluster2$Country

cluster3$Country

cluster4$Country

cluster5$Country

cor(cluster1[,2:15])
#corrplot(cor(cluster1[,2:10]), type = "upper", order="hclust", tl.col="black", tl.srt = 45)
#corrplot.mixed(cor(cluster1[,2:10]), order="hclust", tl.col="black", tl.srt = 45)
# easy to read!
#plot 8 cluster 1 corr matrix
corrplot.mixed(cor(cluster1[,2:15]), upper = "ellipse", lower.col = "black", number.cex = .6,tl.col="black",  tl.cex = .6)

summary(cluster1)


cor(cluster2[,2:15])
#corrplot(cor(cluster2[,2:10]), type = "upper", order="hclust", tl.col="black", tl.srt = 45)
#corrplot.mixed(cor(cluster2[,2:10]), order="hclust", tl.col="black", tl.srt = 45)
# easy to read!
#plot 9 cluster 2 corr matrix
corrplot.mixed(cor(cluster2[,2:15]), upper = "ellipse", order="original",lower.col = "black", number.cex = .6,tl.col="black",  tl.cex = .6)
summary(cluster2)

cor(cluster3[,2:15])
#corrplot(cor(cluster2[,2:10]), type = "upper", order="hclust", tl.col="black", tl.srt = 45)
#corrplot.mixed(cor(cluster2[,2:10]), order="hclust", tl.col="black", tl.srt = 45)
# easy to read!
#plot 9 cluster 2 corr matrix
corrplot.mixed(cor(cluster3[,2:15]), upper = "ellipse", order="original",lower.col = "black",number.cex = .6, tl.col="black",  tl.cex = .6)
summary(cluster3)

cor(cluster4[,2:15])
#corrplot(cor(cluster2[,2:10]), type = "upper", order="hclust", tl.col="black", tl.srt = 45)
#corrplot.mixed(cor(cluster2[,2:10]), order="hclust", tl.col="black", tl.srt = 45)
# easy to read!
#plot 9 cluster 2 corr matrix
corrplot.mixed(cor(cluster4[,2:15]), upper = "ellipse", lower.col = "black",number.cex = .7, tl.col="black",  tl.cex = .7)
summary(cluster4)

cor(cluster5[,2:15])
#corrplot(cor(cluster2[,2:10]), type = "upper", order="hclust", tl.col="black",number.cex = .7, tl.srt = 45)
#corrplot.mixed(cor(cluster2[,2:10]), order="hclust", tl.col="black", tl.srt = 45)
# easy to read!
#plot 9 cluster 2 corr matrix
corrplot.mixed(cor(cluster5[,2:15]), upper = "ellipse", lower.col = "black", tl.col="black",  tl.cex = .7)
summary(cluster5)

# 
# pairs(~LPR16+educpct15+H1B1516+intlstu1516, data=cluster1,main="Scatterplot Relationships in Cluster 1")

# library(car)
# scatterplotMatrix(~LPR16+educpct15+H1B1516+intlstu1516, data=cluster1,
#                   spread=FALSE,smoother.args=list(lty=2),main="Scatterplot Relationships in Cluster 1")
# 
# ######################################
# #  Try regression model - never mind
########################################

# clus1 <- lm(LPR16 ~ educpct15+H.index+H1B1516+intlstu1415+intlstu1516+LPR14+LPR15, data=newdataX)
# summary(clus1)
# clus1.df <- fortify(clus1)
# 

# leveragePlots(clus1,pch=16)  # didnt work
# qqPlot(clus1, pch=16)
# 
# clus1

###########################################################
### trying to look at the real data , not proportional data
###########################################################

myvars <- c("Country","cluster")

clusnum <- newdataX[myvars]

cd <- merge(clusnum,country.data, by="Country")

#29v2 plot 10 -pop by cluster

plot(pop17 ~ cluster, data = cd,
     main="Population by Cluster",
     xlab="Cluster Number",
     ylab="H-Index (Research Quality/Quantity)",log="y")   # interesting?

#29v2 plot 11 -intl students 14-15

plot(intl.students.14.15 ~ cluster, data = cd,
     main="Number of International Students 14-15 by Cluster",
     xlab="Cluster Number",
     ylab="International Student Count",log="y")           # interesting?

#29v2 plot 12 -intl students 15-16

plot(intl.students.15.16 ~ cluster, data = cd,
     main="Number of International Students 15-16 by Cluster",
     xlab="Cluster Number",
     ylab="International Student Count",log="y")                   # interesting?

#29v2 plot 12a -intl students 16-17

plot(intl.students.16.17 ~ cluster, data = cd,
     main="Number of International Students 16-17 by Cluster",
     xlab="Cluster Number",
     ylab="International Student Count",log="y")                   # interesting?

#29v2 plot 12 - H-1B's 15-16
plot(H.1B.count15 ~ cluster, data = cd,
     main="Number of H-1B's by Cluster",
     xlab="Cluster Number",
     ylab="H-1B Petition Count",log="y")                   # interesting?

#29v2 plot 13 - Emp based LPR's 14
plot(LPREmp14 ~ cluster, data = cd,
     main="Number of Employment-Based LPR's in 2014 by Cluster",
     xlab="Cluster Number",
     ylab="Employment-Based LPR's in 2014 ",log="y")          # interesting?

#29v2 plot 14 - Emp based LPR's 15
plot(LPREmp15 ~ cluster, data = cd,
     main="Number of Employment-Based LPR's in 2015 by Cluster",
     xlab="Cluster Number",
     ylab="LPR's in 2015 ",log="y")                        # interesting?

#29v2 plot 15 - Emp based LPR's 16
plot(LPREmp16 ~ cluster, data = cd,
     main="Number of Employment-Based LPR's in 2016 by Cluster",
     xlab="Cluster Number",
     ylab="LPR's in 2016 ",log="y")                        # interesting?

#29v2 plot 16 - pop density
plot(popdensity18 ~ cluster, data = cd,
     main="Population Density by Cluster",
     xlab="Cluster Number",
     ylab="Population Density in 2018 ",log="y")           # interesting?

#29v2 plot 17 - tertiary ed
plot(educpct15 ~ cluster, data = cd,
     main="% of Population with Tertiary Education by Cluster",
     xlab="Cluster Number",
     ylab="Percent ")                                 # interesting?

#29v2 plot 18 - H-index by cluster
plot(H.index ~ cluster, data = cd,
     main="H-Index by Cluster",
     xlab="Cluster Number",
     ylab="H-Index")                                 # interesting?


library(readr)

write_csv(newdataX, "newdataX.1014.csv")
write_csv(cd, "cd.1014.csv")
write_csv(cluster1, "2v9.2cluster1.1014.csv")
write_csv(cluster2, "2v9.2cluster2.1014.csv")
write_csv(cluster3, "2v9cluster3.1014.csv")
write_csv(cluster4, "2v9cluster4.1014.csv")
write_csv(cluster5, "2v9cluster5.1014.csv")
# # 
# cor(cluster5[,2:9])
# corrplot(cor(cluster5[,2:9]), type = "upper", order="hclust", tl.col="black", tl.srt = 45)
# 
# summary(cluster3)

##Learning example below

set.seed(112)
data=matrix(sample(1:30,15) , nrow=3)
colnames(data)=c("A","B","C","D","E")
rownames(data)=c("var1","var2","var3")
# Grouped barplot
barplot(data, col=colors()[c(23,89,12)] , border="white", font.axis=1, beside=T, legend=rownames(data), xlab="group", font.lab=2)

#Close pdf file ** IMPORTANT!
graphics.off()

#  Extra!
plot(LPREmp14 ~ cluster, data = cd,
     main="Plot LPREmp14",
     xlab="Cluster Number",
     ylab="LPREmp14",log="y")                                 # interesting?
plot(LPREmp15 ~ cluster, data = cd,
     main="Plot LPREmp15",
     xlab="Cluster Number",
     ylab="LPREmp15",log="y")                                 # interesting?
plot(LPREmp16 ~ cluster, data = cd,
     main="Plot LPREmp16",
     xlab="Cluster Number",
     ylab="LPREmp16",log="y")                                 # interesting?
#
#
plot(LPR14 ~ cluster, data = cd,
     main="Plot LPR14",
     xlab="Cluster Number",
     ylab="LPR14",log="y")                                 # interesting?
plot(LPR15 ~ cluster, data = cd,
     main="Plot LPR15",
     xlab="Cluster Number",
     ylab="LPR15",log="y")                                 # interesting?
plot(LPR16 ~ cluster, data = cd,
     main="Plot LPR16",
     xlab="Cluster Number",
     ylab="LPR16",log="y")                                 # interesting?
#
#
#
plot(intlstu1415 ~ cluster, data = cd,
     main="Plot intlstu1415",
     xlab="Cluster Number",
     ylab="intlstu1415",log="y")                           # interesting?
plot(intlstu1516 ~ cluster, data = cd,
     main="Plot intlstu1516",
     xlab="Cluster Number",
     ylab="intlstu1516",log="y")                           # interesting?
plot(intlstu1617 ~ cluster, data = cd,
     main="Plot intlstu1617",
     xlab="Cluster Number",
     ylab="intlstu1617",log="y")                           # interesting?
#
plot(H1B1415 ~ cluster, data = cd,
     main="Plot H1B1415",
     xlab="Cluster Number",
     ylab="H1B1415",log="y")                           # interesting?
plot(H1B1516 ~ cluster, data = cd,
     main="Plot H1B1516",
     xlab="Cluster Number",
     ylab="H1B1516",log="y")                           # interesting?
plot(H1B1617 ~ cluster, data = cd,
     main="Plot H1B1617",
     xlab="Cluster Number",
     ylab="H1B1617",log="y")                           # interesting?

