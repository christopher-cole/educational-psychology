setwd("/Users/14colec/Desktop/Grad School Year 1/Semester I/EDP 646A/Final Project")
death = read.csv("NCHS_-_Leading_Causes_of_Death__United_States.csv", header=TRUE, sep=",")
death = death[,-3]
death[death$X113.Cause.Name == "Intentional self-harm (suicide) (*U03,X60-X84,Y87.0)",]
death = death[death$X113.Cause.Name == "Intentional self-harm (suicide) (*U03,X60-X84,Y87.0)",]
death
#could just choose one reshape

require(car)
require(MVN)


deathWide <- reshape(data = death,
                     v.names = "Deaths",
                     timevar = "Year",
                     sep = ".",
                     idvar = "State",
                     direction = "wide")
deathWide
deathWide[45,]
deathWide = deathWide[-45,]
dimnames(deathWide)[[1]]=as.character(deathWide$State)

deathWide
#warnings()
# adjWide <- reshape(data = death,
#                      v.names = "Age-adjusted Death Rate",
#                      timevar = "Year",
#                      sep = ".",
#                      idvar = "State",
#                      direction = "wide")
# adjWide

#make sure to do all the stuff below with the wide and not the regular death
#leave them as reshape functions or 
require(psych)
describe(deathWide)

round(describe(deathWide)$mean[21:4],3)

names(describe(deathWide))

describe(deathWide)$range

round(describe(deathWide)$range[c(21:4)],2)
#describe(death)$range[5]


i = 2016
j=1
while (i >1998 && j<19){
  eval(parse(text=print(paste("deathWide$Deaths.RS.",i," = deathWide$Deaths.",i,"/describe(deathWide)$range[c(4:21)][",j,"]",sep=""))))
  j=j+1
  i=i-1
}




#deathWide$Deaths.RS <- deathWide$Deaths/describe(deathWide)$range[c(4:21)]
#death$Age.adjusted.Death.Rate.RS <- death$Age.adjusted.Death.Rate/describe(death)$range[5]

deathWide
mvn(data=deathWide[21:39],covariance=FALSE,mvnTest="mardia",multivariatePlot="qq")
#There's some skew in our data and it's not multivariate normal



#install.packages("NbClust")
require(NbClust)

hca.fit <- NbClust(data = deathWide[,22:39],
                   distance = "euclidean", 
                   method = "ward.D",
                   index="all")

# ******************************************************************* 
#   * Among all indices:                                                
#   * 1 proposed 2 as the best number of clusters 
# * 11 proposed 3 as the best number of clusters 
# * 2 proposed 7 as the best number of clusters 
# * 1 proposed 8 as the best number of clusters 
# * 2 proposed 9 as the best number of clusters 
# * 3 proposed 12 as the best number of clusters 
# * 3 proposed 15 as the best number of clusters 
# 
# ***** Conclusion *****                            
#   
#   * According to the majority rule, the best number of clusters is  3 
# 
# 
# ******************************************************************* 


#ignore the plot
hca.fit$All.index

hca.fit$All.Critical

hca.duda <- data.frame(Duda_Critical=hca.fit$All.Critical[,"CritValue_Duda"],
                       Duda_Value=hca.fit$All.index[,"Duda"])
hca.duda
round(hca.duda[,1],3)
round(hca.duda[,2],3)

CHindex <- hca.fit$All.index[,"CH"]
CHindex


Cindex <- hca.fit$All.index[,"Cindex"]
Cindex

hca.fit$Best.partition

deathWide$NBhmem3 <- hca.fit$Best.partition
deathWide

d.matrix <- dist(x= deathWide[,22:39], method="euclidean")
d.matrix

hclust.fit <- hclust(d=d.matrix, method="ward.D")
hclust.fit
str(hclust.fit)

#png("dendrogram.png",height=500,width=200,res=500)
#res=500,
plot(hclust.fit,xlim=c(1,100))
#dev.off

cutree(hclust.fit, k=3)

deathWide$hmem3 <- cutree(hclust.fit, k=3)

deathWide


#K-means below

kca.fit <- NbClust(data = deathWide[,22:39],
                   distance = "euclidean", 
                   method = "kmeans",
                   index="all")

# ******************************************************************* 
#   * Among all indices:                                                
#   * 7 proposed 2 as the best number of clusters 
# * 9 proposed 3 as the best number of clusters 
# * 2 proposed 5 as the best number of clusters 
# * 3 proposed 10 as the best number of clusters 
# * 1 proposed 14 as the best number of clusters 
# * 2 proposed 15 as the best number of clusters 
# 
# ***** Conclusion *****                            
#   
#   * According to the majority rule, the best number of clusters is  3 
# 
# 
# ******************************************************************* 


kca.fit$All.index
kca.fit$All.Critical

#kca.duda <- data.frame(Duda_Critical=kca.fit$All.Critical[,"CritValue_Duda"],
#                    Duda_Value=kca.fit$All.index[,"Duda"])
#kca.duda

kCHindex <- kca.fit$All.index[,"CH"]
kCHindex

kCindex <- kca.fit$All.index[,"Cindex"]
kCindex

kca.fit$Best.partition


deathWide$NBkmem3 <- kca.fit$Best.partition
deathWide


x=22:39
eval(parse(text=print(paste('c1mean = c(',paste0("mean(deathWide[deathWide$hmem3==1,",x,"])", collapse = ', '),')'))))
eval(parse(text=print(paste('c2mean = c(',paste0("mean(deathWide[deathWide$hmem3==2,",x,"])", collapse = ', '),')'))))
eval(parse(text=print(paste('c3mean = c(',paste0("mean(deathWide[deathWide$hmem3==3,",x,"])", collapse = ', '),')'))))

#"c1mean = c( mean(deathWide[deathWide$hmem3==1,22]), mean(deathWide[deathWide$hmem3==1,23]), mean(deathWide[deathWide$hmem3==1,24]), mean(deathWide[deathWide$hmem3==1,25]), mean(deathWide[deathWide$hmem3==1,26]), mean(deathWide[deathWide$hmem3==1,27]), mean(deathWide[deathWide$hmem3==1,28]), mean(deathWide[deathWide$hmem3==1,29]), mean(deathWide[deathWide$hmem3==1,30]), mean(deathWide[deathWide$hmem3==1,31]), mean(deathWide[deathWide$hmem3==1,32]), mean(deathWide[deathWide$hmem3==1,33]), mean(deathWide[deathWide$hmem3==1,34]), mean(deathWide[deathWide$hmem3==1,35]), mean(deathWide[deathWide$hmem3==1,36]), mean(deathWide[deathWide$hmem3==1,37]), mean(deathWide[deathWide$hmem3==1,38]), mean(deathWide[deathWide$hmem3==1,39]) )"


HCAcentroids <- rbind(c1mean, c2mean, c3mean)
HCAcentroids

#plot cluster centroids!


kmeans.fit <- kmeans(x=deathWide[,22:39], 
                     centers=HCAcentroids)
kmeans.fit

deathWide$kmem3 <- kmeans.fit$cluster
deathWide

str(kmeans.fit)


#boxplot(cbind(deathWide[,4:21])~as.factor(deathWide$kmem3))
#just do regular ones
#plot(centroid1,y=1:17,type="l");lines(x=centroid2,y=1:17,lty=)
deathWide$State[deathWide$NBkmem3==1]
deathWide$State[deathWide$NBkmem3==2]
deathWide$State[deathWide$NBkmem3==3]
a = 1:3
b = 1:15
c = 1:33
eval(parse(text=print(paste('cluster1 = c(',paste("paste(deathWide$State[deathWide$NBkmem3==1][",a,"])", collapse = ', '),')'))))
cat(cluster1[a], sep = ", ")
eval(parse(text=print(paste('cluster2 = c(',paste("paste(deathWide$State[deathWide$NBkmem3==2][",b,"])", collapse = ', '),')'))))
cat(cluster2[b], sep = ", ")
eval(parse(text=print(paste('cluster3 = c(',paste("paste(deathWide$State[deathWide$NBkmem3==3][",c,"])", collapse = ', '),')'))))
cat(cluster3[c], sep = ", ")






pdf("KmeansBoxplot.pdf")
par(mfrow=c(2,3))  #2x3 grid of plots
boxplot(deathWide$Deaths.1999 ~ as.factor(deathWide$kmem3),
        main="Raw Deaths.1999 by Cluster",
        xlab="Cluster",
        ylab="Deaths.1999",
        col="gold")
boxplot(deathWide$Deaths.2000 ~ as.factor(deathWide$kmem3),
        main="Raw Deaths.2000 by Cluster",
        xlab="Cluster",
        ylab="Deaths.2000",
        col="seashell3")
boxplot(deathWide$Deaths.2001 ~ as.factor(deathWide$kmem3),
        main="Raw Deaths.2001 by Cluster",
        xlab="Cluster",
        ylab="Deaths.2001",
        col="chartreuse4")
boxplot(deathWide$Deaths.2002 ~ as.factor(deathWide$kmem3),
        main="Raw Deaths.2002 by Cluster",
        xlab="Cluster",
        ylab="Deaths.2002",
        col="gold2")
boxplot(deathWide$Deaths.2003 ~ as.factor(deathWide$kmem3),
        main="Raw Deaths.2003 by Cluster",
        xlab="Cluster",
        ylab="Deaths.2003",
        col="gray69")
boxplot(deathWide$Deaths.2004 ~ as.factor(deathWide$kmem3),
        main="Raw Deaths.2004 by Cluster",
        xlab="Cluster",
        ylab="Deaths.2004",
        col="springgreen")
boxplot(deathWide$Deaths.2005 ~ as.factor(deathWide$kmem3),
        main="Raw Deaths.2005 by Cluster",
        xlab="Cluster",
        ylab="Deaths.2005",
        col="slateblue")
boxplot(deathWide$Deaths.2006 ~ as.factor(deathWide$kmem3),
        main="Raw Deaths.2006 by Cluster",
        xlab="Cluster",
        ylab="Deaths.2006",
        col="snow3")
boxplot(deathWide$Deaths.2007 ~ as.factor(deathWide$kmem3),
        main="Raw Deaths.2007 by Cluster",
        xlab="Cluster",
        ylab="Deaths.2007",
        col="deepskyblue")
boxplot(deathWide$Deaths.2008 ~ as.factor(deathWide$kmem3),
        main="Raw Deaths.2008 by Cluster",
        xlab="Cluster",
        ylab="Deaths.2008",
        col="orangered2")
boxplot(deathWide$Deaths.2009 ~ as.factor(deathWide$kmem3),
        main="Raw Deaths.2009 by Cluster",
        xlab="Cluster",
        ylab="Deaths.2009",
        col="lightblue2")
boxplot(deathWide$Deaths.2010 ~ as.factor(deathWide$kmem3),
        main="Raw Deaths.2010 by Cluster",
        xlab="Cluster",
        ylab="Deaths.2010",
        col="deeppink")
boxplot(deathWide$Deaths.2011 ~ as.factor(deathWide$kmem3),
        main="Raw Deaths.2011 by Cluster",
        xlab="Cluster",
        ylab="Deaths.2011",
        col="orangered")
boxplot(deathWide$Deaths.2012 ~ as.factor(deathWide$kmem3),
        main="Raw Deaths.2012 by Cluster",
        xlab="Cluster",
        ylab="Deaths.2012",
        col="dodgerblue")
boxplot(deathWide$Deaths.2013 ~ as.factor(deathWide$kmem3),
        main="Raw Deaths.2013 by Cluster",
        xlab="Cluster",
        ylab="Deaths.2013",
        col="lightskyblue")
boxplot(deathWide$Deaths.2014 ~ as.factor(deathWide$kmem3),
        main="Raw Deaths.2014 by Cluster",
        xlab="Cluster",
        ylab="Deaths.2014",
        col="darkgray")
boxplot(deathWide$Deaths.2015 ~ as.factor(deathWide$kmem3),
        main="Raw Deaths.2015 by Cluster",
        xlab="Cluster",
        ylab="Deaths.2015",
        col="lightblue")
boxplot(deathWide$Deaths.2016 ~ as.factor(deathWide$kmem3),
        main="Raw Deaths.2016 by Cluster",
        xlab="Cluster",
        ylab="Deaths.2016",
        col="green")
dev.off()

#install.packages("ggplot2")
# require(ggplot2)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.RS.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.3)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.6)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.RS.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.3)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.6)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.RS.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.3)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.6)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.RS.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.3)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.6)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.RS.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.3)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.6)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.RS.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.3)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.6)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.RS.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.3)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.6)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.RS.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.3)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.6)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.RS.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.3)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.6)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.RS.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.3)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.6)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.RS.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.3)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.6)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.RS.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.3)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.6)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.RS.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.3)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.6)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.RS.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.3)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.6)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.RS.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.3)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.6)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.RS.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.3)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.6)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.RS.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.3)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.6)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.RS.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.3)
# 
# ggplot(deathWide, 
#        aes(x=Deaths.2016, fill=as.factor(kmem3))) + 
#   geom_density(alpha=.6)








