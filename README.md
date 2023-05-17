# The-Chernobyl-Nuclear-Incident
# Data Cleaning

library(ggplot2)
first=read.csv(file.choose())

#understandthedata
head(first)
tail(first)
summary(first)
#understanding is.na()
is.na(first)
sum(is.na(first))
which(is.na(first))
summary(first)
str(first)
#boxplot
boxplot(first$Longitude)
boxplot(first$Latitude)

summary(first)

# KMeans Clustering
install.packages("ClusterR")
install.packages("cluster")
install.packages("caret")



#Loading libraries
library("ggplot2")
library("cluster")
library("caret")

#Reading the Data
df <- first
head(df)


firstCluster <- kmeans(first$Latitude, center=3, nstart=20)
firstCluster$cluster
cm<-table(first$Longitude,firstCluster$cluster)
cm

clusplot(first, firstCluster$cluster, color=T, shade=T, labels=0, lines=0)


confusionMatrix(cm)

library(pROC)
roc_score=roc(first$Longitude,firstCluster$cluster)
plot(roc_score,main="ROC Curve")


# PCA Algorithm

my_pca <- prcomp(first$Latitude, scale = TRUE,r
                 center = TRUE, retx = T)
names(my_pca)


summary(my_pca)
my_pca


my_pca$rotation


dim(my_pca$x)
my_pca$x


biplot(my_pca, main = "Biplot", ylim = c(0, 1))

my_pca$sdev

my_pca.var <- my_pca$sdev ^ 2
my_pca.var



propve <- my_pca.var / sum(my_pca.var)
propve

plot(propve, xlab = "principal component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b",
     main = "Scree Plot")


plot(cumsum(propve),
     xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")
     



