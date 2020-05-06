library(readr)
BathSoap <- read_csv("Documents/Final Exam/BathSoap.csv")
View(BathSoap)
head(BathSoap)
summary(BathSoap)
#View the data class
class(BathSoap)
#View its dimensions
dim(BathSoap)
#Look at column Names
names(BathSoap)
#Understanding the structure of the data
str(BathSoap)

# Prepare the data for analysis. Remove the missing values and remove the columns that are not useful for clustering
BathSoap1 <- BathSoap
BathSoap1 <- na.omit(BathSoap1)
BathSoap1 <- BathSoap[,-c(20:45)]
view(data)

# Standardize the variables
BathSoap1 <- scale(BathSoap1)

# Determine the number of clusters
wss <- (nrow(BathSoap1)-1)*sum(apply(BathSoap1,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(BathSoap1, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
#From the plot we determine that we can analyze 3 clusters

# Fit the model and print out the cluster means
fit <- kmeans (BathSoap1, 3) #fit the model
aggregate(BathSoap1,by=list(fit$cluster),FUN=mean) #get cluster means
BathSoap1 <- data.frame(BathSoap1, fit$cluster) #append cluster assignment

# Plotting the results
library(cluster)
clusplot(BathSoap1, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# Outlier detection with K-Means
# First, the data is partitioned into k groups by assigning them to closest cluster centers
BathSoap2 <- BathSoap1[,13:19]
kmeans.result <- kmeans(BathSoap2, centers=3)
kmeans.result$centers

# Calculate the distance between each object and its cluster center, and pick those with largest distances as outliers
kmeans.result$cluster #print our cluster IDs
centers <- kmeans.result$centers[kmeans.result$cluster, ]
distances <- sqrt (rowSums((BathSoap2 - centers)^2)) #calculate distances
outliers <- order(distances, decreasing=T) [1:5] #pick up to 5 top distances
print(outliers)
print(BathSoap2[outliers,])

# Hierarchical Clustering
# Drawing a sample of 100 records 
idx <- sample(1:dim(BathSoap1) [1], 100)
BathSoapSample <- BathSoap1 [idx,]
BathSoapSample$`PropCat 15` <- NULL

# Using Ward's method for hierarchical clustering
d<- dist(BathSoapSample, method = "euclidean") #distance matrix
fit <- hclust(d, method="ward")
plot(fit) #display dendogram
groups <- cutree(fit, k=3) #cut tree into 3 clusters
# Draw dendogram with red borders around the 3 clusters
rect.hclust(fit,k=3, border ="red")








