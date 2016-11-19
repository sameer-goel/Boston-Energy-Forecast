####### K-means Clustering

  csv <- read.csv("finland_allBuilding_allWeather.csv",stringsAsFactors = FALSE)
  
  require(dplyr)
  mergeData <- csv %>% 
    select(-c(X,date,
              vac,BuildingID,
              meternumb,type,Consumption,City, Wind_Direction,Conditions))
 
    
  ## Converting non numeric features to numeric
  mergeData$hour <- as.numeric(mergeData$hour)
  mergeData$consumption.Kwh.sqm <- as.numeric(mergeData$consumption.Kwh.sqm)
  mergeData$month <- as.numeric(mergeData$month)
  mergeData$Day.of.Week <-  as.numeric(mergeData$Day.of.Week)
  mergeData$weekday <-  as.numeric(mergeData$weekday)
  mergeData$Holiday <- as.numeric(mergeData$Holiday)
  mergeData$Base_Hour_Class <- as.factor(mergeData$Base_Hour_Class)
  
  #str(mergeData)
  
  # A plot of the within groups sum of squares by number of clusters extracted can help determine the appropriate number of clusters
  # Using the Elbow function for the same
  
  wssplot <- function(data, nc=15, seed=1234){
    wss <- (nrow(data)-1)*sum(apply(data,2,var))
    for (i in 2:nc){
      set.seed(seed)
      wss[i] <- sum(kmeans(data, centers=i)$withinss)}
    plot(1:nc, wss, type="b", xlab="Number of Clusters",
         ylab="Within groups sum of squares")}
  
  ## Calling the elbow function to check within groups sum of squares by number of clusters and deciding the optimal number of clusters
  wssplot(mergeData[,-10],nc=7)
 
  
  ## Using k=3 as seen from the Bend graph 
  k.means.fit <- kmeans(mergeData[,-10],3)
  
  attributes(k.means.fit)
  k.means.fit$centers
  k.means.fit$size
  
  
  
  #
  plot(mergeData, col =(k.means.fit$cluster +1) , main="K-Means result with 6 clusters", pch=20, cex=2)
  
  library(cluster)
  clusplot(mergeData, k.means.fit$cluster, main='2D representation of the Cluster solution',
           color=TRUE, shade=TRUE,
           labels=2, lines=0)
          

table(mergeData[,10],k.means.fit$cluster)
          
####### Hierarchical Clustering

#Hierarchical methods use a distance matrix as an input for the clustering algorithm.
# The choice of an appropriate metric will influence the shape of the clusters, as some elements may be close to one another according to one distance and farther away according to another


# Create transposed data matrix
data.matrix.t <- t(as.matrix(mergeData[,-10]))

# Create distance matrix
dists <- dist(data.matrix.t)

# Clustering
hcl <- hclust(dists)

# draw dendogram with red borders around the 3 clusters
rect.hclust(hcl, k=3, border="red") 
          
#We use the Euclidean distance as an input for the clustering algorithm (Ward’s minimum variance criterion minimizes the total within-cluster variance):
H.fit <- hclust(dists, method="ward.D")

#The clustering output can be displayed in a dendrogram
plot(H.fit) # display dendogram
groups <- cutree(H.fit, k=3) # cut tree into 5 clusters

# draw dendogram with red borders around the 5 clusters
rect.hclust(H.fit, k=3, border="red") 


