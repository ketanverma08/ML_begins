install.packages('mclust')
library(mclust)
data1<- read.csv(file.choose(), header=T)
data(diabetes)
class <- diabetes$class
table(class)
head(diabetes)
X <- diabetes[,-1]
head(X)
clPairs(X, class)
clust <- mclustBIC(X)
plot(clust)
summary(clust)
mod1 <- Mclust(X, x = clust)  	#Gaussian finite mixture model fitted by EM algorithm
summary(mod1, parameters = TRUE)
plot(mod1, what = "classification")
table(class, mod1$classification)
plot(mod1, what = "uncertainty")
