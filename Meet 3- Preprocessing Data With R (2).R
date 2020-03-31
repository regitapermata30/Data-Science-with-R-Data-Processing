datafix <- read.csv("D:/STATISTIKA/S2/DSI/DSC STATISTIKA/datacancer.csv",header=T,sep=";")
head(datafix)
str(datafix)

datafix$radius_mean <- as.numeric(datafix$radius_mean)
datafix$radius_se <- as.numeric(datafix$radius_se)
datafix$area_se <- as.numeric(datafix$area_se)
datafix$radius_worst <- as.numeric(datafix$radius_worst)
str(datafix)
datatrans <- datafix[,-1]
n <- dim(datatrans)[1]
p <- dim(datatrans)[2]

#transformasi data ke standardize
xbar <- matrix(ncol=p, nrow=1)
varx <-matrix(ncol=p,nrow=1)
z <- matrix(ncol=p,nrow=n)
for (i in 1:p){
  xbar[i] <- (mean(datatrans[,i]))
  varx[i] <- var(datatrans[,i])
  for (j in 1:n){
  z[j,i] <- ((datatrans[j,i]-xbar[i])/(sqrt(varx[i])))
  }
}
colnames(z) <- colnames(datatrans)
head(z) #-1 sampai 1
dim(z)[2]

#fungsi mendapatkan jarak mahalanobis
distance<-function(x)
{
  x<-as.data.frame(x)
  mu<-colMeans(x)
  S<-cov(x)
  invS<-solve(S)
  d<-matrix(rep(0,nrow(x)),nrow(x),1)
  eval<-matrix(rep(0,nrow(x)),nrow(x),1)
  q<-qchisq(0.5,ncol(x))
  for (i in 1:nrow(x))
  {
    d[i]<-as.numeric(x[i,]-mu) %*% (invS) %*% as.numeric(t(x[i,]-mu))
    ifelse (d[i]<=q, eval[i]<-1, eval[i]<-0)
  }
  prop <- sum(eval)/nrow(x)
  result<- list(distance=d, chisquared=q, 
                proportion=prop
                )
  return (result)
}

distance_k <- distance(z)$distance
ord_dis <- sort(distance_k)
j2 <- seq(1,n)
j22 <- (j2-0.5)/n
qj2 <- qchisq(j22,p)
plot(ord_dis,qj2,pch=19,title("QQ Plot"),ylab="Chi-Square",xlab="Square distance")

chis_tab <- qchisq(0.95,p)
p
kategori=matrix(nrow=n,ncol=1)
for (i in 1:length(distance_k)){
  if (distance_k[i] > chis_tab){
    kategori[i] <- 1 #outlier
}  
else kategori[i] <- 0   #no outlier
}

kategori
databaru <- cbind(datafix,kategori)
datanooutlier <- subset(databaru,databaru$kategori==0)
boxplot(datanooutlier)
dim(datanooutlier)

#selection feature 
#menganggap radius_mean sebagai depedent variabel
corel <- cor(datanooutlier$radius_mean,datanooutlier[,c(3:20)])

win.graph()
par(mfrow=(c(3,6)))
for (i in 1:length(datanooutlier[,3:20]))
{
  plot(datanooutlier$radius_mean,datanooutlier[,i])
}

#Stepwise selection

datanew <- datanooutlier[,2:21]
library(MASS)
library(leaps)
library(caret)
# Fit the full model 
full.model <- lm(radius_mean ~., data = datanew)
summary(full.model)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE) #library MASS
summary(step.model)

#tunning parameter set variabels in nvmax= the number of variable in the model
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 5)
# Train the model

step.model <- train(radius_mean ~., data = datanew,
                    method = "leapSeq", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
)
step.model$results

#best tuning
step.model$bestTune
coef(step.model$finalModel, 3)

#trying feature selection using backward
step.model2 <- stepAIC(full.model, direction = "backward", 
                      trace = FALSE)
summary(step.model2)

set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model2 <- train(radius_mean~., data = datanew,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:6),
                    trControl = train.control
)
step.model2$results

#best tuning  
step.model2$bestTune
coef(step.model2$finalModel,2)



### PCA ###
y <- datafix[,2]
xnum <- datafix[,-c(1:2)]
head(y)
head(xnum)
set.seed(101)
pca <- prcomp(xnum,center=TRUE,scale.=TRUE)
summary(pca)
View(pca$x)
View(pca$rotation)
#screeplot
screeplot(pca, type = "l", npcs = 19, main = "Screeplot") 
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)
#plot 2D
plot(pca$x[,1],pca$x[,2], xlab="PC1", ylab = "PC2", main = "PC1 PC2 Plot")
