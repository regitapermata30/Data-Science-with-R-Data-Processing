age <- c(23, 16, NA)
mean(age)
mean(age, na.rm = TRUE)

age1 <- c(21,42,18, 21 )
height <-c(6,5.9,NA,NA)
data <- data.frame(cbind(age1,height))
complete.cases(data)
(persons_complete <- na.omit(data))
#imputasi mean variabel height
data$height[is.na(data$height)] <- mean(data$height, na.rm = T)

#Simulasi preprocessing pada data
data<-read.csv("D:/STATISTIKA/S2/DSI/house_prices_melbourne-master/Melbourne_housing_FULL.csv",header=T,sep=",")
View(data)

#Pre-Processing DATA MANIPULATION
#1. Inconsistent Data
str(data)
summary(data)
data$Date<-as.Date(data$Date, "%d/%m/%y")
str(data)


#2. Missing Value
## Indentifikasi Missing Value
summary(data)
a<-data.frame(sum(is.na(data[,1]))) #missing value kolom 1 
colnames(a)<-"Jumlah missing value" 
b<-data.frame(sum(is.na(data[,2]))) #missing value kolom 2
colnames(b)<-"Jumlah missing value"
c<-rbind(a,b)

n<-length(data)-2
#menghitung missing value disetiap variabel
for (i in 1:n)
{
  d<-data.frame(sum(is.na(data[,i+2])))
  colnames(d)<-"Jumlah missing value"
  c<-rbind(c,d)
}
variabel<-data.frame(colnames(data))
colnames(variabel)<-"variabel"
jumlahmv<-cbind(variabel,c)
dim(data)


##3. Imputasi Missing Value dengan Median dan Modus
clean2<-data

boxplot(data$Price)
#imputasi price
meprice = median(clean2$Price,na.rm = TRUE)
clean2$Price[is.na(clean2$Price)] = meprice
sum(is.na(clean2$Price)) #checking missing

#imputasi bedroom
mebed = median(clean2$Bedroom2,na.rm = TRUE)
clean2$Bedroom2[is.na(clean2$Bedroom2)] = mebed


#imputasi bathroom
mebath = median(clean2$Bathroom,na.rm = TRUE)
clean2$Bathroom[is.na(clean2$Bathroom)] = mebath


mecar = median(clean2$Car,na.rm = TRUE)
clean2$Car[is.na(clean2$Car)] = mecar


meland = median(clean2$Landsize,na.rm = TRUE)
clean2$Landsize[is.na(clean2$Landsize)] = meland


mebuilding = median(clean2$BuildingArea,na.rm = TRUE)
clean2$BuildingArea[is.na(clean2$BuildingArea)] = mebuilding

#x <- c(3,2,2,5,2,8,8)
#unique(x)

#menghitung modus
getmode<-function(v){
  uniqv<-unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
modeyear = getmode(clean2$YearBuilt[!is.na(clean2$YearBuilt)])
modeyear
clean2$YearBuilt[is.na(clean2$YearBuilt)] = modeyear
clean2$YearBuilt<-as.factor(clean2$YearBuilt)
unique(data$YearBuilt)
mela = median(clean2$Lattitude,na.rm = TRUE)
clean2$Lattitude[is.na(clean2$Lattitude)] = mela

melong = median(clean2$Longtitude,na.rm = TRUE)
clean2$Longtitude[is.na(clean2$Longtitude)] = melong

sum(is.na(clean2))

str(clean2)
summary(clean2)

#save data pakai write.csv
write.csv(clean2,file="D:/STATISTIKA/S2/DSI/house_prices_melbourne-master/clean2.csv")

#4.Handling outlier univariate using IQR
datafix <- read.csv("D:/STATISTIKA/S2/DSI/house_prices_melbourne-master/clean2.csv",header=T,sep=",")
head(datafix)
summary(datafix$Price)
boxplot(datafix$Price)
removeoutlier <- function(x){
  #Get Q1 and Q3
  qnt = quantile(x,probs=c(0.25,0.75))
  #Get the interquartile range time 1.5
  iqt=1.5*IQR(x)
  
  #apply on a copy of the original data
  y=x
  y[x<(qnt[1]-iqt)]=NA
  y[x>(qnt[2]+iqt)]=NA
  return(y[complete.cases(y)])
}
boxplot(datafix$Distance)
xx<-removeoutlier(datafix$Distance)
length(xx)
length(datafix$Distance)
boxplot(xx)

par(mfrow=c(2,1))
hist(xx)
hist(datafix$Distance)







#EDA
str(clean1)
library(ggplot2)
library(ggpubr)
theme_set(theme_light())
## HISTOGRAM
ggplot(clean1,aes(x=Price, fill=Type))+geom_histogram(color="white")

## BARCHART
ggplot(clean1,aes(x=Regionname,fill=Type))+geom_bar(color="black")
## PIE Chart
summary(clean1$Type)
df = data.frame(group = c("u", "t", "h"),value = c(1540,722,6625))
df
labs<-paste0(df$group," (",df$value,")")
ggpie(df, "value", label = labs,lab.pos="in",lab.font="white",fill="group",color="white",palette = c("#00AFBB", "#E7B800", "#FC4E07"))


