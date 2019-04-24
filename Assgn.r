getwd()
getwd()
d= read.csv2("fb.csv")
dim(d)
dim(fb)
d=read.csv2("fb.csv")
dim(d)
nrow(d)
ncol(d)
ncol(fb)
head(d)

##1.1 create data subset 

d=read.csv("fb.csv", sep =",")
dim(d)
head(d)
View(d)
View(head(d))
sub=d[c('comment','like','share')]
head(sub)

##store subset with write operation 
write.csv(sub,"fb-sub.csv")

## create subset with condition 
subset1=subset(sub,comment>50)
View(subset1)


##1.2 Merege Data
dataA=read.csv("fb.csv",sep=",")
dataB=read.csv("fb1.csv",sep=",")

newAB=rbind(dataA,dataB)
View(newAB)
dim(newAB)

## 1.3 sort data 
x=sub[order(-d$share),]
head(x)
x=sub[order(d$share),]
head(x)
x=sub[order(-d$share,-d$like),]
head(x)

## 1.4 Transposing data

tran=t(sub)
head(tran)
View(head(tran))


## 1.5 Melting and casting
##  Include reshape
library(reshape)

installed.packages("reshape")

melt(data=sub,id.vars = "comment")

View(sub)
melt(data=sub,id.vars = "share")

sub=d[c('postmonth','posthour','paid')]
View(head(sub))
View(cast(sub,posthour ~ postmonth,sum,value='paid'))


##data integration ********************

getwd()

## for data cleaning consider air quality as data set
airquality

View(airquality)

## 2.1 Error correction by removing NA 

mean(airquality$Ozone)
mean(airquality$Ozone,na.rm=TRUE)
max(airquality$Solar.R,na.rm=TRUE)
summary(airquality)

## 2,2 Data Correction

air=airquality
air$Ozone=ifelse(is.na(air$Ozone),median(air$Ozone,na.rm=TRUE),air$Ozone)
summary(air)
air$Solar.R=ifelse(is.na(air$Solar.R),median(air$Solar.R,na.rm=TRUE),air$Solar.R)
summary(air)

## 2.3 Data Transformation

air$Solar.Danger=air$Solar.R>150
head(air)

brks=c(0,50,100,150,200,250,300,350)

brks

air$Solar.R=cut(air$Solar.R,breaks = brks,include.lowest = TRUE)
head(air)
air1=air
air1$Month=gsub(5,"May",air1$Month)
head(air1)


## 2.5 Data Modelling

#Data Model Building Operation in R for airquality dataset
#Y = mX + c   ,where m= slope of straight lineand c= Y-intercept
#1. Load and view dataset

require("datasets")
data("airquality")
View(airquality)

## Stats of air quality for e.g int,char num
str(airquality)
head(airquality)
air=airquality

#2. Preprocess the dataset
summary(air) ## Mean median max min 
# Input monthly mean in Ozone
for (i in 1:nrow(air)){
  if(is.na(air[i,"Ozone"])){
    air[i,"Ozone"]<- mean(air[which(air[,"Month"]==air[i,"Month"]),"Ozone"],na.rm = TRUE)
  }
  # Input monthly mean in Solar.R
  if(is.na(air[i,"Solar.R"])){
    air[i,"Solar.R"]<- mean(air[which(air[,"Month"]==air[i,"Month"]),"Solar.R"],na.rm = TRUE)
  }
}
View(air)
#Normalize the dataset so that no particular attribute has more impact on clustering algorithm than others.
normalize<- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

# replace contents of dataset with normalized values
air<- normalize(air)
View(air)
str(air)
#3. Apply linear regression algorithm using Least Squares Method on â??Ozoneâ? and â??Solar.Râ?
Y<- air[,"Ozone"] # select Target attribute
X<- air[,"Solar.R"] # select Predictor attribute

model1<- lm(Y~X)
model1 # provides regression line coefficients i.e. slope and y-intercept

plot(Y~X) # scatter plot between X and Y
abline(model1, col="blue", lwd=3) # add regression line to scatter plot to see relationship between X and Y

##4. Apply linear regression algorithm using Least Squares Method on â??Ozoneâ? and â??Windâ?
Y<- air[,"Ozone"] # select Target attribute
X<- air[,"Wind"] # select Predictor attribute

model2<- lm(Y~X)
model2 # provides regression line coefficients i.e. slope and y-intercept

plot(Y~X) # scatter plot between X and Y
abline(model2, col="blue", lwd=3) # add regression line to scatter plot to see relationship between X and Y
##4. Perform prediction
# Prediction of 'Ozone' when 'Solar.R'= 10
p1<- predict(model1,data.frame("X"=0.8))
p1
# Prediction of 'Ozone' when 'Wind'= 5
p2<- predict(model2,data.frame("X"=5))
p2


###
runs=c(21,62,10,53)
players=c("yuvraj","sachin","mark","Steve")
png(file="scorecard.png")
pie(runs,players)
dev.off()

runs2=c(20,70,10)
players2=c("Rahul Gandhi","Modi","Others")
png(file="scorecard2.png")
pie(runs2,players2)
dev.off()

png(file="bar.png")
barplot(runs)
dev.off()

head(mtcars)
input=mtcars[c('mpg','cyl')]
head(input)
png(file="box.png")
boxplot(mpg~cyl,data=mtcars,xlab="Cylinders",ylab="mpg",main="Arif Chutiya Hai..!")
dev.off()

png(file="line-chart.png")
plot(runs,type = "o")
dev.off()


png(file="scatterplot-matrices.png")
pairs(~wt+mpg+cyl+disp,data=mtcars,main="scatterplot matrix")
dev.off()

png(file="histogram.png")
hist(runs,xlab="weight",col="yellow",border="blue")
dev.off
dev.off()

getwd()

input=mtcars[,c('wt','mpg')]
png(file="scatter.png")
plot(x=input$wt,y=input$mpg,xlab="weight",ylab="mpg",xlim=c(2.5,5),ylim=c(15,30),main="Wt VS Mpg")
dev.off()
