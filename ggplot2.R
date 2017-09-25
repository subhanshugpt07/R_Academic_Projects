##########    DATA VISUALIZATON    ::  ggplot2  ::    ##########
#NUMBER 12   ggplot(mtcars, aes(x = mpg, y = qsec, col = factor(cyl), shape = factor(am), size = (hp/wt))) +geom_point()
install.packages("arules")

install.packages("ggplot2")
install.packages("gridExtra")
library("gridExtra")
library("ggplot2")
library("ggvis")
search()  
iris<-read.csv("iris.csv")
View(iris)
maths<-read.csv("Maths.csv")
View(maths)
diamond<-read.csv("diamond.csv")
View(diamond)
movie<-read.csv("https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/Week-03/Movies.csv")
movie$numbers<-1:161
movie$num10<-seq(1,1610,by=10)
View(movie)

#1######################          important 

ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width),fill=Species,col=Species,alpha(0.1))+geom_point(col="red")?fill
ggplot(movie,aes(x=factor(movie$Genre),y=movie$Overall_Rating))+geom_point()

factor(movie$Genre)
levels(movie$Genre)
str(movie$Genre)

factor(movie$Num_Theaters)
?goem_point
?aes

#2######################          important
ggplot(movie,aes(x=movie$Num_Theaters,y=movie$Overall_Rating,col=movie$numbers,size=movie$numbers))+geom_point()

#3######################          important
ggplot(movie,aes(x=movie$Opening_Week_Revenue,y=movie$numbers,col=movie$numbers))+geom_point(alpha=0.5)+geom_smooth()
ggplot(movie,aes(x=movie$Genre,y=movie$Opening_Week_Revenue,col=movie$Genre))+geom_smooth()

#4######################          important  #another way to use ggplot 
arnav<-ggplot(movie,aes(x=movie$Num_Theaters,y=movie$Overall_Rating))
arnav+geom_point(aes(col=movie$Genre,size=movie$numbers))
set.seed(1) #error shading= se
arnav+geom_point(aes(col=movie$Genre,size=movie$numbers))

#5######################          important
p<-ggplot(iris,aes(x=iris$Sepal.Length,y=iris$Sepal.Width,col=iris$Species))
s<-position_jitter(0.1)
s
a<-p + geom_jitter()
a

b<-p + geom_point()
b
b1<-p + geom_point(position=s)
b1

c<-p + geom_tile()
c

grid.arrange(a,b,c,nrow = 2,ncol = 2)



#6######################          important 

plot(model,col=movie$Genre)
movie$Genre<-as.factor(movie$Genre)
model<-lm(movie$Opening_Week_Revenue~movie$Num_Theaters)
summary(model)
abline(model,lty=2)

#ggplot2 concept 
ggplot(movie,aes(x=movie$Num_Theaters,y=movie$Opening_Week_Revenue,col= movie$Genre))+geom_point()+geom_smooth(aes(group=1),se=F,formula = y~x,method = lm,linetype=4)+geom_smooth(method = lm,se=F)

?aes(group)

View(iris)

#7######################          important 
long<-gather(iris,Part,Value,-Species)
View(long)
iris_tidy<-separate(long,Part,c("Part","Measure"))#(sep=".")
View(iris_tidy)
ggplot(iris_tidy,aes(x=Species,y=Measure,col=Part))+geom_jitter()
ggplot(iris_tidy,aes(x=Species,y=Measure,col=Part))+geom_jitter()+facet_grid(~LAW)#(sep=".")
ggplot(iris_tidy,aes(x=Part,y=Measure,col=Species))+geom_jitter()+facet_grid(~LAW)#(sep=".")
ggplot(iris_tidy,aes(x=Part,y=Measure,col=LAW))+geom_jitter()+facet_grid(~Species)#(sep=".")


#8######################          important 
iris_tidy<-separate(long,Part,c("Part","Measure"))#(sep=".")
View(iris_tidy)

#iris_tidy$row <- 1:nrow(iris_tidy) ## added explicit row numbers

#9######################          important 

ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width,col=Species))+geom_point()#+facet_grid(~Species)

#10######################         important 
mtcars<-read.csv("mtcars.csv")
View(mtcars)
ggplot(mtcars, aes(x = wt, y = mpg, col ="cyl",fill=cyl,labels=cyl))+geom_point(shape = 16, size = 5,alpha=0.9)

#11######################         important 
color<-"#123456"
ggplot(mtcars, aes(x = wt, y = mpg, col =color,fill=cyl,labels=cyl))+geom_point(shape = 16, size = 5,alpha=0.9,col=color)

#12######################         important     IMPORTANT    
ggplot(mtcars, aes(x = mpg, y = qsec, col = factor(cyl), shape = factor(am), size = (hp/wt))) +geom_point()
#13######################         important 
View(maths)
ggplot(maths,aes(x=Mjob,fill=guardian))+geom_bar()

ggplot(maths,aes(x=Mjob,fill=factor(guardian),col=G3))+geom_bar(position = "fill")

ggplot(maths,aes(x=Mjob,fill=guardian,col=G3))+geom_bar(position = "dodge")

#14######################         important 
plot(maths$G2,maths$G3)
ggplot(maths,aes(x=G2,y=G3,col="red"))+geom_jitter(alpha=1)
ggplot(maths,aes(x=G2,y=G3,col="red"))+geom_jitter(alpha=1)+scale_y_continuous(limits = c(2,18))+scale_x_continuous(limits = c(2,15))

#15######################         important ######HISTOGRAM
ggplot(maths,aes(x=G3))+geom_histogram(binwidth = 1)
ggplot(maths,aes(x=G3))+geom_histogram(aes(y=..density..),binwidth = 1)
ggplot(maths,aes(x=G3))+geom_histogram(aes(y=..density..),binwidth = 1,fill="red")

#########################      BAR      ##########################
ggplot(maths,aes(x=G3,fill=Mjob))+geom_bar(position="stack")+scale_fill_brewer()
ggplot(maths,aes(x=G3,fill=Mjob))+geom_bar(position="fill")
ggplot(maths,aes(x=G3,fill=Mjob))+geom_bar(position="dodge")
ggplot(maths,aes(x=G3,fill=Mjob))+geom_bar(position=position_dodge(0.2),binwidth = 2,alpha=0.5)


#17#########################      Line      ##########################
beaver<-read.csv("beaver.csv")
View(beaver)
ggplot(beaver,aes(x=time,y=temp))+geom_line()

#18########################## QPLOT      important # qplot fine for couple of variables but ggplot is preferred more
mtcars
# The old way (shown)
plot(mpg ~ wt, data = mtcars)

# Using ggplot:
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

# Using qplot:
qplot(wt, mpg, data = mtcars) 

# basic scatter plot:
qplot(wt, mpg, data = mtcars)

# Categorical:
# cyl
qplot(wt, mpg, data = mtcars, size = factor(cyl))

# gear
qplot(wt, mpg, data = mtcars, size = factor(gear))

# Continuous
# hp
qplot(wt, mpg, data = mtcars, col = hp)

# qsec
qplot(wt, mpg, data = mtcars, col = qsec)
#19##########################          important 
#20##########################         important 

