setwd('~/Desktop/lying-survey-analysis/data')
datas <- read.csv('initial_results.csv', header=TRUE);
likerthist <- function(colname,survey) { 
   surveysubset <- survey[!is.na(survey[,colname]),colname];
   hist(surveysubset, breaks=c(0.5,1.5,2.5,3.5,4.5,5.5),main=colname);
   dim(as.array(surveysubset))
}
## usage likerts
usage <- c("q1.facebook","q1.grindr","q1.instagram","q1.tinder","q1.tumblr","q1.twitter","q1.vine","q1.yelp","q1.youtube")
for (i in 1:dim(as.array(usage))) { 
  likerthist(usage[i],datas);
}

lying <- c("q2.privconcern","q2.privfriends","q3.corps","q3.coworkers","q3.friends","q3.govts","q3.impression","q4.lies", "q4.liesfriends")
for (i in 1:dim(as.array(lying))) { likerthist(lying[i],datas); }

## demographics
demographics <- c("q12.age","q12.gender","q12.genderdisclosed","q12.smphone","q12.smtabletother","q12.smweb")
likerthist(usage[1], datas)
barplot(table(datas[,'q12.age']),main='age')
barplot(table(datas[,'q12.gender']),main='genderdisclosed')
barplot(table(datas[,'q12.genderdisclosed']),main='gender')
barplot(table(datas[,'q12.smphone']),main='via smartphones')
barplot(table(datas[,'q12.smtabletother']),main='via tablets')
barplot(table(datas[,'q12.smweb']),main='via the web')
barplot(table(datas[,'q11.tweet']),main='how much do you tweet')
barplot(table(datas[,'q11.twitteraccts']),main='how many twitter accts')

## just testing - do people tweet more when they say they use twitter?
print(summary(aov(q1.twitter ~ q11.tweet, data=datas)))

## 
# privacy vs age?
print(summary(aov(q2.privconcern ~ q12.age, data=datas)))
print(summary(aov(q2.privconcern ~ q12.gender, data=datas)))
print('priv concern twitter')
print(summary(aov(q2.privconcern ~ q1.twitter, data=datas)))
print('facebook')
print(summary(aov(q1.facebook ~ q12.age, data=datas)))
print('tumblr')
print(summary(aov(q1.tumblr ~ q12.age, data=datas)))

nonblanks <- function(colname,survey) { 
  si <- as.character(survey[,colname]);
  si[nchar(si)>0]
}
  

