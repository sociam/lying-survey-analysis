setwd('~/Desktop/lying-survey-analysis/data')
datas <- read.csv('initial_results.csv', header=TRUE);
lyinghist <- function(colname,survey) { 
   surveysubset <- survey[!is.na(survey[,colname]),colname];
   hist(surveysubset, breaks=c(0.5,1.5,2.5,3.5,4.5,5.5),main=colname);
   dim(as.array(surveysubset))
}
## usage likerts
usage <- c("q1.facebook","q1.grindr","q1.instagram","q1.tinder","q1.tumblr","q1.twitter","q1.vine","q1.yelp","q1.youtube")

## demographics
demographics <- c("q12.age","q12.gender","q12.genderdisclosed","q12.smphone","q12.smtabletother","q12.smweb")

for (i in 1:dim(as.array(usage))) { 
  c <- usage[i];
  #print(c); 
  #frame(); 
  lyinghist(c,datas)
}

lyinghist(usage[1], datas)

plot(table(datas[,'q12.age']))

