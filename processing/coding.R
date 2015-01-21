library("ggplot2")
library("reshape")

# Read the codes from a filename, and convert them to a true/false table
# as per codesToTable

codesFromFile <- function(filename="../data/Q4.csv",colname="Consolidated") {
  csv <- read.csv(filename)
  codesToTable(csv,colname)
}
  

#Converts a data.frame with one column holding a separated list
#of tags to a data.frame with one column for each tag, and boolean presence/absence
#
# id, tag1, tag2, tag3
# 1   true  false true
# 2   false true  false

codesToTable <- function(data,colname,idcol="personid",separator="\\s*[;,]\\s*") {
  #Pull out unique tags
  tags = unique(unlist( lapply(as.character(data[,colname]),function(x) strsplit(x,separator) ) ) )
  print(paste("Tags in",colname)); print(tags)
  
  #New data.frame for results with ID column in
  d <- data.frame(id=data[,idcol])
  names(d) <- c(idcol)

  # Column for each tag with true/false in
  for( t in tags ) { d[[t]] = grepl(t,data[,colname])}
  d
}

# Takes a table made by codesToTable and plots a bar chart
tableToBar <- function(data) {
	# Convert to long form, i.e. id,tagname,true/false
	md = melt(data,id.vars="personid")
	#Filter just the true ones
	mdf = md[md$value == T,]
	#aes(factor...) means count for the bar chart
	ggplot(mdf,aes(factor(variable))) + geom_bar() + coord_flip()
}

# Takes a list of tables from codesToTable, plots a chart with all the tags in
multipleToBar <- function(data) {
	d <- data.frame()
	for( n in names(data) )
	{
		df <- melt(data[[n]],id.vars="personid")
		df$question = n
		d <- rbind(d,df)
	}
	va = aggregate(d$value,by=list(Tag=d$variable,Question=d$question),FUN=sum)
	print(va)
	ggplot(va,aes(x=Tag,y=x),fill=Question) + geom_bar(aes(fill=Question),position="dodge",stat="identity") + coord_flip()
}
