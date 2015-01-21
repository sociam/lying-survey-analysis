library("ggplot2")
library("reshape2")

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
  for( t in tags ) { d[[t]] = unlist(lapply(as.character(data[,colname]),function(x) {hasTag(t,x,separator)}))}
					
  d
}

hasTag <- function(tag,input,separator) {
	tags = unlist(strsplit(as.character(unlist(input)),separator))
  if( tag %in% tags ) T
	else F
}

# Takes a table made by codesToTable and plots a bar chart
tableToBar <- function(data,remove=c()) {
	# Convert to long form, i.e. id,tagname,true/false
	md = melt(data,id.vars="personid")
	#Filter just the true ones
	mdf = md[md$value == T,]
	# Remove the tags we don't want
	for( r in remove ) d <- d[d$variable != r,]
	#aes(factor...) means count for the bar chart
	ggplot(mdf,aes(factor(variable))) + geom_bar() + coord_flip() +
			scale_y_continuous(name="Count") + scale_x_discrete(name="Code") +
			theme_minimal()
}

# Takes a list of tables from codesToTable, plots a chart with all the tags in
multipleToBar <- function(data,remove=c()) {
  print("Starting multiple bar")
	d <- data.frame()
  print("Melting and binding")
	for( n in names(data) )
	{
    print("Input")
    print(head(data[[n]]))
    print("Melting")
		df <- melt(data[[n]],id.vars="personid")
    print("Melted")
		df$question = n
    print(head(df))
    print("Binding")
		d <- rbind(d,df)
	}
  
  # Remove the tags we don't want
print("Removing tags")
  for( r in remove ) d <- d[d$variable != r,]
  
  # Aggregate
print("Aggregating")
	va = aggregate(d$value,by=list(Tag=d$variable,Question=d$question),FUN=sum)
  va$Question = factor(va$Question)
  va$Tag = factor(va$Tag)
  
	# Add in zero values
  #cb = expand.grid(Tag=levels(va$Tag), Question=levels(va$Question))
  #cb$x = 0
	#va <- rbind(va, cb)
  
  print("Data to plot")
  print(va)
  
	g <- ggplot(va,aes(x=reorder(Tag,x),y=x),fill=Question) + 
			geom_bar(aes(fill=Question),position="dodge",stat="identity") + coord_flip() +
    	scale_y_continuous(name="Count") + scale_x_discrete(name="Code") 
  # if( exists("theme_tufte") ) g <- g + theme_tufte()
	#else 
		g <- g + theme_minimal()
  g
}

questionData <- function() {
	q4 <<- codesFromFile("../data/all_results.csv","q4.coded")
	q5a <<- codesFromFile("../data/all_results.csv","q5a.coded")
	q5b <<- codesFromFile("../data/all_results.csv","q5b.coded")
}

# Assumes that questionData has been run to create the q5a and q5b variables
q5graph <- function() {
	d <- multipleToBar(list(Pseudonym=q5a,Persona=q5b),remove=c("-","no")) +
			theme(legend.justification=c(1,0), legend.position=c(1,0))
  ggsave("../output/q5frequency.pdf",d,width=5)
}

# Assumes that questionData has been run to create the q5a and q5b variables
q4graph <- function() {
	d <- multipleToBar(list(q4=q4),remove=c("-","no"))  +
      theme(legend.position="none")
	ggsave("../output/q4frequency.pdf",d,width=5,height=3)
}

# Assumes that questionData has been run to create the q5a and q5b variables
q5agraph <- function() {
	d <- multipleToBar(list(Pseudonym=q5a),remove=c("-","no")) +
      theme(legend.position="none")
	ggsave("../output/q5afrequency.pdf",d,width=5,height=5)
}

# Assumes that questionData has been run to create the q5a and q5b variables
q5bgraph <- function() {
	d <- multipleToBar(list(Persona=q5b),remove=c("-","no")) +
      theme(legend.position="none")
	ggsave("../output/q5bfrequency.pdf",d,width=5,height=4)
}


makeGraphs <- function() {
  questionData()
	q5graph()
	q5agraph()
	q5bgraph()
	q4graph()
}
