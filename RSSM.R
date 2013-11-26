

# R functions for SSM





ssm.plot.hat <- function(hatpath,packagepath,plotdata=0){
  hat <- as.data.frame(read.csv(paste(packagepath,hatpath,sep=""), header = TRUE))
  data <- import.data(packagepath)
  names = names(hat)
  hat <- merge(hat,data)
	ps = list()
	hat$Date = as.Date(hat$date)
  hat$ObsDate = as.Date(hat$obsdates)
	for (i in 1:((length(names)-2)/3) ){
		ps[[i]] = ggplot(hat,aes_string(x='Date',y = names[3*(i-1)+2])) + geom_line() + 
      geom_ribbon(aes_string(ymin=names[3*(i-1)+3], ymax=names[3*(i-1)+4]),fill="skyblue",alpha=0.6) +
      ylab(substr(names[3*(i-1)+2],6,nchar(names[3*(i-1)+2])))
	}	
  i = ((length(names)-1)/3)
  if (plotdata){
  ps[[i]] = ggplot(hat,aes_string(x='Date',y = names[3*(i-1)+2])) + geom_line() + 
    geom_ribbon(aes_string(ymin=names[3*(i-1)+3], ymax=names[3*(i-1)+4]),fill="skyblue",alpha=0.6) +
    ylab(substr(names[3*(i-1)+2],6,nchar(names[3*(i-1)+2]))) + 
    geom_point(aes_string(x='ObsDate', y='obsvalues'),color="chartreuse3")
  } else {
    ps[[i]] = ggplot(hat,aes_string(x='Date',y = names[3*(i-1)+2])) + geom_line() + 
      geom_ribbon(aes_string(ymin=names[3*(i-1)+3], ymax=names[3*(i-1)+4]),fill="skyblue",alpha=0.6) +
      ylab(substr(names[3*(i-1)+2],6,nchar(names[3*(i-1)+2]))) 
  }
    
	multiplot(plotlist = ps, cols = 3)
}


ssm.plot.X <- function(Xpath,packagepath){
  X <- as.data.frame(read.csv(paste(packagepath,Xpath,sep=""), header = TRUE))
#  data <- import.data(packagepath)
  names = names(X)
#  X <- merge(X,data)
  ps = list()
  X$Date = as.Date(X$date)
#  X$ObsDate = as.Date(X$obsdates)
  for (i in 2:(length(names)-1)){
    ps[[i-1]] = ggplot(X,aes_string(x='Date',y = names[i], group = "index")) + geom_line() 
  }  
  multiplot(plotlist = ps, cols = 3)
}


ssm.plot.trace <- function(tracepath,packagepath){
  trace <- as.data.frame(read.csv(paste(packagepath,tracepath,sep=""), header = TRUE))
	ps = list()
	names = names(trace)
	for (i in 1:((length(names)-1)) ){
		ps[[i]] = ggplot(trace,aes_string(x="index",y = names[i])) + geom_line() 
	}
  multiplot(plotlist = ps, cols = 3)
}




ssm.plot.post <- function(tracepath,packagepath){
  trace <- as.data.frame(read.csv(paste(packagepath,tracepath,sep=""), header = TRUE))
	ps = list()
	names = names(trace)
	for (i in 1:((length(names)-1)) ){
		ps[[i]] = ggplot(trace, aes_string(x = names[i])) +  geom_density(alpha=.2, fill="#FF6666") 
	}
	multiplot(plotlist = ps, cols = 3)
}


ssm.plot.scatter <- function(tracepath,packagepath){
  trace <- as.data.frame(read.csv(paste(packagepath,tracepath,sep=""), header = TRUE))
  ps = list()
  names = names(trace)
  ind = 0
  for (i in 1:((length(names)-3)) ){
    for (j in i+1:((length(names)-3)) ){
      ind = ind + 1
      ps[[ind]] = ggplot(trace, aes_string(x = names[i], y= names[j])) +  geom_point(shape=1) 
    }
  }
  multiplot(plotlist = ps, cols = 3)
}


import.data <- function(packagepath){
  json_file <- fromJSON(paste(packagepath,'/bin/.data.json',sep=""))
  dates = rep('',length(json_file$data)-1)
  values = rep(0,length(json_file$data)-1)
  for (i in 1:(length(json_file$data)-1)){
    dates[i] = json_file$data[[i]]$date
    if (length(json_file$data[[i]]$values)>0){
      values[i] = json_file$data[[i]]$values*1.0
    } else 
      values[i] = NaN
  }
  return(data.frame(obsdates=dates,obsvalues=values))
}


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}