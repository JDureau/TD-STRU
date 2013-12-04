

# R functions for SSM





ssm.plot.hat <- function(hatpath,packagepath,plotdata=0){
  hat <- as.data.frame(read.csv(paste(packagepath,hatpath,sep=""), header = TRUE))
  data <- import.data(packagepath)
  names = names(hat)
  datanames = names(data)
  hat <- merge(hat,data)
	ps = list()
	hat$Date = as.Date(hat$date)
  nobs = ncol(data)-1
	for (i in 1:((length(names)-1-nobs)/3) ){
		ps[[i]] = ggplot(hat,aes_string(x='Date',y = names[3*(i-1)+2])) + geom_line() + 
      geom_ribbon(aes_string(ymin=names[3*(i-1)+3], ymax=names[3*(i-1)+4]),fill="skyblue",alpha=0.6) +
      ylab(substr(names[3*(i-1)+2],6,nchar(names[3*(i-1)+2])))
	}	
  for (ind in 1:nobs){
    i = ((length(names)-1-nobs*3)/3) + ind
    if (plotdata){
      ps[[i]] = ggplot(hat,aes_string(x='Date',y = names[3*(i-1)+2])) + geom_line() + 
        geom_ribbon(aes_string(ymin=names[3*(i-1)+3], ymax=names[3*(i-1)+4]),fill="skyblue",alpha=0.6) +
        ylab(substr(names[3*(i-1)+2],6,nchar(names[3*(i-1)+2]))) + 
        geom_point(aes_string(x='Date', y=datanames[ind+1]),color="chartreuse3")
    } else {
      ps[[i]] = ggplot(hat,aes_string(x='Date',y = names[3*(i-1)+2])) + geom_line() + 
        geom_ribbon(aes_string(ymin=names[3*(i-1)+3], ymax=names[3*(i-1)+4]),fill="skyblue",alpha=0.6) +
        ylab(substr(names[3*(i-1)+2],6,nchar(names[3*(i-1)+2]))) 
    }
  }
    
	multiplot(plotlist = ps, cols = 3)
}


ssm.plot.X <- function(Xpath,packagepath,plotdata=0){	
	print('you')
  X <- as.data.frame(read.csv(paste(packagepath,Xpath,sep=""), header = TRUE))
  data <- import.data(packagepath)
  names = names(X)
  datanames = names(data)
  X <- merge(X,data)
  ps = list()
  X$Date = as.Date(X$date)
  nobs = ncol(data)-1
  for (i in 1:(length(names)-2-nobs*2) ){
    ps[[i]] = ggplot(X,aes_string(x='Date',y = names[i+1], group = "index")) + geom_line() 
  }	
  for (ind in 1:nobs){
    i = (length(names)-2-nobs*2) + ind
    if (plotdata){
      ps[[i]] = ggplot(X,aes_string(x='Date',y = names[i+1], group = "index")) + geom_line() + 
        geom_point(aes_string(x='Date', y=datanames[ind+1]),color="chartreuse3")
    } else {
      ps[[i]] = ggplot(X,aes_string(x='Date',y = names[i+1], group = "index")) + geom_line() 
    }
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
		ps[[i]] = ggplot(trace, aes_string(x = names[i])) +  geom_density(alpha=.2, fill="#FF6666",adjust=3) 
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
  values = list()
  nmax  = 0
  for (i in 1:(length(json_file$data)-1)){
    nmax = max(nmax,length(json_file$data[[i]]$observed))
  }    
  for (i in 1:(length(json_file$data)-1)){
    if(i == 1){
      data = data.frame(date=NaN)
      for (j in 1:nmax){
        data[paste('obs',as.character(j),sep='')] = NaN 
      }
    } else {
      data = rbind(data,rep(NaN,nmax+1))
    }
    data[i,1] = json_file$data[[i]]$date
    for(j in 1:length(json_file$data[[i]]$observed)){
      data[i,json_file$data[[i]]$observed[j]+2] = json_file$data[[i]]$values[j]*1.0
    } 
  }
  return(data)
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