##' Download Phenocam data
##' 
##' @param URL  web address where data is located
download.phenocam <- function(URL) {
  ## check that we've been passed a URL
  if (length(URL) == 1 & is.character(URL) & substr(URL,1,4)=="http") {
    ## read data
    dat <- read.csv(URL,skip = 22)
    
    ## convert date
    dat$date <- as.Date(as.character(dat$date))
    
    return(dat)
  } else {
    print(paste("download.phenocam: Input URL not provided correctly",URL))
  }
}

## Define ciEnvelope function
ciEnvelope <- function(x,ylo,yhi,col="lightgrey",...){
  ## identify chunks of data with no missing values
  has.na = apply(is.na(cbind(x,ylo,yhi)),1,sum)
  block = cumsum(has.na);block[has.na>0] = NA
  blocks = na.omit(unique(block))
  
  for(i in blocks){
    sel = which(block==i)
    polygon(cbind(c(x[sel], rev(x[sel]), x[sel[1]]), c(ylo[sel], rev(yhi[sel]),
                                                       ylo[sel[1]])), col=col,border = NA,...) 
  }
}
##' Plot Phenocam data
##' 
##' @param dat  dataframe of date, gcc_mean, gcc_std
##' @param ...  additional graphing parameters
plot.phenocam <- function(dat,...){
  
  if(!is.null(dat)){
    
    ## QC flags
    gcc_mean = dat$gcc_mean
    gcc_mean[dat$outlierflag_gcc_mean>-9999] = NA
    
    ## base plot
    plot(dat$date,dat$gcc_mean,type='l',...)
    
    ## calculate CI
    ylo = dat$gcc_mean-1.96*dat$gcc_std
    yhi = dat$gcc_mean+1.96*dat$gcc_std
    
    ## add confidence envelope
    ciEnvelope(dat$date,ylo,yhi)
    
    ## replot mean line
    lines(dat$date,dat$gcc_mean,lwd=1.5)
  } else {
    print("plot.phenocam: input data not provided")
  }
  
}