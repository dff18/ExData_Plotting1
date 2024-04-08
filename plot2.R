plot2 <- function(path) {
  
  #/////////////////////////////////////
  #PREPARING DATA
  #/////////////////////////////////////
  
  data <- read.table(path,sep=";",skip=1)
  names(data) <- c("Date","Time","GAP","GRP","Voltage","Global Intensity","SM1","SM2","SM3")
  data_s <- subset(data,Date=="1/2/2007"|Date=="2/2/2007")
  
  for (i in 3:9) {
    for (j in 1:nrow(data_s)) {
      if (data_s[j,i]=="?") {
        data_s[i,j] <- NA
      }
    }
  }
  
  for (i in 3:9) {
    data_s[,i] <- as.numeric(data_s[,i])
  }
  
  for (i in 1:nrow(data_s)) {
    data_s[i,10] <- paste(data_s[i,1],data_s[i,2])
  }
  names(data_s)[10] <- "DateTime"
  data_s$DateTime <- strptime(data_s$DateTime,format="%d/%m/%Y %H:%M:%S")
  
  #/////////////////////////////////////
  #PLOTTING
  #/////////////////////////////////////
  
  png(filename="plot2.png")
  par(pin=c(5,5))
  with(data_s,plot(DateTime,GAP,labels=FALSE,type="n",tick=FALSE,
                   xlab="",ylab="Global Active Power (kilowatts)"))
  axis(1,at=c(as.numeric(min(data_s[,10])),as.numeric(median(data_s[,10])),
              as.numeric(max(data_s[,10]))),labels=c("Thu","Fri","Sat"))
  axis(2,at=c(0,2,4,6))
  with(data_s,lines(DateTime,GAP))
  dev.off()
  
}