plot4 <- function(path) {
  
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
  
  png(filename="plot4.png")
  par(mfrow=c(2,2))
  
  with(data_s,plot(DateTime,GAP,labels=FALSE,type="n",tick=FALSE,
                   xlab="",ylab="Global Active Power (kilowatts)"))
  axis(1,at=c(as.numeric(min(data_s[,10])),as.numeric(median(data_s[,10])),
              as.numeric(max(data_s[,10]))),labels=c("Thu","Fri","Sat"))
  axis(2,at=c(0,2,4,6))
  with(data_s,lines(DateTime,GAP))
  
  with(data_s,plot(DateTime,Voltage,labels=FALSE,type="n",tick=FALSE,
                   xlab="datetime",ylab="Voltage"))
  axis(1,at=c(as.numeric(min(data_s[,10])),as.numeric(median(data_s[,10])),
              as.numeric(max(data_s[,10]))),labels=c("Thu","Fri","Sat"))
  axis(2,at=c(234,238,242,246))
  with(data_s,lines(DateTime,Voltage))
  
  with(data_s,plot(DateTime,SM1,labels=FALSE,type="n",tick=FALSE,
                   xlab="",ylab="Energy sub metering"))
  axis(1,at=c(as.numeric(min(data_s[,10])),as.numeric(median(data_s[,10])),
              as.numeric(max(data_s[,10]))),labels=c("Thu","Fri","Sat"))
  axis(2,at=c(0,10,20,30))
  with(data_s,lines(DateTime,SM1,col="black"))
  with(data_s,lines(DateTime,SM2,col="red"))
  with(data_s,lines(DateTime,SM3,col="blue"))
  legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
         col=c("black","red","blue"),lty=1,bty="n")
  
  with(data_s,plot(DateTime,GRP,labels=FALSE,type="n",tick=FALSE,
                   xlab="datetime",ylab="Global_reactive_power"))
  axis(1,at=c(as.numeric(min(data_s[,10])),as.numeric(median(data_s[,10])),
              as.numeric(max(data_s[,10]))),labels=c("Thu","Fri","Sat"))
  axis(2,at=c(0.0,0.1,0.2,0.3,0.4,0.5))
  with(data_s,lines(DateTime,GRP))
  
  dev.off()
  
}