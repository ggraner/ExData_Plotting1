
create_plot4<-function() {
        ### 1 - DOWNLOADING DATA ###
        
        # create data subfolder if not exists
        if(!file.exists("./data")) {dir.create("./data")}
        # project data
        fileUrl<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        if(!file.exists("./data/exdata_data_household_power_consumption.zip")) {
                cat("downloading dataset...\n")
                download.file(fileUrl, destfile="./data/exdata_data_household_power_consumption.zip", method="curl")
                cat("extract dataset...\n")
                unzip("./data/exdata_data_household_power_consumption.zip", exdir="./data")
        }
        data_path<-file.path("./data")
        
        ### 2 - READ DATA ###
        
        data <- read.table(text = grep("^[1,2]/2/2007", readLines(file.path(data_path,"household_power_consumption.txt")), value = TRUE), 
                           sep = ";", header = TRUE, na.strings = "?")
        # Calc memory size for later usage
        data_size_mb<-object.size(data)/1048600
        # Set col names
        names(data)<-c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", 
                       "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
        
        dates<-data$Date
        times<-data$Time
        #  Convert datetime
        data$DateTime<-paste(dates, times)
        data$DateTime<-strptime(data$DateTime, "%d/%m/%Y %H:%M:%S")
        data$DateTime <- as.POSIXct(data$DateTime)
        
        ### 3 - CREATE & SAVE PLOT ###
        
        png(filename = "plot4.png", width = 480, height = 480, units = "px")
        par(mfrow = c(2,2), mar = c(4,4,2,1), oma = c(0,0,2,0))
        with(data, {
                plot(Global_active_power ~ DateTime, type = "l", 
                     ylab = "Global Active Power", xlab = "")
                plot(Voltage ~ DateTime, type = "l", ylab = "Voltage", xlab = "datetime")
                plot(Sub_metering_1 ~ DateTime, type = "l", ylab = "Energy sub metering",
                     xlab = "")
                lines(Sub_metering_2 ~ DateTime, col = 'Red')
                lines(Sub_metering_3 ~ DateTime, col = 'Blue')
                legend("topright", col = c("black", "red", "blue"), lty = 1, lwd = 2, 
                       bty = "n",
                       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
                plot(Global_reactive_power ~ DateTime, type = "l", 
                     ylab = "Global_reactive_power", xlab = "datetime")
        })
        # Close Device IMPORTANT
        dev.off()
        cat("Plot has been saved to plot4.png\n")
        
}