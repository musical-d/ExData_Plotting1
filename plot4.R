plot4 <- function() {
    # Set the URL to the data source. Assume the data file is in the same folder as the R script
    # If the data is in a different location alter the path below
    datafile <- "household_power_consumption.txt"
    
    # Only require data for 1/2/2007 and 2/2/2007 (UK Date format)
    # So find out where those sectiions begin and read in those lines
    # 1 entry per minute per day is made so a total of 60*24 entries per day
    date1 <- grep("1/2/2007", readLines(datafile))
    date2 <- grep("2/2/2007", readLines(datafile))
    household_power_date1 <- read.table(datafile, sep=";", skip=date1[1]-1, nrow=60*24)
    household_power_date2 <- read.table(datafile, sep=";", skip=date2[1]-1, nrow=60*24)
    
    # Combine the data for the two days
    household_power <- rbind(household_power_date1, household_power_date2)
    
    # Reading in the data 'skips' over the headers so read these in and add them to the data
    household_header <- read.table(datafile, sep=";", nrow=1)
    for (i in 1:9) { names(household_power)[i] <- as.character(household_header[1,i]) }
    
    # Remove all temporary variables to free up system memory
    rm(i, household_header, date1, date2, household_power_date1, household_power_date2, datafile)
    
    # Combine date and time fields into one timestamp
    datetime <- paste(household_power$Date, household_power$Time)
    datetime <- strptime(datetime, format="%d/%m/%Y %H:%M:%S")
    household_power <- cbind(household_power, datetime)
    rm(datetime)
    
    
    # Plot the graph and save in a PNG file
    png(filename="plot4.png", width=480, height=480)
    par(mfrow=c(2,2), mar = c(4,4,2,2))
    
    # First Plot, Top Left
    plot(household_power$datetime, household_power$Global_active_power, type="l", xlab="", ylab="Global Active Power")
    # Second Plot, Top Right
    plot(household_power$datetime, household_power$Voltage, type="l", xlab="datetime", ylab="Volage")
    
    # Third Plot, Bottom Left 
    # with extra lines for the different graphs
    # Similar to plot 3
    plot(household_power$datetime, household_power$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
    lines(household_power$datetime, household_power$Sub_metering_1, type="l", col="black")
    lines(household_power$datetime, household_power$Sub_metering_2, type="l", col="red")
    lines(household_power$datetime, household_power$Sub_metering_3, type="l", col="blue")
    legend("topright", bty="n", col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=c(1, 1, 1), cex=0.8)
    
    # Fourth Plot, Bottom Right
    # Similar to plot 2
    plot(household_power$datetime, household_power$Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power")
    dev.off()

}