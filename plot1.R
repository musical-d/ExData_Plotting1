plot1 <- function() {
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
    
    # Plot the histogram and save in a PNG file
    png(filename="plot1.png", width=480, height=480)
    par(mar = c(5,5,5,5))
    hist(household_power$Global_active_power, xlab="Global Active Power (kilowatts)", ylab="Frequency", freq=TRUE, col = "red", main = "Global Active Power")
    dev.off()

}