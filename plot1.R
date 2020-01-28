
##Assignment Question 1:Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?  
##                      Plot a graph showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.


## Read PM 2.5 emissions data. 
NEI <- readRDS("summarySCC_PM25.rds")

## using aggregate function collapse data
emission <- aggregate(NEI[, 'Emissions'], by=list(NEI$year), FUN=sum)
#convert PM in kilotons as it is difficult to represent these numbers on graph axes
emission$PMKiloton <- round(emission[,2]/1000,2)

#draw graphics
png("plot1.png", width=480, height=480)

barplot(emission$PMKiloton, names.arg=emission$Group.1, 
        main=expression('Total PM'[2.5]*' Emissions For Given Years'),
        xlab='Year', ylab=expression('Total PM'[2.5]*' emissions in Kilotons'), col = c("blue", "gray","green","black"))

dev.off()