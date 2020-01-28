
##Assignment Question 2:Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008

## Read PM 2.5 emissions data.
NEI_assignment2 <- readRDS("summarySCC_PM25.rds")

## filter data for baltimore,maryland
BaltimoreDataset <- subset(NEI_assignment2, fips=='24510')

## use aggregate function to collapse data
BaltimoreEmission <- aggregate(BaltimoreDataset[, 'Emissions'], by=list(BaltimoreDataset$year), FUN=sum)

#draw graph
png("plot2.png", width=480, height=480)

barplot(BaltimoreEmission$x, names.arg=BaltimoreEmission$Group.1, 
        main=expression('Total PM'[2.5]*' Emissions of Baltimore City, Maryland For Given Years'),
        xlab='Year', ylab=expression('Total PM'[2.5]*' emissions in Kilotons'),col = c("blue", "gray","green","orange"))

dev.off()