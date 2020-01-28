##Assignment question 5: How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City

library(ggplot2)

## Read PM 2.5 emissions data. 
NEI_assignment5 <- readRDS("summarySCC_PM25.rds")

# read source file which provides a mapping from the SCC digit strings in the Emissions table to the actual name of the PM2.5 source. 
SCC <- readRDS("Source_Classification_Code.rds")

#filter out dataset for baltimore city
BaltimoreDataset <- subset(NEI_assignment5, fips=='24510')

#get dataset for vehicle related sources
VehicleDataset <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
#get SSC data for vehicle related sources
SCCVehicleDataset <- SCC[VehicleDataset,]$SCC
#get NEI data based on SSC dataset for vehicle related sources
NEIVehicleDataset <- BaltimoreDataset[BaltimoreDataset$SCC %in% SCCVehicleDataset,]

#draw graphics
png("plot5.png", width=480, height=480)

ggplot(NEIVehicleDataset,aes(factor(year),Emissions)) +
  geom_bar(stat="identity",fill="blue",width=0.5) +
  theme_bw()  + guides(fill=FALSE) +
  labs(x="Year", y=expression("Total PM"[2.5]*" Emission in Kilotons")) + 
  labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore from 1999-2008"))

dev.off()
