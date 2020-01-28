##Assignment question 6: Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
##Which city has seen greater changes over time in motor vehicle emissions

library(ggplot2)

## Read PM 2.5 emissions data.
NEI_assignment6 <- readRDS("summarySCC_PM25.rds")

# read source file which provides a mapping from the SCC digit strings in the Emissions table to the actual name of the PM2.5 source. 
SCC <- readRDS("Source_Classification_Code.rds")


VehicleDataset <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
SCCVehicleDataset <- SCC[VehicleDataset,]$SCC
NEIVehicleDataset <- NEI_assignment6[NEI_assignment6$SCC %in% SCCVehicleDataset,]

BaltimoreNEI <- NEIVehicleDataset[NEIVehicleDataset$fips == '24510',]
BaltimoreNEI$city <- "Baltimore City"
LANEI <- NEIVehicleDataset[NEIVehicleDataset$fips=="06037",]
LANEI$city <- "Los Angeles County"
BaltimoreLANEI <- rbind(BaltimoreNEI,LANEI)

#draw graphics
png("plot6.png", width=480, height=480)


 ggplot(BaltimoreLANEI, aes(x=factor(year), y=Emissions, fill=city)) +
  geom_bar(aes(fill=year),stat="identity") +
  facet_grid(scales="free",  .~city) +
  guides(fill=FALSE) +
  labs(x="Year", y=expression("Total PM"[2.5]*" Emission")) + 
  labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions Comparison in Baltimore & LA, 1999-2008"))

 dev.off()