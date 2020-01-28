
##Assignment question 4:Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008

library(ggplot2)

## Read PM 2.5 emissions data. 
NEI_assignment4 <- readRDS("summarySCC_PM25.rds")

# read source file which provides a mapping from the SCC digit strings in the Emissions table to the actual name of the PM2.5 source. 
SCC <- readRDS("Source_Classification_Code.rds")

#get dataset for combustion related sources,coal
CombustionDataset <- grepl("comb", SCC$SCC.Level.One, ignore.case=TRUE)
CoalDataset <- grepl("coal", SCC$SCC.Level.Four, ignore.case=TRUE) 
#combine data from dataset and get only coal combustion-related sources 
CombinedDataset <- (CombustionDataset & CoalDataset)
#get SSC data for 
CombustionSCC <- SCC[CombinedDataset,]$SCC
#get NEI data for above dataset
NEICombustionData <- NEI_assignment4[NEI_assignment4$SCC %in% CombustionSCC,]

#draw graphics
png("plot4.png", width=480, height=480)

ggplot(NEICombustionData,aes(factor(year),round(Emissions/1000,2))) +
  geom_bar(stat="identity",fill="green",width=0.50) +  guides(fill=TRUE) +
  labs(x="Year", y=expression("Total PM"[2.5]*" Emission in Kilotons")) + 
  labs(title=expression("PM"[2.5]*" Coal Combustion Source Emissions Across US from 1999-2008"))

dev.off()