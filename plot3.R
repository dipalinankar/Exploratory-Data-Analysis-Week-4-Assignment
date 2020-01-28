##Assignment Question 3:Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? Which have seen increases in emissions from 1999-2008

library(ggplot2)

## Read PM 2.5 emissions data. This file contains a data frame with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008.
##For each year, the table contains number of tons of PM2.5 emitted from a specific type of source for the entire year. 
NEI_assignment3 <- readRDS("summarySCC_PM25.rds")

## filter data for baltimore,maryland
BaltimoreDataset <- subset(NEI_assignment3, fips=='24510')

#draw graphics
png("plot3.png", width=480, height=480)

ggplot(BaltimoreDataset,aes(factor(year),Emissions,fill=type)) +
  geom_bar(stat="identity") +
  theme_bw() + guides(fill=FALSE)+
  facet_grid(.~type,scales = "free",space="free") + 
  labs(x="Year", y=expression("Total PM"[2.5]*" Emission in Tons")) + 
  labs(title=expression("PM"[2.5]*" Emissions, Baltimore City 1999-2008 by Source Type"))

dev.off()