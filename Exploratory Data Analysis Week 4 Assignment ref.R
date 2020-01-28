NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# calculate the sum of all emissions by year

emissionbyyear <- with(NEI, tapply(Emissions, year, sum))

# open PNG graphics device

png(filename = 'plot1.png')


# Create a bar plot of total emissions

barplot(emissionbyyear/1000000, col = 'blue', xlab = "year", 
        ylab = "Amount of PM2.5 emitted (mln tons)",
        main = "Total emissions from PM2.5 in the United States" )

# close the graphics device

dev.off()


# Read the data (assuming the unzipped files are located in the working directory)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Extract the Baltimore data

BaltimoreNEI <- NEI[NEI$fips == "24510",]

# calculate the sum of all emissions by year

Baltiemissionbyyear <- with(BaltimoreNEI, tapply(Emissions, year, sum))

# open PNG graphics device

png(filename = 'plot2.png')


# Create a bar plot of total emissions for Baltimore. Store it as an object 
# to get access to x axis coordinates

myplot.bar <- barplot(Baltiemissionbyyear, col = 'darkorange', xlab = "year", 
                      ylab = "Amount of PM2.5 emitted (tons)",
                      ylim = c(0,3500),
                      main = "Total emissions from PM2.5 in Baltimore City, Maryland")

# Create a linear model for the data, using the x coordinates of the bar plot
# rather than the year	

baltimodel <- lm(Baltiemissionbyyear ~ myplot.bar)

# Add the linear model as a trendline to the plot

abline(baltimodel, lwd = 2)

# Add a legend for the trendline

legend("topright", lwd = 2, legend = "Trend")

# close the graphics device

dev.off()


NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

BaltimoreNEI <- NEI[NEI$fips == "24510",]

# Load the ggplot2 package
library(ggplot2)


# Calculate the sum of Baltimore Emissions by type and year

BaltimoreNEIsum <- aggregate(Emissions ~ type + year, data = BaltimoreNEI, FUN = sum)


# Pass the data to ggplot 2

g <- ggplot(BaltimoreNEIsum, aes(year, Emissions))

# Create a point geometry. Add a linear smoother with no confidence intervals
g + geom_point() + geom_smooth(method = "lm", se = FALSE) +
  # Change the label of the y axis and the plot title
  labs( y = "Total Emissions (tons)",
        title = "Emissions by source in Baltimore City, Maryland") +
  # specify type as the column facet
  facet_grid(. ~ type) +
  # modify the x-axis such that it displays the years with data as tick marks
  # limits extended to prevent the plot from overlying
  
  scale_x_continuous(limits = c(1998,2009), breaks = c(1999,2002,2005,2008), labels = c("1999",
                                                                                        "2002", "2005", "2008"))

# save the plot to a png file

ggsave("plot3.png")


NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Extract a list of coal combustion-related sectors

coalcombustsector <- unique(grep("Coal",
                                 grep("Fuel Comb", SCC$EI.Sector, value = TRUE), value = TRUE))


# create a vector list of SCC codes for coal combustion-related sources	

coalcombustcodes <- SCC$SCC[SCC$EI.Sector %in% coalcombustgroup]

# select NEI data for coal combustion-related sources only

coalcombustdata <- NEI[NEI$SCC %in% coalcombustcodes,]

# calculate the sum of Emissions for coal combustion-related sources by year

coalcombustbyyear <- with(coalcombustdata, tapply(Emissions, year, sum))

# open PNG graphics device

png(filename = 'plot4.png')


# Create a bar plot of total emissions

barplot(coalcombustbyyear/1000, col = 'blue', xlab = "year", 
        ylab = "Amount of PM2.5 emitted (kilotons)",
        main = "Emissions from coal combustion-related sources" )

# close the graphics device

dev.off()

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Extract the Baltimore data

BaltimoreNEI <- NEI[NEI$fips == "24510",]

# Extract the data for on-road sources.
# This is assumed to represent emissions from motor vehicle sources 

baltimoreonroad <- BaltimoreNEI[BaltimoreNEI$type == "ON-ROAD",]

# calculate the sum of Emissions for motor vehicle sources by year

baltimoreonroadbyyear <- with(baltimoreonroad, tapply(Emissions, year, sum))

# open PNG graphics device

png(filename = 'plot5.png')


# Create a bar plot of total emissions

barplot(baltimoreonroadbyyear, col = 'blue', xlab = "year", 
        ylab = "Amount of PM2.5 emitted (tons)",
        main = "Emissions from motor vehicle sources in Baltimore, Maryland" )

# close the graphics device

dev.off()

# Read the data (assuming the unzipped files are located in the working directory)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Extract data for Baltimore and Los Angeles

BaltiLANEI <- NEI[NEI$fips == "24510" | NEI$fips == "06037",]

# Extract the data for on-road sources.
# This is assumed to represent emissions from motor vehicle sources 

baltiLAonroad <- BaltiLANEI[BaltiLANEI$type == "ON-ROAD",]

# Change the fips numbers to the actual location for easy plotting with ggplot2

baltiLAonroad$fips[baltiLAonroad$fips == "24510"] <- "Baltimore City, Maryland"

baltiLAonroad$fips[baltiLAonroad$fips == "06037"] <- "Los Angeles County, California"

# aggregate the sum of Emissions for motor vehicle sources by year

baltiLAonroadsum <- aggregate(Emissions ~ fips + year, data = baltiLAonroad, FUN = sum)

# load the ggplot2 package

library(ggplot2)

g <- ggplot(baltiLAonroadsum, aes(year, Emissions))

# Create a column geometry. Specify the fips(location) as the column facet
g + geom_col(fill = "seagreen") + facet_grid(. ~ fips) +
  # Change the y axis label and add a plot title
  labs(y = "Total Emissions (tons)",
       title = "Comparison of emissions from motor vehicle sources") +
  # modify the x-axis such that it displays the years with data as tick marks
  scale_x_continuous(breaks = c(1999,2002,2005,2008), labels = c("1999",
                                                                 "2002", "2005", "2008"))

# save the graphic as a png image

ggsave("plot6.png")