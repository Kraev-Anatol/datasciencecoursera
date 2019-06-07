## Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
#  which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
#  Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

summaryPM25 <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")

BaltimorPM25 <- subset(summaryPM25, summaryPM25$fips == "24510")

BalTypePM25 <- aggregate(Emissions ~ year + type, BaltimorPM25, sum)

require(ggplot2)

png("plot3.png", width=480, height=480)

BaltGrafPm25 <- ggplot2::ggplot(BalTypePM25, aes( x = year, y = Emissions, color = type)) + 
        geom_point(size = 4) +
        geom_smooth(method = "loess", size = 2) +
        xlab("Year") +
        ylab(expression('Total Emissions')) + 
        ggtitle("Total Baltimore Emissions PM 2.5 From 1999 to 2008")
print(BaltGrafPm25)

dev.off()
