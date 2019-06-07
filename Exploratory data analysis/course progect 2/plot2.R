## Have total emissions from PM2.5 decreased in the Baltimore City, Maryland fips=="24510") from 1999 to 2008?
#  Use the base plotting system to make a plot answering this question.


summaryPM25 <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")

BaltimorPM25 <- subset(summaryPM25, summaryPM25$fips == "24510")

Baltimor_emmissions <- aggregate(Emissions ~ year, BaltimorPM25, sum)

colors <- c("red", "aquamarine", "black", "chartreuse")

png("plot2.png", width=480, height=480)

barplot(Baltimor_emmissions$Emissions/1000, names.arg = Baltimor_emmissions$year, ylab = "Baltimore annual emissions (thousand tons)", main = "Baltimore annual emissions PM 2.5", col = colors)

dev.off()
