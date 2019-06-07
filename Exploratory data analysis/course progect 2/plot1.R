## Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#  Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

summaryPM25 <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")

yearly_emmissions <- aggregate(Emissions ~ year, summaryPM25, sum)

RColorBrewer::display.brewer.pal(n = 4, name = "RdYlGn")
jBrewColors <- RColorBrewer::brewer.pal(n = 4, name = "RdYlGn")

png("plot1.png", width=480, height=480)

barplot(yearly_emmissions$Emissions/100000, horiz = T,  density=50, angle = 90, names.arg = yearly_emmissions$year, xlab = "Total annual emissions (million tons)", main = "Total annual emissions PM 2.5 in USA", col = jBrewColors)

dev.off()
