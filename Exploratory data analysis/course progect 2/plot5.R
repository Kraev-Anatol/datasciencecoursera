## How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

summaryPM25 <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")

BaltimorPM25OnRoad <- subset(summaryPM25, summaryPM25$fips == "24510" & summaryPM25$type =="ON-ROAD")

BaltMotorEmission <- aggregate(Emissions ~ year, BaltimorPM25OnRoad, sum)


require(ggplot2)
png("plot5.png", width=480, height=480)

g <- ggplot(BaltMotorEmission, aes(x=factor(year), y=Emissions,fill=year, label = round(Emissions,2))) +
        geom_bar(stat="identity", width=0.75) +
        xlab("Year") +
        ylab(expression("total PM"[2.5]*" emissions (tons)")) +
        ggtitle("Emissions from motor vehicle sources in Baltimore")+
        geom_label(aes(fill = year),colour = "yellow", fontface = "bold")
print(g)

dev.off()
