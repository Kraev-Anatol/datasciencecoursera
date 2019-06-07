## Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California fips=="06037"). 
#  Which city has seen greater changes over time in motor vehicle emissions?

summaryPM25 <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")

BaltimorPM25OnRoad <- subset(summaryPM25, summaryPM25$fips == "24510" & summaryPM25$type =="ON-ROAD")
LosangPM25OnRoad <- subset(summaryPM25, summaryPM25$fips == "06037" & summaryPM25$type =="ON-ROAD")

LosangMotorEmission <- aggregate(Emissions ~ year, LosangPM25OnRoad, sum)
BaltMotorEmission <- aggregate(Emissions ~ year, BaltimorPM25OnRoad, sum)

LosangMotorEmission$Region <- "Los Angeles County"
BaltMotorEmission$Region <- "Baltimore City"
GenEmission <- rbind(BaltMotorEmission, LosangMotorEmission)


require(ggplot2)
png("plot6.png", width=480, height=480)

g <- ggplot(GenEmission, aes(x=factor(year), y=Emissions, fill= Region,label = round(Emissions,2))) +
        geom_bar(stat="identity") + 
        facet_wrap(~ Region, nrow = 2) +
        ylab(expression("total PM"[2.5]*" emissions in tons")) + 
        xlab("Year") +
        ggtitle(expression("Motor vehicle emission variation in Baltimore and Los Angeles in tons"))+
        geom_label(aes(fill = Region),colour = "white", fontface = "bold")
print(g)

dev.off()
