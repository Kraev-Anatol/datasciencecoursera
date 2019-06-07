## Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

SourceCCode <- readRDS("exdata_data_NEI_data/Source_Classification_Code.rds")
summaryPM25 <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")

MergerData <- merge(summaryPM25, SourceCCode, by = "SCC")

CoalBin <- grepl("coal", MergerData$Short.Name, ignore.case = TRUE)

subsetMergerData <- MergerData[CoalBin,]

subsetMDByYear <- aggregate(Emissions ~ year, subsetMergerData, sum)

require(ggplot2)

png("plot4.png", width=480, height=480)

g <- ggplot(subsetMDByYear, aes(factor(year), Emissions/1000, fill = year, label = round(Emissions/1000,2))) +
     geom_bar(stat="identity", width=0.7) +
     geom_label(aes(fill = year),colour = "white", fontface = "bold") +
     xlab("Year") +
     ylab(expression('Total PM'[2.5]*" Emissions (thousands of tons)")) +
     ggtitle('Total Emissions from coal sources USA 1999 to 2008')          

print(g)

dev.off()
