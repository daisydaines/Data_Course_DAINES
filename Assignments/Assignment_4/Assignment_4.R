# Assignment_4.R
?read.table()
df <- read.csv("../../Data/ITS_mapping.csv", sep = "\t") 
?read.csv
summary(df)
summary(df$Collection_Date)
dim(df)
str(df)
plot(x=factor(df$Ecosystem), y=df$Lat,
     main="silly_boxplot",
     xlab="ecosystem",
     ylab="latitude",
     col="yellow")
png(filename = "./silly_boxplot.png")
plot(x=factor(df$Ecosystem), y=df$Lat,
     main="silly_boxplot",
     xlab="ecosystem",
     ylab="latitude",
     col="yellow")
dev.off()