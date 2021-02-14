#Daines Exam 1
library(tidyverse)
read.csv("DNA_Conc_by_Extraction_Date.csv")
df <- DNA_Conc_by_Extraction_Date
#1
pBen <- ggplot(df, aes(x= DNA_Concentration_Ben, y=)) +
  geom_histogram() +
  theme_minimal() +
  labs(title = "DNA Concentrations- Ben",
       subtitle = "from 'DNA_Conc_by_Extraction' dataset",
       x= "DNA Concentrations")
pBen
  
pKaty <- ggplot(df, aes(x= DNA_Concentration_Katy, y=)) +
  geom_histogram() +
  theme_minimal() +
  labs(title = "DNA Concentrations- Katy",
       subtitle = "from 'DNA_Conc_by_Extraction' dataset",
       x= "DNA Concentrations")

pKaty

#2
str(df)
boxplot(df$DNA_Concentration_Katy~df$Year_Collected, 
col="white",
main = "Katy's Extractions", 
xlab = "YEAR", 
ylab = "DNA Concentration")

boxplot(df$DNA_Concentration_Ben~df$Year_Collected, 
        col="white",
        main = "Ben's Extractions", 
        xlab = "YEAR", 
        ylab = "DNA Concentration")

#3: save the plots
jpeg("./DAINES_Plot1.jpeg")
boxplot(df$DNA_Concentration_Katy~df$Year_Collected, 
        col="white",
        main = "Katy's Extractions", 
        xlab = "YEAR", 
        ylab = "DNA Concentration")
dev.off()

jpeg("./DAINES_Plot2.jpeg")
boxplot(df$DNA_Concentration_Ben~df$Year_Collected, 
        col="white",
        main = "Ben's Extractions", 
        xlab = "YEAR", 
        ylab = "DNA Concentration")
dev.off()

#4
# dplyr:: group_by %>% summarize
meanBen <- df %>% group_by(Year_Collected) %>% summarize(mean(DNA_Concentration_Ben))
names(meanBen)[2] <- 'Mean.Ben'
print(meanBen)

# dplyr:: group_by %>% summarize
meanKaty <- df %>% group_by(Year_Collected) %>% summarize(mean(DNA_Concentration_Katy))
names(meanKaty)[2] <- 'Mean.Katy'
print(meanKaty)

DNAdiff <- cbind(meanBen,meanKaty[2])
names(DNAdiff)[2] <- 'Mean.Ben'
DNAdiff$diff <- DNAdiff$Mean.Ben - DNAdiff$Mean.Katy
DNAdiff <- DNAdiff %>% arrange(desc(diff))
lowest_year <- DNAdiff[12,1]

#5 just downstairs data
d.s.data <- DNA_Conc_by_Extraction_Date$Lab == "Downstairs"
down_data <- DNA_Conc_by_Extraction_Date[d.s.data,]
plot(x=down_data$Date_Collected,
     y=down_data$DNA_Concentration_Ben,
     main = "", 
     xlab = "Date_Collected", 
     ylab = "DNA_Concentration_Ben")


jpeg("./Ben_DNA_over_time.jpg")
plot(x=down_data$Date_Collected,
     y=down_data$DNA_Concentration_Ben,
     main = "", 
     xlab = "Date_Collected", 
     ylab = "DNA_Concentration_Ben")
dev.off()

#6 Bonus
meanBen <- df %>% group_by(Year_Collected) %>% summarize(mean(DNA_Concentration_Ben))
names(meanBen)[2] <- 'Mean'
head(meanBen %>% arrange(desc(Mean)),1)

write.csv(meanBen, file= "Ben_Average_Conc.csv")
