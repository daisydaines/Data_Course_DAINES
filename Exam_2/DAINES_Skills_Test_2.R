#DAINES_Skills_Test_2
library(tidyverse)
df <- read.csv("./landdata-states.csv")
library(dplyr)
library(tidyr)
#I.
options(scipen = 999)

ggplot(df, aes(x=Year, y=Land.Value, color=region)) +
  geom_smooth() +
  labs(y="Land Value (USD)",
       color="Region") +
  theme_minimal()


jpeg("./DAINES_Fig_1.jpg")
ggplot(df, aes(x=Year, y=Land.Value, color=region)) +
  geom_smooth() +
  labs(y="Land Value (USD)",
       color="Region") +
  theme_minimal()
dev.off()

#II.
unique(df$region)
df.NA <- filter(df,is.na(region))
print(unique(df.NA$State))

  
#III. 
un <- read.csv("./unicef-u5mr.csv")

un2 <- un %>% 
  pivot_longer(U5MR.1950:U5MR.2015,
               names_to = "Year",
               values_to = "MortalityRate"
               ) %>% 
  arrange(CountryName) %>% 
  mutate(
    Year = as.integer(gsub("U5MR.","",Year))
    )


#IV. 
ggplot(un2, aes(x=Year, y= MortalityRate, color= Continent)) +
         geom_point() + 
  theme_minimal()

jpeg("./DAINES_Fig_2.jpg")
ggplot(un2, aes(x=Year, y= MortalityRate, color= Continent)) +
  geom_point() + 
  theme_minimal()
dev.off()


#V. 

un3 <- un2 %>% 
select(CountryName, Continent, Year, MortalityRate, Region) %>% 
  group_by(Continent, Year) %>% 
  mutate(Mean_MR = mean(MortalityRate, na.rm = TRUE))

?mean()
?mutate
?select()

ggplot(un3, aes(x=Year, y= Mean_MR, color= Continent)) +
   geom_line(size=2) + 
    theme_minimal()+ 
    labs(y= "Mean Mortality Rate (deaths per 1000 live births)")

jpeg("./DAINES_Fig_3.jpg")
ggplot(un3, aes(x=Year, y= Mean_MR, color= Continent)) +
  geom_line(size=2) + 
  theme_minimal()+ 
  labs(y= "Mean Mortality Rate (deaths per 1000 live births)")
dev.off()

#VI. 
ggplot(un3, aes(x=Year, y= MortalityRate/1000)) +
  geom_point(size= 0.5, alpha=0.5, color= "blue") + 
  facet_wrap(~Region) +
  labs(y= "Mortality Rate") +
  theme_minimal() + 
  theme(strip.background= element_rect(fill= "white", color= "black", size=0.75, linetype = "solid" ))


jpeg("./DAINES_Fig_4.jpg")
ggplot(un3, aes(x=Year, y= MortalityRate/1000)) +
  geom_point(size= 0.5, alpha=0.5, color= "blue") + 
  facet_wrap(~Region) +
  labs(y= "Mortality Rate") +
  theme_minimal() + 
  theme(strip.background= element_rect(fill= "white", color= "black", size=0.75, linetype = "solid" ))
dev.off()


