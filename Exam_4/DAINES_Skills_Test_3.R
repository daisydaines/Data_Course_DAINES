library(tidyverse)
library(janitor)
library(modelr)
library(broom)

#I.
fs <- read_csv("./FacultySalaries_1995.csv")
glimpse(fs)
names(fs)

fs2 <- select(fs, -c(AvgProfSalaryAll,  AvgFullProfComp,     
                     AvgAssocProfComp,  AvgAssistProfComp, AvgProfCompAll,     
                     NumFullProfs,      NumAssocProfs,       
                     NumAssistProfs,    NumInstructors,     NumFacultyAll))     

fs3 <- fs2 %>% 
  pivot_longer(
    cols = starts_with("Avg"),
    names_to = "Rank",
    names_prefix = "Avg",
    values_to = "Salary")  
fs4 <- fs3 %>% mutate(Rank = as.character(gsub("ProfSalary", "", fs3$Rank)))

fs4 <- fs4[!(fs4$Tier=="VIIB"),]
class(fs4$Tier)
as.factor(fs4$Tier)

ggplot(fs4, aes(y=Salary,x= Rank)) +
  geom_boxplot(aes(fill=Rank)) +
  facet_wrap(~Tier) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 55))

jpeg("./DAINES_Fig_1.jpg")
ggplot(fs4, aes(y=Salary,x= Rank)) +
  geom_boxplot(aes(fill=Rank)) +
  facet_wrap(~Tier) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 55))
dev.off()

#II. 

# factor State before running ANOVA
df3$State <- as_factor(df3$State)

# factor State before running ANOVA
df3$Rank <- as_factor(df3$Rank)

# create linear model
model <- lm(Salary ~ State + Tier + Rank, data=df3)

# create sum of squares table using type 3, common in social science
sstable <- Anova(model, type = 3)

sstable

capture.output(summary(sstable), file="Salary_ANOVA_Summary.txt")

#III.
jo <- read_csv("./Juniper_Oils.csv")
jo1 <- jo %>% 
  select(c("alpha-pinene","para-cymene","alpha-terpineol","cedr-9-ene","alpha-cedrene",
           "beta-cedrene","cis-thujopsene","alpha-himachalene","beta-chamigrene","cuparene",
           "compound 1","alpha-chamigrene","widdrol","cedrol","beta-acorenol","alpha-acorenol",
           "gamma-eudesmol","beta-eudesmol","alpha-eudesmol","cedr-8-en-13-ol","cedr-8-en-15-ol",
           "compound 2","thujopsenal","YearsSinceBurn"))

jo2 <- jo1 %>% 
  pivot_longer(cols= "alpha-pinene":"thujopsenal",  
    names_to = "ChemicalID",
    values_to = "Concentration") 

#IV. 
ggplot(jo2, aes(x = YearsSinceBurn, y = Concentration)) + 
  facet_wrap(~ChemicalID, scales = "free") +
  geom_smooth(size=1.1) +
  theme_minimal()

jpeg("./DAINES_Fig_2.jpg")
ggplot(jo2, aes(x = YearsSinceBurn, y = Concentration)) + 
  facet_wrap(~ChemicalID, scales = "free") +
  geom_smooth(size=1.1) +
  theme_minimal()
dev.off()

#V. 

modJO <- glm(data = jo2, 
    formula = Concentration ~ ChemicalID * YearsSinceBurn )

tibble <-  tidy(modJO) %>% filter(p.value < 0.05) %>% mutate(term = as.character(gsub("ChemicalID","",tibble$term)))
print(tibble)
#VI. 
