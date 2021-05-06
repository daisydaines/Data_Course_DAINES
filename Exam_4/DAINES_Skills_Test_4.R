#Exam 4 (retake of exam 3)
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

pl1 <- ggplot(fs4, aes(y=Salary,x= Rank)) +
  geom_boxplot(aes(fill=Rank)) +
  facet_wrap(~Tier) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 55))

pl1
# really need to use ggsave rather than jpeg() - Zahn
ggsave("./DAINES_Fig_1.jpg", plot = pl1)


#II. 

# factor State before running ANOVA
fs4$State <- as_factor(fs4$State)

# factor State before running ANOVA
fs4$Rank <- as_factor(fs4$Rank)

# create linear model
model <- lm(Salary ~ State + Tier + Rank, data=fs4)

# create sum of squares table using type 3, common in social science
sstable <- aov(model, type = 3)

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
pl2 <- ggplot(jo2, aes(x = YearsSinceBurn, y = Concentration)) + 
  facet_wrap(~ChemicalID, scales = "free") +
  geom_smooth(size=1.1) +
  theme_minimal()

pl2
# really need to use ggsave rather than jpeg() - Zahn
ggsave("./DAINES_Fig_2.jpg", plot = pl2)

#V. didn't parse model output with broom package - Zahn
modJO <- glm(data = jo2, Concentration ~ ChemicalID * YearsSinceBurn)

tidy(modJO) %>%
  filter(p.value <= 0.05) %>%
  mutate(term = term %>% str_remove_all("ChemicalID"))

#VI. 
# See DAINES_Skills_Test_4.Rmd