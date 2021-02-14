#assignment 6
library(tidyverse)
library(patchwork)
#1. loads mtcars df
data("mtcars")
str(mtcars)
?mtcars
#2. subsets automatic transmissions
mtcars$am<1
a.t. <- mtcars$am<1
mtcars[a.t.,]
mtcars_auto <- mtcars[a.t.,]
#3. saves new file called "automatic_mtcars.csv"
write.csv(mtcars_auto, file = "./automatic_mtcars.csv")
#4. plots the effect on horsepower on mpg using ggplot2
ggplot(mtcars_auto, 
       aes(x=hp, y=mpg))+
         geom_point()+
  labs(title = "Effect of horsepower on miles-per-gallon",
       subtitle= "from mtcars dataset",
       x= "horsepower", 
       y= "miles-per-gallon")
#5. saves plot as png called "mpg_vs_hp_auto.png" 
png(filename= "./mpg_vs_hp_auto.png")
hp_vs_mpg <- ggplot(mtcars_auto, 
       aes(x=hp, y=mpg))+
  geom_point()+
  labs(title = "Effect of horsepower on miles-per-gallon",
       subtitle= "from mtcars dataset",
       x= "horsepower", 
       y= "miles-per-gallon")
hp_vs_mpg
dev.off()
#6 plot effect of weight on mpg 
weight_vs_mpg <- ggplot(mtcars_auto, 
                        aes(x=wt, y=mpg))+
  geom_point()+ 
  labs(title ="Effect of weight on miles-per-gallon",
       subtitle= "from mtcars dataset", 
       x= "weight", 
       y= "miles-per-gallon")
weight_vs_mpg
#7 saves this second plot as tiff called "mpg_vs_wt_auto.tiff" 
png(filename= "./mpg_vs_wt_auto.tiff")
weight_vs_mpg <- ggplot(mtcars_auto, 
                        aes(x=wt, y=mpg))+
  geom_point()+ 
  labs(title ="Effect of weight on miles-per-gallon",
       subtitle= "from mtcars dataset", 
       x= "weight", 
       y= "miles-per-gallon")
weight_vs_mpg
dev.off()
#8 subset original mtcars df to cars w/ disp </= to 200 cu.in. 
mtcars$disp<=200
max200 <- mtcars$disp<=200
mtcars[max200,]
mtcars_sub200 <- mtcars[max200,]
#9 saves that new subset as a csv file called mtcars_max200_disp.csv
write.csv(mtcars_sub200, file= "./mtcars_max200_disp.csv")
#10 code to calculate the maximum horsepower for each of the three dfs 
max_mtcars <- max(mtcars$hp)
max_mtcars_auto <- max(mtcars_auto$hp)
max_mtcars_sub200 <- max(mtcars_sub200$hp)
#11 prints these calculations in a readable format to new plain text file called "hp_maximums.txt"
sink("./hp_maximums.txt")
cat("===================================================\n")
cat("== Maximum Horsepower                            ==\n")
cat("===================================================\n")
cat("mtcars data set:                             : ",max_mtcars,"\n")
cat("mtcars (automatic transmission only) data set: ",max_mtcars_auto,"\n")
cat("mtcars (<= 200 cu in displacement) data set  : ",max_mtcars_sub200,"\n")
sink()

#12 combines the following 3 plots into one image using patchwork package (use full original df)
#Scatterplot + trendline of the effect of weight on mpg (points and linear trendlines colored by the number of cylinders)
mtcars$cyl <- as.factor(mtcars$cyl)
p1 <- ggplot(mtcars, aes(x=wt, y=mpg, color= cyl)) +
  geom_point(aes(color=cyl)) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Weight effect on Miles per Gallon", 
       subtitle = "from mtcars dataset",
       y= "Miles per Gallon",
       x= "weight (tons)")
p1

#Violin chart of the distributions of mpg for cars, separated and colored by the number of cylinders
mtcars$cyl <- as.factor(mtcars$cyl)
p2 <- ggplot(mtcars, aes(x=cyl, y=mpg, color= cyl)) +
  geom_violin(aes(color=cyl)) +
  theme_minimal() +
  labs(title = "Distribution of Miles per Gallon",
       subtitle = "from mtcars dataset",
       x= "Number of Cylinders",
       y= "Miles per Gallon")
p2
  
#Scatterplot + trendline of the effect of horsepower on mpg (points and linear trendlines colored by the number of cylinders)
p3 <- ggplot(mtcars, aes(x=hp, y=mpg, color= cyl)) + 
  geom_point() +
  geom_smooth(method= "lm") +
  theme_minimal() +
  labs(title = "Horsepower effect on Miles per Gallon", 
       subtitle = "from mtcars dataset",
       y= "Miles per Gallon",
       x= "horsepower")
p3

p2 + p1/p3

#13 saves combined figure as a single png image called "combined_mtcars_plot.png"
png(filename = "./combined_mtcars_plot.png")
p2 + p1/p3
dev.off()
