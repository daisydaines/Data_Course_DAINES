# DAINES_ggplot.R
data(iris)
iris
#chart1
png(filename = "./iris_fig1.png")
ggplot(iris)

pl <- ggplot(iris) 

pl <- ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, color= Species))

pl <- pl + geom_point()

pl <- pl + geom_smooth(method = "lm")

pl <- pl + theme_minimal()

pl <- pl + labs(title="Sepal length vs petal length", 
          subtitle="for three iris species", 
          y="Petal.Length", 
          x="Sepal.Length")
pl

dev.off()

#chart2
png(filename = "./iris_fig2.png")
ggplot(iris)

pl2 <- ggplot(iris)

pl2 <- ggplot(iris, aes(x=Petal.Width, fill= Species))

pl2 <- pl2 + geom_density(alpha=0.5)

pl2 <- pl2 + theme_minimal()

pl2 <- pl2 + labs(title="Distribution of Petal Widths", 
                subtitle="for three iris species", 
                y="density", 
                x="Petal Width")
pl2

dev.off()
#chart3
png(filename = "./iris_fig3.png")
ggplot(iris)

pl3 <- ggplot(iris)

pl3 <- ggplot(iris, aes(x=Species, y= Petal.Width/Sepal.Width, fill= Species))

pl3 <- pl3 + geom_boxplot()

pl3 <- pl3 + theme_minimal()

pl3 <- pl3 + labs(title="Sepal- to Petal-Width Ratio", 
                  subtitle="for three iris species", 
                  y="Ratio of Sepal Width to Petal Width", 
                  x="Species")

pl3
dev.off()
#chart4
png(filename = "./iris_fig4.png")
iris$num <- rownames(iris)
iris$deviance <- iris$Sepal.Length - mean(iris$Sepal.Length)
iris <- iris[order(iris$deviance),]
iris$num <- factor(iris$num,levels=iris$num)

pl4 <- ggplot(iris, aes(x=num, y=deviance)) +
  geom_bar(stat='identity', aes(fill=Species), width=0.9)  +
  labs(title= "Sepal length deviance from the mean of all observations",
       y = "Deviance from the Mean",
       x = "",
       caption= "Note: Deviance = Sepal.Length -mean(Sepal.Length)") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y=element_blank())

print(pl4)

dev.off()


library(ggplot2)
theme_set(theme_bw())  

# Data Prep
data("mtcars")  # load data
mtcars$`car name` <- rownames(mtcars)  # create new column for car names
mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)  # compute normalized mpg
mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")  # above / below avg flag
mtcars <- mtcars[order(mtcars$mpg_z), ]  # sort
mtcars$`car name` <- factor(mtcars$`car name`, levels = mtcars$`car name`)  # convert to factor to retain sorted order in plot.

# Diverging Barcharts
ggplot(mtcars, aes(x=`car name`, y=mpg_z, label=mpg_z)) + 
  geom_bar(stat='identity', aes(fill=mpg_type), width=.5)  +
  scale_fill_manual(name="Mileage", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised mileage from 'mtcars'", 
       title= "Diverging Bars") + 
  coord_flip()

####
iris$num <- rownames(iris)
iris$deviance <- iris$Sepal.Length - mean(iris$Sepal.Length)
iris <- iris[order(iris$deviance),]
iris$num <- factor(iris$num,levels=iris$num)

pl5 <- ggplot(iris, aes(x=num, y=deviance)) +
  geom_bar(stat='identity', aes(fill=Species), width=0.9)  +
  labs(title= "Sepal length deviance from the mean of all observations",
       y = "Deviance from the Mean",
       x = "",
       caption= "Note: Deviance = Sepal.Length -mean(Sepal.Length)") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y=element_blank())

print(pl5)

# create a new column using the row names
iris$num <- rownames(iris)
# create a new column deviance
iris$deviance <- iris$Sepal.Length - mean(iris$Sepal.Length)
# sort data frame by deviance
iris <- iris[order(iris$deviance), ]
# factor the num column using row number of levels
iris$num <- factor(iris$num,levels=iris$num)

# create bar chart
# plot deviance vs. row
# unique bar per row, colored by Species, small gap between bars
# title, y-axis, x-axis blank, include caption
# flip char to be horizontal
# use theme minimal
# prevent x-axis labels from printing
pl5 <- ggplot(iris, aes(x=num, y=deviance)) +
  geom_bar(stat='identity', aes(fill=Species), width=0.9)  +
  labs(title= "Sepal length deviance from the mean of all observations",
       y = "Deviance from the Mean",
       x = "",
       caption = "Note: Deviance = Sepal.Length - mean(Sepal.Length)") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y=element_blank())

print(pl5)
