###########################
#                         #
#    Assignment Week 3    #
#                         # 
###########################

# Instructions ####
# Fill in this script with stuff that we do in class.
# It might be a good idea to include comments/notes as well so you remember things we talk about
# At the end of this script are some comments with blank space after them
# They are plain-text instructions for what you need to accomplish.
# Your task is to write the code that accomplished those tasks.

# Then, make sure to upload this to both Canvas and your GitHub repository




# Vector operations! ####

# Vectors are 1-dimensional series of values in some order
1:10 # ':' only works for integers
letters # built-in pre-made vector of a - z



vector1 <- c(1,2,3,4,5,6,7,8,9,10)
vector2 <- c(5,6,7,8,4,3,2,1,3,10)
vector3 <- letters

vector1 + 5
vector2 / 2
vector1*vector2
vector1+vector2

vector3 + 1 # can't add 1 to "a"



# Data Frames ####
# R has quite a few built-in data sets
data("iris") # load it like this

# For built-in data, there's often a 'help file'
?iris

# "Iris" is a 'data frame.' 
# Data frames are 2-dimensional (think Excel spreadsheet)
# Rows and columns
# Each row or column is a vector


dat <- iris # can rename the object to be easier to type if you want
identical(dat,iris)

# ways to get a peek at our data set
names(dat)
dim(dat)
head(dat)
head(dat,n=2)
?head

# You can access specific columns of a "data frame" by name using '$'
dat$Species
dim(dat$Sepal.Length)
dim(1:10)
?dim
length(dat$Sepal.Length)
Sepal.Area <- dat$Sepal.Length * dat$Sepal.Width
dat$Sepal.Area <- Sepal.Area
dat$NEWCOLUMN <- "oops"
dat$numbers <- 1:150


# You can also use square brackets to get specific 1-D or 2-D subsets of a data frame (rows and/or columns)
dat[1,1] # [Rows, Columns]
dat[1:3,5]
dat[1:3,1&5]
dat[1:3,c(1,5)]


# Plotting ####

# Can make a quick plot....just give vectors for x and y axes

?plot
plot(x=dat$Petal.Length, y=dat$Sepal.Length, col= dat$Species)

plot(x=dat$Species, y=dat$Sepal.Length)


# Object "Classes" ####

#check the classes of these vectors
class(dat$Petal.Length)
class(dat$Species)

# plot() function behaves differently depending on classes of objects given to it!

# Check all classes (for each column in dat)
str(dat)

# "Classes" of vectors can be changed if needed (you'll need to, for sure, at some point!)

# Let's try
nums <- c(1,1,2,2,2,2,3,3,3,4,4,4,4,4,4,4,5,6,7,8,9)
class(nums) # make sure it's numeric

# convert to a factor
as.factor(nums) # show in console
nums_factor <- as.factor(nums) #assign it to a new object as a factor
class(nums_factor) # check it

#check it out
plot(nums) 
plot(nums_factor)
# take note of how numeric vectors and factors behave differently in plot()

# Let's modify and save these plots. Why not!?
?plot()
plot(nums, main = "My Title", xlab = "My axis label", ylab = "My other axis label")


?jpeg()
jpeg("./Sepal_vs_Petal.jpg")
plot(x=dat$Sepal.Length,
     y=dat$Petal.Length,
     col=dat$Species,
     main = "My Title", 
     xlab = "My axis label", 
     ylab = "My other axis label")
dev.off()




# Making a data frame ####

# LET'S LEARN HOW TO MAKE A DATA FRAME FROM SCRATCH... WE JUST FEED IT VECTORS WITH NAMES!

# make some vectors *of equal length* (or you can pull these from existing vectors)
col1 <-  c("hat", "tie", "shoes", "bandana")
col2 <-  c(1,2,3,4)
col3 <-  factor(c(1,2,3,4)) # see how we can designate something as a factor             


# here's the data frame command:
data.frame(Clothes = col1, Numbers = col2, Factor_numbers = col3) # colname = vector, colname = vector....
df1 <-  data.frame(Clothes = col1, Numbers = col2, Factor_numbers = col3) # assign to df1
df1 # look at it...note column names are what we gave it.




# Practice subsetting ####

# Make a data frame from the first 20 rows of iris that has only Species and Sepal.Length columns
# save it into an object called "dat3"

dat3 <- dat[c(1:20),c(5,1)]
# WRITING OUT FILES FROM R ####
?write.csv()


# Write your new object "dat3" to a file named "LASTNAME_first_file.csv" in your PERSONAL git repository
write.csv(dat3, file= "DAINES_first_file.csv")

?levels
for(i in levels(dat$Species)){print(dat$Species)}
### for-loops in R ####

#simplest example:
for(i in 1:10){
  print(i)
}

#another easy one
for(i in levels(dat$Species)){
  print(i)
}
for(i in nrow(dat))
# can calculate something for each value of i ...can use to subset to groups of interest
for(i in levels(dat$Species)){
  print(mean(dat[dat$Species == i,"Sepal.Length"]))
}


iris$Species == "setosa"
3>4
3==3
3==2+1
"A" %in% c("A","B","C")
c("A","D") %in% c("A","B","C")
1:10>5

iris$Sepal.Length>5
myrows <- iris$Sepal.Length>=6 
iris[myrows,]
dim(iris[myrows,])
my_iris <- iris[myrows,]

table(my_iris$Species)


new_df <- read.csv("./setosa_and_virginica.csv")



#iris[iris$Species != "versicolor",]

newdat1 <- dat[c(1:50),c(1:5)]
newdat2 <- dat[c(101:150),c(1:5)]
newdat <- rbind(newdat1,newdat2)
newerdat <- rbind(dat[c(1:50),c(1:5)],dat[c(101:150),c(1:5)])

# YOUR REMAINING HOMEWORK ASSIGNMENT (Fill in with code) ####

# 1.  Make a scatterplot of Sepal.Length vs Sepal.Width. See if you can get the points to be colored by "Species"
plot(x=dat$Sepal.Length, y=dat$Sepal.Width, col=dat$Species)

# 2.  Write the code to save it (with meaningful labels) as a jpeg file
jpeg("./Sepal.Length_vs_Sepal.Width.jpg")
plot(x=dat$Sepal.Length,
     y=dat$Sepal.Width,
     col=dat$Species,
     main = "Sepal.Length_vs_Width", 
     xlab = "Sepal.Length", 
     ylab = "Sepal.Width")
dev.off()

# 3.  Subset the Iris data set to only include rows from the setosa and virginica Species
# CTRL ALT B -> runs all previous code in order

therowsiwant <- iris$Species %in% c("setosa","virginica") 
iris[therowsiwant,]

# 4.  Write code to save this new subset as a .csv file called setosa_and_virginica.csv
write.csv(newerdat, file= "./setosa_and_virginica.csv")

# 5.  Upload this R script (with all answers filled in and tasks completed) to canvas and GitHub
      # I should be able to run your R script and get all the plots created and saved, etc.
