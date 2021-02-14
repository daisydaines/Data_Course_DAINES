# Assignment 6
library(tidyverse)
data(mpg)
mpg

ggplot(mpg, aes(x=displ, y=hwy, color=factor(cyl))) + 
  geom_point(alpha=0.25) +
  geom_smooth(method="lm") +
  facet_wrap(~year)

#evolution of a plot (mpg)


