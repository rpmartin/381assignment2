# Anything following a hashtag (#) is a comment, and is ignored. This is the file
# where you save your R code that produces the objects (graphs, tables) that you 
# want to embed into  your assignment.  Make sure that there are no errors when 
# you source this file before sourcing it from assignment1.Rmd file.
# To show you how it is done I have included some code below.
#####################################
library("tidyverse") # if tidyverse not found, install.packages("tidyverse") 
library("plotly") #if plotly not found, install.packages("plotly") 
library("here")
here::i_am("ass2_2021_fall.R")
source("functions.R") #I hid some code here.
##########which section of the course are you in?
section <- 1 # set section to either 1 or 2, depending on what section you are in.
######################
public_folder <- paste0("publicdata",section)
all_data <- paste0("mydf.csv")
mydf <- read_csv(here(public_folder,all_data))
supply_and_demand <- read_csv(here(public_folder,"supply_and_demand.csv"))
bid_and_ask <- read_csv(here(public_folder,"bid_and_ask.csv"))
outcomes <- read_csv(here(public_folder,"outcomes.csv"))
#####################
(first_plot <- ggplot()+
  geom_step(data=filter(supply_and_demand, round_of_ten==7), 
            mapping=aes(x=q, y=variable, colour=role), 
            direction="vh", lwd=1.25, alpha=.5)+
  geom_step(data=filter(supply_and_demand, round_of_ten==7), 
            mapping=aes(x=q, y=net_of_tax, colour=role), 
            direction="vh", lwd=1.25, alpha=.5)+
  geom_step(data=filter(bid_and_ask, round_of_ten==7), 
            mapping=aes(x=q, y=variable, colour=role), 
            direction="vh", alpha=.25)+
  geom_point(data=filter(outcomes, round_of_ten==7), 
             mapping=aes(x=q,y=price), colour="black", size=2, alpha=.2)+ 
  facet_grid(treatment~Rounds)+
  labs(title = paste0("Round ",7 ," of 10."),
      x=" \n Quantity",
      y="Price")+
  theme(axis.title.y = element_text(angle = 0)))

