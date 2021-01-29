# Anything following a hashtag (#) is a comment, and is ignored. This is the file
# where you save your R code that produces the objects (graphs, tables) that you 
# want to embed into  your assignment.  Make sure that there are no errors when 
# you source this file before sourcing it from assignment1.Rmd file.
# To show you how it is done I have included some code below.
#####################################
rm(list=ls()) #makes sure that your work environment is clean.
library("tidyverse") # if tidyverse not found, install.packages("tidyverse") 
library("plotly") #if plotly not found, install.packages("plotly") 
slash <- ifelse(.Platform$OS.type=="unix", "/", "\\") #The eternal directory battle: Windows vs. *nix
source("functions.R") #I hid some code here.
##########which section of the course are you in?
section <- 1 # set section to either 1 or 2, depending on what section you are in.
######################
mydf <- read_csv(paste("publicdata",section,slash,"ext.csv",sep=""))
supply_and_demand <- read_csv(paste("publicdata",section,slash,"supply_and_demand.csv",sep=""))
bid_and_ask <- read_csv(paste("publicdata",section,slash,"bid_and_ask.csv",sep=""))
outcomes <- read_csv(paste("publicdata",section,slash,"outcomes.csv",sep=""))
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

