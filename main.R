#Project
#Vincent Gariepy
#This is the main script of the project, it will call two other scripts 
#CountingCards.r runs all the simulations and graphs the results of our exercise with a finite
#amount of decks.
#RandomCards.r runs all the simulations and graphs the results of our exercise without counting cards

#The graphs used in our report appear in the plot section, there are also other graphs which where not used
#directly in our report but still helped with the analysis.
#The script for RandomCards.r will also print in the console some data that was used directly
#in the report like the confidence intervals.
#All other data appears in the environment

#To not mix up any variables between script and since they are independent from one another,
#code is added to clear the plots and environment between scripts

#This one takes about 1min30 to run
source("CountingCards.r")

#Clear the environment, the plots and memory usage to run the next code more smoothly
rm(list = ls())
if(!is.null(dev.list())) dev.off()
gc()

#This one takes about 5min30 to run
source("RandomCards.r")

