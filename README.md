# Econ 381 assignment 2

The following instructions assume that: 

1) you have, following instructions here: https://github.com/rpmartin/Rinstall, either: 
  - installed R, Rstudio, tidyverse, Rmarkdown, and git locally (on your computer) OR
  - signed up for an Rstudio cloud account and installed the tidyverse.

2) I have uploaded the data and the assignment2.R and assignment2.Rmd files (you should see them listed above. If not, they will be uploaded shortly after the experiment is completed.)

## Steps:

Click on the green button labeled code. (above)

Click on the clipboard icon to copy address to clipboard (the icon is to the right of the text box.)

open Rstudio either locally or on the cloud.

click on File in the top menu bar, then New Project, then Version Control, then Git.

Where it asks for Repository URL paste (control V) from your clipboard (this should be what you copied from above)

R chooses the name of the folder in which you will be working, but you should choose where you want this folder saved.

Click Create Project and Rstudio will start in this folder, and all the files you need have been cloned into this folder from github.

Open file assignment2.R

Ensure that your cursor is at the top of the file assignment2.R (the file is open in the top-left box in Rstudio). 
Run the code by repeatedly hitting both control and enter. 
By doing so you step through the code line by line, and in the bottom left panel you see the results of each line of code. 
The last line of code creates your first graph, which should show up in the bottom right panel.

There should be no errors (yet ;) when you run the code of assignment2.R. If there are errors, do not proceed further! 
Get in touch either by email or on the message board. 
Only once you have checked that there are no errors in your assignment2.R file should you open up assignment2.Rmd file.

In the assignment2.Rmd file set author to your first initial and last name, and “sign” your academic integrity statement (replace name here with your first initial and last name).

Knit your assignment2.Rmd file by hitting the knit button (there is a ball of yarn with a needle sticking out of it beside the word knit). 
This is called knitting because we are “weaving” together R code (that produces graphs and tables) and prose.

A new window should open with your assignment.

At this point you can start writing code (incrementally) in your .R file to create the plots you will eventually reference by name in your .Rmd file.

For the written answers you include your prose in the .Rmd file.

## Assignment Submission:

Unfortunately Brightspace strips images out of html files, so you will not be able to submit your knitted html file to brightspace.  Instead,

* Assignment submissions to Brightspace will be your 
1) assignment2.R file
2) assignment2.Rmd file
3) a link a.k.a. URL for your assignment2.html file.

* The easiest way to host your assignment2.html file on the web is to click the `publish` button on Rstudio's html viewer (the html viewer opens when you successfully knit your output.) 

* Choose Rpubs and setup a free account: Note that the *content* that you publish on RPubs is publicly viewable: probably best to limit the amount of personal information you provide i.e. do **not** include your student ID, and do **not** include your full name e.g. in your .Rmd file set author to `Y. Name` (good) rather than `Your Name:  V00123456` (bad).

