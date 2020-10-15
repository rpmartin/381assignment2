# You have no need to read or understand what is going on here: This is just if you are interested in how the supply, demand
# and outcome and your participation marks are created from the raw data. 
rm(list=ls())
library("tidyverse")
library("purrrlyr")
library("RMySQL")
source("credentials.R")
############################which section?
section <- 1
##############################Functions
supply <- function(data){
    cumsum(data)
}    
demand <- function(data){
    rev(cumsum(rev(data)))
}    
pad_out <- function(data,role){
    if(role=="buyer"){
        q <- c(0,0,data$q)
        p <- c(30,data$variable,0)
        data.frame(quantity=q,price=p)
    }else{
        q <- c(0,0,data$q)
        p <- c(0,data$variable,30)
        data.frame(quantity=q,price=p)
    }
}
##########################The program
con <- dbConnect(RMySQL::MySQL(), host = hst, user = usr, password = pswd, dbname= db)
mydf <- dbGetQuery(con, paste("SELECT * FROM ext",section,sep=""))
write_csv(mydf, file=paste("publicdata",section,"/ext",section,".csv",sep=""))

participants <- mydf%>%group_by(oneid)%>%select(oneid)%>%summarize(`ex1 Points Grade`=2)
class <- read_csv("privatedata/class.csv")
class <-  class%>%mutate(OrgDefinedId=str_replace(OrgDefinedId,"#",""),
                         Username=str_replace(Username,"#",""),
                         oneid=as.numeric(str_sub(OrgDefinedId,-5)))
marks <- left_join(class,participants)%>%
    select(OrgDefinedId,Username,`ex1 Points Grade`,`End-of-Line Indicator`)%>%
    replace_na(list(OrgDefinedId=0,Username=0,ex1=0,`End-of-Line Indicator`=0))
write_csv(marks,paste("privatedata/marks",section,".csv",sep=""))

vandc <- mydf%>% 
    group_by(treatment,role)%>% 
    filter(round==10|round==20)%>%
    select(variable,round)

sandd <- vandc%>%
    group_by(treatment,role,round,variable)%>%
    summarise(freq=n())%>%
    mutate(q= case_when(role=="seller" ~ supply(freq),role=="buyer" ~ demand(freq)))%>%
    arrange(q)%>%
    select(-freq)%>%
    group_nest()%>% 
    mutate(data2 = map2(data, role, pad_out))%>% 
    by_row(~write.csv(.$data2, 
                      row.names=FALSE, 
                      file = paste("publicdata",section,"/",.$role,.$round,.$treatment,".csv",sep="")))

outcomes <- mydf%>%
    group_by(treatment,round)%>%
    summarise(p=median(price),q=max(numtrans))%>%
    mutate(session=case_when(round<11 ~ "1-10",round>10 ~ "11-20"))%>%
    group_by(treatment,session)%>%
    group_nest()%>% 
    mutate(data2 = map(data, as.data.frame))%>%
    by_row(~write.csv(.$data2, 
                      row.names=FALSE, 
                      file = paste("publicdata",section,"/outcomes",.$treatment,.$session,".csv",sep="")))

