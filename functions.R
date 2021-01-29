#### This file contains the functions to calculate the maximal welfare, 
# which is what we would observe in a competitive equilibrium with an optimal per unit tax.
# There are quite a few steps, which is why I hid these functions here, away from prying eyes... 
damage_per_person_per_unit <- .3

get_maximal_welfare <- function(mydf){
  maximal_welfare <- mydf%>%#take dataframe mydf which contains all the data THEN
    group_by(treatment, Rounds, round_of_ten)%>%#create separate groups for each separate simulation
    select(role,variable,tax)%>%# keep these columns (as well as the grouping columns)
    mutate(population=n(),#counts the number of people in each group a.k.a. simulation.
           opt_tax_per_unit=population*damage_per_person_per_unit,#the optimal per unit tax is equal to the per unit harm.
           net_of_otax=case_when(role=="buyer" ~ variable, #buyers do not pay the tax.
                                 role=="seller" ~ variable + opt_tax_per_unit))%>%# a.k.a. marginal social cost.
    nest()%>% #separate the grouping variables from the actual data, putting the actual data into a "nest".
    mutate(maximal_mean=map_dbl(data,calc_max_welfare)) #maps the function calc_max_welfare to each of the "nests" of data.
}

calc_max_welfare <- function(data){
  buyers <- data%>% #take the data from a single simulation THEN
    filter(role=="buyer")%>%#keep just the buyers
    arrange(desc(net_of_otax))%>%#arrange from highest value to lowest
    mutate(ID = row_number())#we will use this ID when we merge the buyers and sellers back together
  sellers <- data%>% #take the data from an individual simulation THEN
    filter(role=="seller")%>%#keep just the sellers
    arrange(net_of_otax)%>%#arrange from lowest to highest
    mutate(ID = row_number())#we will use this ID when we merge the buyers and sellers back together 
  net_welfare <- full_join(buyers,sellers, by=c("ID"))%>% #merge by ID 
    filter(net_of_otax.x > net_of_otax.y)%>%#keep rows where value exceeds net cost: number of rows is q*.
    mutate(gross_welfare = variable.x - variable.y)%>%#gross of the tax: transfers do not affect welfare. 
    summarise(gross_welfare=sum(gross_welfare),#the total. 
              units=n(),# number of transactions in equilibrium (featuring optimal tax)
              persons=mean(population.x))%>%# this is a hack: summarise requires a many:one function.
    mutate(net_wel=gross_welfare/persons-damage_per_person_per_unit*units)%>%#this is per capita.
    pull(net_wel)#pull variable net_wel out of dataframe net_welfare.
  }
