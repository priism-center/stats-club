library(plyr)
library(tidyverse)
library(httr)
library(rvest)
library(XML)
library(xml2)
          
          ##### Exercise One #####

#Lets scrape the first page of Data

baseURL <- "https://salaries.texastribune.org/search/?q=Dallas"

baseURL %>%
  read_html() %>%
  html_nodes(xpath = "/html/body/section/section/div[1]/table") %>% #fill in xpath using inspect and copy + paste 
  html_table()

#Lets scrape multiple pages,this requires messing around with the URL 
#Also bc there are multiple iterations a 'for loop' or 'apply function' is necessary 

#Usually when scraping multiple web pages across a site, the supplied xpath is not good

dallas_sal <- lapply(paste0('', ),
                     function(url){
                       url %>%
                         read_html() %>% 
                         html_nodes(xpath = "/html/body/section/section/div[1]/table") %>% 
                         html_table()
                     })

###### WHAT HAPPENS ? #####

#Let's try again creating our own xpath 

dallas_sal <- lapply(paste0('', ),
                     function(url){
                       url %>%
                         read_html() %>% 
                         html_nodes(xpath = "") %>% 
                         html_table()
                     })

#examine structure
str(dallas_sal) 

#We can unlist the list and make it a traditional dataframe to make analysis easier 
dallas_sal <- ldply(dallas_sal, data.frame)

#the only thing is we do not have headers 
names(dallas_sal) <- c("Name", "Title", "Entity","Department","Salary")

          


          ##### Exercise Two #####

#Now lets get a list of the names of employees across the multiple pages

dallas_names <- lapply(paste0('https://salaries.texastribune.org/search/?q=Dallas&page=', 1:50),
                       function(url){
                         url %>%
                           read_html() %>% 
                           html_nodes(xpath = "") %>% 
                           html_text()
                       })

dallas_names <- data.frame(unlist(dallas_names))

          ##### Exercise Three #####

#Practice with hrefs 

baseURL <- "https://www.imdb.com/movies-in-theaters/?ref_=nv_mv_inth" #movies in theaters

hrefs <- baseURL %>%
  read_html() %>%
  html_nodes(xpath = "") %>%
  html_attr(name = "href")

firstpart <- "https://www.imdb.com"

absolutelinks <- paste0(firstpart, hrefs, sep = "")

#Let's figure out what we want to scrape :) 


