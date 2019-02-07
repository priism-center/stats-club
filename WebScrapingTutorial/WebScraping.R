library(plyr)
library(tidyverse)
library(httr)
library(rvest)
library(XML)
library(xml2)
library(RCurl)

#there are 2611 pages

#lets scrape the first page of Data

baseURL <- "https://salaries.texastribune.org/search/?q=Dallas"

#sometimes using the xpath given by Chrome or Firefox is okay! 
#the headers are not brought in automatically, but we can always manually supply them

baseURL %>%
  read_html() %>%
  html_nodes(xpath = "/html/body/section/section/div[1]/table") %>%
  html_table()

#lets extend to scrape multiple pages,this requires fiddling with the URL 
#also bc now there are multiple iterations a for loop or apply function is necessary 

    # usually with scraping multiple web pages across a site, the supplied xpath is not good
    dallas_sal <- lapply(paste0('https://salaries.texastribune.org/search/?q=Dallas&page=', 1:50),
                     function(url){
                       url %>%
                         read_html() %>% 
                         html_nodes(xpath = "/html/body/section/section/div[1]/table") %>% 
                         html_table()
                     })
    #however, we can create our own that works for each page 
    dallas_sal <- lapply(paste0('https://salaries.texastribune.org/search/?q=Dallas&page=', 1:50),
       function(url){
          url %>%
           read_html() %>% 
           html_nodes(xpath = "//table") %>% 
           html_table()
         })

#examine structure
str(dallas_sal) 

#We can unlist the list and make it a traditional dataframe to make analysis easier 
dallas_sal <- ldply(dallas_sal, data.frame)

#the only thing is we do not have headers 
names(dallas_sal) <- c("Name", "Title", "Entity","Department","Salary")

#Now lets get a list of the names of employees across the multiple pages

dallas_names <- lapply(paste0('https://salaries.texastribune.org/search/?q=Dallas&page=', 1:50),
                     function(url){
                       url %>%
                         read_html() %>% 
                         html_nodes(xpath = "//td[@data-title = 'Name']") %>% 
                         html_text()
                     })

dallas_names <- data.frame(unlist(dallas_names))


# Hrefs exercise

baseURL <- "https://www.imdb.com/movies-in-theaters/?ref_=nv_mv_inth" #movies in theaters

hrefs <- baseURL %>%
  read_html() %>%
  html_nodes(xpath = "//td/h4/a") %>%
  html_attr(name = "href")

firstpart <- "https://www.imdb.com"

absolutelinks <- paste0(firstpart, hrefs, sep = "")














