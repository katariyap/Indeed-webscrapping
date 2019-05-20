
x<-c("XML","RCurl","rvest","stringr","dplyr","ggplot2")
lapply(x, require, character.only = TRUE)
library(XML)
library(RCurl)
library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)

##paste the URL you selected from indeed
page_result_start <- 10 # starting page 
page_result_end <- 240 # last page results
page_results <- seq(from = page_result_start, to = page_result_end, by = 10)

full_df <- data.frame()
for(i in seq_along(page_results)) {
    
    first_page_url <- "https://www.indeed.com/jobs?as_and=&as_phr=&as_any=hvac+refrigeration+air&as_not=&as_ttl=sales+engineer&as_cmp=&jt=all&st=&sr=directhire&salary=&radius=25&l=&fromage=any&limit=10&sort=&psf=advsrch&start10"
    url <- paste0(first_page_url, "&start=", page_results[i])
    page <- xml2::read_html(url)
    # Sys.sleep pauses R for two seconds before it resumes
    # Putting it there avoids error messages such as "Error in open.connection(con, "rb") : Timeout was reached"
    Sys.sleep(2)
    
    #get the job title
    job_title <- page %>% 
        rvest::html_nodes("div") %>%
        rvest::html_nodes(xpath = '//a[@data-tn-element = "jobTitle"]') %>%
        rvest::html_attr("title")
    
    #get the company name
    company_name <- page %>% 
        rvest::html_nodes("span")  %>% 
        rvest::html_nodes(xpath = '//*[@class="company"]')  %>% 
        rvest::html_text() %>%
        stringi::stri_trim_both() -> company.name 
    
    
    #get job location
    job_location <- page %>% 
        rvest::html_nodes("span") %>% 
        rvest::html_nodes(xpath = '//*[@class="location"]')%>% 
        rvest::html_text() %>%
        stringi::stri_trim_both()
    
    # get links
    links <- page %>% 
        rvest::html_nodes("div") %>%
        rvest::html_nodes(xpath = '//*[@data-tn-element="jobTitle"]') %>%
        rvest::html_attr("href")
    
    job_description <- c()
    for(i in seq_along(links)) {
        
        url <- paste0("https://ca.indeed.com/", links[i])
        page <- xml2::read_html(url)
        
        job_description[[i]] <- page %>%
            rvest::html_nodes("span")  %>% 
            rvest::html_nodes(xpath = '//*[@class="jobsearch-JobComponent-description icl-u-xs-mt--md"]') %>% 
            rvest::html_text() %>%
            stringi::stri_trim_both()
    }
    df <- data.frame(job_title, company_name, job_location, job_description)
    full_df <- rbind(full_df, df)}
write.csv(full_df, "full_df.csv")
