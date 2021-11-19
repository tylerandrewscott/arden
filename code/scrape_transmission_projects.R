#https://www.transmissionhub.com/transmission-projects
library(jsonlite)
library(tidyverse)
library(stringr)
library(data.table)
library(httr)

url = 'https://nqzkggqnbb.execute-api.us-east-1.amazonaws.com/Prod/api/CompanyProfile/GetProjectsData/'
raw = GET(url)
json = content(raw)

split_json <- str_split(json,'\\}\\,\\{')
split_json <- split_json[[1]]
split_json <- str_remove(split_json,'^\\[\\{')

processSplit = function(x){
  sp = str_split(x,',\"')
  title_and_entry = lapply(sp,str_split,'\\:')
  title = sapply(title_and_entry[[1]],function(x) x[1])
  entry = sapply(title_and_entry[[1]],function(x) x[2])
  entry <- str_replace_all(entry, "[^[:alnum:]|\\s]", "") 
  title <- str_replace_all(title, "[^[:alnum:]|\\s]", "") 
  entry <- entry[!is.na(entry)]
  title <- title[title!='']
  tdt = data.table(t(data.table(entry)),keep.rownames = F)
  names(tdt) <-title
  tdt[tdt=='null'] <-NA
  tdt
}

plists <- lapply(split_json,processSplit)
plist <- rbindlist(plists,use.names = T,fill = T)

fwrite(x = plist,file = paste0('input/transmission_project_records-',Sys.Date(),'.csv'))






