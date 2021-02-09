require(pacman)
p_load(tidyverse,pdftools,tabulizer,lubridate,rvest,yaml,jsonlite)

####### format data on representatives #########

current_leg <- fromJSON("https://theunitedstates.io/congress-legislators/legislators-current.json") %>% flatten()
past_leg <- fromJSON("https://theunitedstates.io/congress-legislators/legislators-historical.json") %>% flatten()
past_leg <- past_leg %>% dplyr::select(names(current_leg))

leg <- rbind(current_leg,past_leg)
rm(current_leg,past_leg)
leg$LASTNAME <- toupper(leg$name.last)
leg$birthday <- ymd(leg$bio.birthday)
leg <- leg %>% dplyr::filter(birthday>ymd("1900-01-01"))

leg$in_house <- NA %>% as.interval()
leg$party <- NA

for (i in 1:nrow(leg)) {
  terms <- leg$terms[i] %>% as.data.frame() %>% dplyr::filter(type=="rep")
  in_office <- interval(start=min(terms$start,na.rm=T),end=max(terms$end,na.rm=T))
  leg$in_house[i] <- in_office %>% as.interval()
  leg$party[i] <- first(terms$party)
  leg$state[i] <- first(terms$state)
}

leg <- leg %>% dplyr::filter(!is.na(in_house))

############# get session dates ################

url <- "https://history.house.gov/Institution/Session-Dates/All/"

con_sess <- url %>% read_html() %>% html_nodes(xpath="/html/body/div[2]/div[2]/div/div/div/div[2]/div/div[1]/table") %>% html_table()
con_sess <- con_sess[[1]]

con_sess$Congress2 <- str_extract(con_sess$Congress,"^[:digit:]{1,3}") %>% as.character() %>% as.numeric()
con_sess <- con_sess %>%
  fill(Congress2, .direction = "down")

con_sess$begin <- mdy(con_sess$`Beginning Date`)
con_sess$end <- mdy(con_sess$AdjournmentDate1)
con_sess$end[nrow(con_sess)] <- ymd("2022-12-31")
con_sess$sesh <- interval(con_sess$begin,con_sess$end)




get_url <- function (d) {
  d2 <- as.character(d)
  d1 <- d2 %>% str_replace_all("-","/")
  if(sum(d %within% con_sess$sesh,na.rm=T)==1) {
    s <- con_sess$Congress2[which(d %within% con_sess$sesh)]
    url <- paste0("https://www.congress.gov/",s,"/crec/",d1,"/CREC-",d2,"-house.pdf")
    return(url)
  } else {
    return(NA)
  }
}

get_session <- function (d) {
  s <- con_sess$Congress2[which(d %within% con_sess$sesh)]
  s
}


#################################################################

dat <- data.frame(x=1:(365*20+60))
dat$date <- ymd("2001-01-02") + days(dat$x)

for (i in 1:nrow(dat)) {
  
  d <- ymd(dat$date[i])
  
  if (exists("pdf1")) {
    rm(pdf1)
  }
  
  tryCatch({
    pdf1 <- get_url(d) %>% tabulizer::extract_text()
  }, error=function(e){})
  
  if (exists("pdf1")) {
    
    name_file <- paste0("CR_",str_replace_all(as.character(d),"-","_"))
    
    assign(name_file,pdf1)
  #  rm(pdf1)
    
    
    
    save(list=c(name_file),file=paste0("~/congressional-record/data/",name_file,".Rdata"))
    
    } 
  }




##################################################################
