####################################
### WIKI
####################################
library(googledrive)
library(tidyverse)
library(rvest)
library(rjson)
#### open list of wikilinks ####
drive_download("DAVOS/Davos members 2013-2019", path = "data/Davos members 2013-2019", overwrite = TRUE)
data    <- readxl::read_xlsx("data/Davos members 2013-2019.xlsx", sheet = "Collection day1", na = "NA")
wiki    <- data[, c("organisation", "wiki_id", "wiki_lan")]
colnames(wiki) <- c("organisation", "page", "lang")
wiki    <- wiki[!is.na(wiki$page),]

###############################################
#
#        Function
#
###############################################

wiki_content_len    <- function(x) {
  con             <- x$url %>% read_html() 
  content         <- con %>% html_node("#mw-content-text") %>% html_text()
  if(length(con %>% html_nodes("style") %>% html_text())>0){
    content_del     <- con %>% html_nodes("style") %>% html_text()
    for(i in 1:length(content_del)){
      content         <- gsub(content_del[i], "", content, fixed = T)  
    }}
  length          <- content %>% nchar()
}


wiki_content    <- function(x) {
  con             <- x %>% read_html() 
  content         <- con %>% html_node("#mw-content-text")
}

wiki_language_links <- function(x) {
  page            <- x$page
  lang            <- x$lang
  url             <- paste0("http://", lang, ".wikipedia.org/wiki/", page)           
  content         <- url %>% read_html() 
  title           <- content %>% html_node("#firstHeading") %>% html_text() %>% gsub(" ", "_", .)
  links           <- data.frame(url = url, page = page, lang = lang, title = title, stringsAsFactors = F)
  interlink       <- content %>% html_nodes(".interlanguage-link-target") %>% html_attr("href")
  title           <- content %>% html_nodes(".interlanguage-link-target") %>% html_attr("title")
  page            <- interlink %>% gsub(".*wiki/", "", .)
  lang            <- interlink %>% gsub("(http://|https://)(.+?)(\\.wiki.*)", "\\2", .)
  interlink       <- data.frame(url = interlink, page = page, lang = lang, title = title)
  links           <- bind_rows(links, interlink)
}

###############################################
#
#        
#
###############################################

# Links

wiki_split <- split(wiki, f = factor(wiki$organisation, levels = unique(wiki$organisation)))
links           <- pbapply::pblapply(wiki_split, wiki_language_links)  

url_long     <- bind_rows(links, .id = "organisations")
lan <- c("en", "fr", "de", "es")
url_red      <- url_long[url_long$lang %in% lan, ]

# Content
content_list <- list()
pb <- txtProgressBar(min = 1, max = length(url_red$url), style = 3)
for(i in 1:length(url_red$url)){
content_list[[i]] <- wiki_content(url_red$url[i])
setTxtProgressBar(pb, i)
}
url_red$content             <- unlist(lapply(content_list, html_text))
url_red$content_html        <- unlist(lapply(content_list, function(x) x$doc))
url_red$critique               <- grepl("critique|critiquer|kritik|kritiser|crítico|criticar", url_red$content)
url_red$scandale               <- grepl("scandal|scandale|escándalo|skandale", url_red$content)
url_red$controversy            <- grepl("controvers|kontrovers|controversia", url_red$content)
url_red$corruption            <- grepl("corrupt|korrupt|corropción|soborno|chantaje", url_red$content)
url_red$tax                  <- grepl("fraud|tax eva|avoid tax|skattesnyd|skatteunddr|steuerhinter|evasión fiscal|fraude fiscal|évasion fiscale", url_red$content)

# Sentiment analysis English
url_en  <- url_red[url_red$lang == "en",]
library("quanteda")
library(tidyverse)
library("tidytext")
library("glue")
library(stringr)

sent <- get_sentiments("bing")
t <- lapply(url_en$content, tokens)
s <- lapply(t, function(x) table(inner_join(data.frame(word = unlist(unname(x))), sent)$sentiment))
empty <- which(sapply(s, function(x) length(x) == 0))
s[[empty]] <- data.frame(negative = 0, positive = 0)
s <- do.call(rbind, s)
s <- data.frame(organisation = url_en$organisations, s)
s$sentiment <- s$positive - s$negative

# lengths
lengths <- pbapply::pblapply(links, function(x) {
  tmp            <- x
  nb_lan         <- nrow(tmp)
  lan            <- tmp$lang
  tmp            <- split(tmp, f = factor(tmp$lang, levels = unique(tmp$lang))) 
  
  length         <- lapply(tmp, safely(wiki_content_len,0))
  length         <- lapply(length, function(x) x$result)
  length         <- length %>% unlist()
  ord            <- names(sort(length, decreasing = T))
  lan            <- paste0(lan[match(ord, lan)], " (", unname(sort(length, decreasing = T)), ")")
  lan            <- paste(lan, collapse = "; ")
  longest_lan    <- names(length)[which.max(length)]
  longest_length <- length[which.max(length)] %>% unlist() %>% unname()
  data.frame(nb_lan = nb_lan, lan = lan, longest_lan = longest_lan, longest_length = longest_length)
})

len <- bind_rows(lengths, .id = "Org")


write.csv(len, "~/Dropbox/DAVOS/R/Scraping data/Davos_wiki_lang_and_length.csv")


#  View history only en, fr, de, es

api         <- "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/"
access      <- "all-access"
agent       <- "user"
granularity <- "monthly"
start       <- 20150911
end         <- 20190911

pb <- txtProgressBar(min = 1, max = nrow(url_red), style = 3)
for(i in 1:nrow(url_red)) {
  project     <-  paste0(url_red$lang[i],".wikipedia.org")
  page_title  <-  URLdecode(url_red$page[i])                            # URLdecode is necessary because the http's we got contains strange encoding
  f2                            <- paste0(api, 
                                          project, "/",
                                          access, "/",
                                          agent, "/",
                                          page_title, "/",
                                          granularity, "/",
                                          start, "/",
                                          end)
  p                             <- f2 %>% read_html() %>% html_text() %>% fromJSON()
  
  tmp                           <- lapply(p$items, function(x) data.frame(name = url_red$organisations[i], time = x$timestamp, views = x$views))
  tmp                           <- bind_rows(tmp)
  url_red$avr_view_pr_day[i]         <- mean(tmp$views)
  # views                         <- rbind(views, tmp)
  setTxtProgressBar(pb, i)
}




#####################################################
###
### API: "https://wikimedia.org/api/rest_v1/metrics/edits/per-page/"
###       limitations: one year only
#####################################################

api         <- "https://wikimedia.org/api/rest_v1/metrics/edits/per-page/"
project     <- "fr.wikipedia.org"
editortype  <- "all-editor-types"
granularity <- "monthly"
start       <- 20180911
end         <- 20190911

pb <- txtProgressBar(min = 1, max = nrow(url_red), style = 3)
for(i in 1:nrow(url_red)) {
  page_title  <-  URLdecode(url_red$page[i])
  f2                            <- paste0("https://wikimedia.org/api/rest_v1/metrics/edits/per-page/", 
                                          project, "/",
                                          page_title, "/",
                                          editortype, "/",
                                          granularity, "/",
                                          start, "/",
                                          end)
  
  p                             <- f2 %>% read_html() %>% html_text() %>% fromJSON()
  tmp                           <- unlist(p$items[[1]]$results)
  tmp                           <- as.numeric(tmp[names(tmp) == "edits"])
  url_red$edits_sum_10months[i]    <- sum(tmp)
  url_red$edits_avr_month[i]       <- mean(tmp)
  setTxtProgressBar(pb, i)
  Sys.sleep(.5)
}

head(url_red[order(url_red$edits_avr_month, decreasing = T),c("organisations", "edits_avr_month")])

### Article info
api   <- "https://xtools.wmflabs.org/api/page/articleinfo/"
urls  <- paste0(url_long$lang[i], ".", "wikipedia.org/", url_long$page[i])
l     <- paste0(api, urls)
info <-  tryCatch(pbapply::pblapply(l, function(x) x %>% read_html() %>% html_text() %>% fromJSON()), error = function(e) NA)
info <- lapply(info, function(x) x[-c(length(x)-1)])
info <- bind_rows(info)
setTxtProgressBar(pb, i)

### Prose
api   <- "https://xtools.wmflabs.org/api/page/prose/"
urls  <- paste0(url_long$lang, ".", "wikipedia.org/", url_long$page)
l     <- paste0(api, urls)
prose <-  tryCatch(pbapply::pblapply(l, function(x) x %>% read_html() %>% html_text() %>% fromJSON()), error = function(e) NA)
prose <- bind_rows(prose)

### Links 
api   <- "https://xtools.wmflabs.org/api/page/links/"
urls  <- paste0(url_long$lang, ".", "wikipedia.org/", url_long$page)
l     <- paste0(api, urls)
links <- tryCatch(pbapply::pblapply(l, function(x) x %>% read_html() %>% html_text() %>% fromJSON()), error = function(e) NA)
links <- bind_rows(links)

d <- data.frame(organisations = url_long$organisations, info, prose, links)
View(d)



########## Doesn't really work

# pb <- txtProgressBar(min = 1, max = nrow(url_red), style = 3)
# for(i in 1:nrow(url_red)){
#   f2                            <- paste0("https://",
#                                           url_red$lang[i],
#                                           ".wikipedia.org/w/index.php?title=",
#                                           URLdecode(url_red$page[i]), 
#                                           "&offset=&limit=50000&action=history")
#   p                             <- f2 %>% read_html() %>% html_nodes(".mw-changeslist-date") %>% html_text()
#   m                             <- f2 %>% read_html() %>% html_nodes(".minoredit") %>% html_text()
#   u                             <- f2 %>% read_html() %>% html_nodes(".history-user") %>% html_text()
#   
#   url_red$unique_editors[i]        <- length(unique(u))
#   url_red$nb_edits[i]              <- length(p)
#   url_red$subst_edits[i]           <- length(p) - length(m)
#   url_red$latest_edit[i]           <- p[1]
#   url_red$first_edit[i]            <- p[length(p)]
#   setTxtProgressBar(pb, i)
# }
# pb$kill()
# 
# data$firstyr <- as.numeric(stringr::str_extract(data$first_edit, "[0-9]{4}"))
# data$lastyr  <- as.numeric(stringr::str_extract(data$latest_edit, "[0-9]{4}"))
# data$yr      <- data$lastyr - data$firstyr + 1
# data$edits_pr_yr <- data$nb_edits / data$yr
# View(data)

