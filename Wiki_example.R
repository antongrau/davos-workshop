library(rvest)
library(stringi)
library(wikipediatrend)
library("rjson")
library("pageviews")
library(WikipediR)
library(progress)
library(knitr)
library(dplyr)

########################################
##  First we find the links on the 
########################################

urls <- read_html("https://fr.wikipedia.org/wiki/Cat%C3%A9gorie:Sociologue_fran%C3%A7ais") %>% 
        html_node("#mw-subcategories") %>% html_node(".mw-content-ltr") %>% 
        html_nodes("a") %>% 
        html_attr("href")

urls <- paste0("https://fr.wikipedia.org", urls)

urls

#The we follow each of the links and scrape the names + links to all the sociologists

data <- data.frame()

pb <- txtProgressBar(min = 1, max = length(urls), style = 3)

for (i in 1:length(urls)) {
  f1          <- read_html(urls[i])
  names       <- f1 %>% html_nodes(".mw-category-group") %>% html_nodes("a") %>% html_text()
  links       <- f1 %>% html_nodes(".mw-category-group") %>% html_nodes("a") %>% html_attr("href")
  
  # check whether there are more pages 
  page_plus   <- length(f1 %>% html_nodes(xpath = '//a[contains(@href, "mw-pages")]') %>% html_attr("href"))
  if(page_plus > 0) {
    next_p      <- f1 %>% html_nodes(xpath = '//a[contains(@href, "mw-pages")]') %>% html_attr("href")
    url2        <- paste0("https://fr.wikipedia.org", next_p[2])
    f2          <- read_html(url2) 
    names2      <- f2 %>% html_nodes(".mw-category-group") %>% html_nodes("a") %>% html_text()
    links2      <- f2 %>% html_nodes(".mw-category-group") %>% html_nodes("a") %>% html_attr("href") 
    names       <- c(names, names2)
    links       <- c(links, links2)
  }
  data      <- rbind(data, data.frame(round = i, name = names, wikilink = paste0("https://fr.wikipedia.org", links), stringsAsFactors = F))
  setTxtProgressBar(pb, i)
}

## Since some are in more than one 'set' we delete duplicates
data        <- data[!duplicated(data$wikilink),]
data <- data[-grep("/", data$name),] ########### Who has a / in his name!

head(data[,-1], 25)

any(data$name %in% c("François Denord", "Paul Lagneau–Ymonet", "Sylvain Thine"))



##################################################################
#   Now that we have all sociologists and the links to their individual wikipages, 
#   we can scrape all kinds of information... 
##################################################################

###############
# page length
###############
page_content("fr", "wikipedia", page_name = data$name[59], as_wikitext = T)$parse$wikitext %>% nchar()
data$wikilink[59] %>% read_html() %>% html_node("#mw-content-text") %>% html_text() %>% nchar()

data$length    <- pbapply::pbsapply(data$wikilink, function(x) x %>% read_html() %>% html_node("#mw-content-text") %>% html_text() %>% nchar())

head(data[order(data$length, decreasing = T), c("name", "length")], 10)
#######################################
# number of languages besides french
#######################################

wp_linked_pages(data$name[59], "fr") %>% nrow()
data$wikilink[59] %>% read_html() %>% html_nodes(".interlanguage-link-target") %>% html_attr("href") %>% length()

data$nb_lan    <- pbapply::pbsapply(data$wikilink, function(x) x %>% read_html() %>% html_nodes(".interlanguage-link-target") %>% length())

head(data[order(data$nb_lan, decreasing = T), c("name", "nb_lan")], 10)
######################################
# edit history
###################################
data <- read.csv("~/Dropbox/davos workshop 2019/Scraping/data/wiki_example_data.csv")

data$unique_editors  <- NA
data$nb_edits        <- NA
data$subst_edits     <- NA
data$latest_edit     <- NA
data$first_edit      <- NA

wiki_id <- gsub("(.*/wiki/)(.*)", "\\2",data$wikilink)

pb <- pbmcapply::progressBar(min = 1, max = nrow(data), style = "ETA")
for(i in 1:length(wiki_id)){
  url                            <- paste0("https://fr.wikipedia.org/w/index.php?title=",
                                          URLdecode(wiki_id[i]), 
                                          "&offset=&limit=2500&action=history")
  p                             <- url %>% read_html() %>% html_nodes(".mw-changeslist-date") %>% html_text()
  m                             <- url %>% read_html() %>% html_nodes(".minoredit") %>% html_text()
  u                             <- url %>% read_html() %>% html_nodes(".mw-userlink") %>% html_text()
  
  
  data$unique_editors[i]        <- length(unique(u))
  data$eager_editor[i]          <- names(sort(table(u), decreasing = T))[1]
  data$nb_edits[i]              <- length(p)
  data$subst_edits[i]           <- length(p) - length(m)
  data$latest_edit[i]           <- p[1]
  data$first_edit[i]            <- p[length(p)]
  setTxtProgressBar(pb, i)
}

data$firstyr       <- as.numeric(stringr::str_extract(data$first_edit, "[0-9]{4}"))
data$lastyr        <- as.numeric(stringr::str_extract(data$latest_edit, "[0-9]{4}"))
data$page_age      <- data$lastyr - data$firstyr + 1
data$edits_pr_yr   <- data$nb_edits / data$page_age

head(data[order(data$subst_edits, decreasing = T), c("name", "subst_edits")], 10)
head(data[order(data$page_age, decreasing = T), c("name", "page_age")], 10)
head(data[order(data$edits_pr_yr , decreasing = T), c("name", "edits_pr_yr")], 10)

#############################################
# Let's compare some sociologists over time
#############################################


api         <- "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article"
project     <- "fr.wikipedia.org"
access      <- "all-access"
agent       <- "user"
granularity <- "daily"
start       <- 20100831
end         <- 20190910

data$wiki_id <- wiki_id

wp_view_hist  <- function(x) {
  page_title  <-  x$wiki_id
  url         <-  paste(api, project, access, agent, page_title, granularity, start, end, sep = "/")
  p           <-  url %>% read_html() %>% html_text() %>% fromJSON()
  tmp_i       <-  lapply(p$items, function(x) data.frame(time = x$timestamp, views = x$views, stringsAsFactors = F))
  bind_rows(tmp_i)
}

data_split    <- split(data, f = factor(data$name, levels = unique(data$name)))
views         <- pbapply::pblapply(data_split, wp_view_hist) 

data$views              <- sapply(views, function(x) sum(x$views))
data$avr_view_pr_day    <- sapply(views, function(x) sum(x$views) / nrow(x))


head(data[order(data$avr_view_pr_day , decreasing = T), c("name", "avr_view_pr_day")], 10)




view_hist <- bind_rows(views, .id = "name")

library(ggplot2)
library(scales)
view_hist$time <- as.Date(paste0(substr(view_hist$time, 1, 4), "-", substr(view_hist$time, 5, 6 ), "-", substr(view_hist$time, 7, 9)))

# let's pick 3 sociologists to compare

sel <- which(view_hist$name %in% c("Pierre Bourdieu", "Luc Boltanski", "Bruno Latour"))

p <- ggplot() + geom_smooth(view_hist[sel,], mapping = aes(x = time, y = views, color = name)) +  scale_x_date(labels = date_format("%Y-%m-%d"),
                                                                                                               breaks = "1 year", minor_breaks = "months") + geom_line(view_hist[sel,], mapping = aes(x = time, y = views, color = name)) 
p



write.csv(data, "~/Dropbox/davos workshop 2019/Scraping/data/wiki_example_data.csv")
