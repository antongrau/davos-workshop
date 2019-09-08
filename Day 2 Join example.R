source("0_functions.R")

davos       <- read_xlsx("data/Davos members 2013-2019.xlsx", sheet = 1)
davos.org   <- read_xlsx("data/Davos members 2013-2019.xlsx", sheet = 2)

# Group and summarise 

country <- davos %>% group_by(Org) %>% summarise("Number of countries" = length(table(Country)),
                                      "Most typical country" = Country %>% table %>% sort %>% tail(1) %>% names)


# Filter and join
joined  <- country %>% filter(Org %in% davos.org$Organization) %>%
                       left_join(davos.org, ., by = c("Organization" = "Org"))

View(joined)