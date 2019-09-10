source("0_functions.R")

davos.org        <- read_xlsx(path = "data/Davos members 2013-2019.xlsx", sheet = 2)
davos            <- read_xlsx(path = "data/Davos members 2013-2019.xlsx", sheet = 1)
davos.monday     <- read_xlsx(path = "data/Davos members 2013-2019.xlsx", sheet = 3)

orbis_1 <- read_excel(path = "data/Orbis 1.xls", sheet = 2)
orbis_2 <- read_xlsx(path = "data/Orbis 2.xlsx", sheet = 1)
orbis_3 <- read_xlsx(path = "data/Orbis 3.xlsx", sheet = 3)



# The number of columns and rows
dim(orbis_1)
dim(orbis_2)
dim(orbis_3)

# Lets see what the overlap is
intersect(colnames(orbis_1), colnames(orbis_3))
setdiff(colnames(orbis_1), colnames(orbis_3))

intersect(colnames(orbis_2), colnames(orbis_3))
setdiff(colnames(orbis_2), colnames(orbis_3))

# Cleaning columnnames
colnames(orbis_1)  <- gsub("\n", " ", colnames(orbis_1))
colnames(orbis_2)  <- gsub("\n", " ", colnames(orbis_2)) %>%  gsub("\r", "", .)
colnames(orbis_3)  <- gsub("\n", " ", colnames(orbis_3)) %>%  gsub("\r", "", .)

orbis_1$Organization <- orbis_1$`HEJ!`

# Joining by rows
bind_rows(orbis_1, orbis_2, orbis_3)

# We have to force all variables to character.
orbis    <- bind_rows(mutate_all(orbis_1, as.character),
                      mutate_all(orbis_2, as.character),
                      mutate_all(orbis_3, as.character))

# Filtering davos.org
str(davos.org)
table(davos.org$`In sample`)
davos.sample   <- davos.org %>% filter(`In sample` != "FALSE")

# Join with Orbis
davos.join     <- left_join(davos.sample, orbis, by = c("Organization" = "Organization"))

# Join with monday collection
davos.join   <- left_join(davos.join, davos.monday, by = c("Organization" = "organisation"))

# Wide and long: Country example -------

head(davos) # Head gives us the beginning of an object
table(davos$Country) %>% sort %>% tail(10)
country <- davos %>% group_by(Org) %>%
                summarise("Number of countries in WEF" = length(table(Country)),
                "Most typical country in WEF" = Country %>% table %>% sort %>% tail(1) %>% names)

# Converting from country names to ISO codes
library(countrycode)
?countrycode

country$"Most typical country code in WEF"<- countrycode(country$`Most typical country in WEF`,
                                                         origin = "country.name", destination = "iso2c")


# Someone - (Anton) - couldn't spell
country$`Most typical country in WEF`   <- fct_recode(country$`Most typical country in WEF`,
                                                      "Israel" = "Isreal", "Philippines"  = "Phillipines")

# Now it works
country$"Most typical country code in WEF"<- countrycode(country$`Most typical country in WEF`,
                                                         origin = "country.name", destination = "iso2c")

country$Continent <- countrycode(country$"Most typical country code in WEF", origin = "iso2c", destination = "continent")
country$Region    <- countrycode(country$"Most typical country code in WEF", origin = "iso2c", destination = "region")

# Finally! All the sources are joined
davos.join  <- left_join(davos.join, country, by = c("Organization" = "Org" ))

dim(davos.join)
dim(davos.monday)

# Oh no - new rows!
# anti_join tells us what they are
anti_join(davos.join, davos.monday)

# They can luckily be removed
davos.join       <- davos.join %>% filter(Organization %in% davos.monday$organisation)


save(davos.join, file = "data/davos.joined.Rda")
