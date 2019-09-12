source("0_functions.R")
load(file = "data/davos.joined.Rda")


# Geocode sets
files <- list.files(path = "data/Coding team/", full.names = TRUE, recursive = TRUE)
files <- files[grep(".xls", files, fixed = TRUE)]

# Read files ----

rf <- lapply(files, read_xlsx, sheet = 1)
names(rf) <- files %>% gsub("data/Coding team/", "", ., fixed = TRUE) %>% 
  gsub(".xlsx", "", ., fixed = TRUE) %>%
  strsplit("/") %>% lapply(tail, n = 1) %>% unlist()

l.16 <- rf[[16]] %>% split(f = .$modalities)
l.12 <- rf[[12]] %>% split(f = .$taxlist)
l.11 <- rf[[11]] %>% split(f = .$"Trade agreement")

rf[c(11, 16, 12)] <- NULL

rf        <- c(rf, l.16, l.12, l.11)
rf        <- bind_rows(rf, .id = "file")


rf %>% filter(is.na(Country)) %>% select(file) %>% unique

# Recode country codes ----
iso2c              <- countrycode(rf$Country, origin = "country.name", destination = "iso2c")

char.set           <- nchar(rf$Country) == 2
rf$iso2c           <- iso2c
rf$iso2c[char.set] <- rf$Country[char.set]

char.set           <- nchar(rf$Country) == 3
rf$iso2c[char.set] <- countrycode(rf$Country[char.set], origin = "iso3c", destination = "iso2c")

# Recode matrix ----
country.matrix     <- xtabs(~ iso2c + file, rf) 
country.matrix     <- country.matrix %>% as_tibble( ) %>% spread(key = file, value = n)



# Join the matrix ----
joined.countries.davos    <- davos.join %>% select(Organization, iso2c = `Most typical country code in WEF`) %>% left_join(country.matrix)

# From Orbis to data ----
orbis.country             <- davos.join$`Code ISO Pays`
# Fix wrong codes
orbis.country[which(nchar(orbis.country) == 3)] <- countrycode(orbis.country[which(nchar(orbis.country) == 3)], origin = "iso3c", destination = "iso2c")
davos.join$`Code ISO Pays` <- orbis.country

# The join ----
joined.countries.orbis     <- davos.join %>% select(Organization, iso2c = `Most typical country code in WEF`) %>% left_join(country.matrix)

# Splitting subsidiaries ----
sub.list                    <- davos.join$`Filiales - Code pays ISO` %>% str_split("\n")
names(sub.list)             <- davos.join$Organization
subsidiary.countries        <- stack(sub.list) %>% select(iso2c = values, Organization = ind)
subsidiary.countries$Country <- subsidiary.countries$iso2c %>% countrycode(origin = "iso2c", destination = "iso2c")

all.subsidiary.countries     <- left_join(subsidiary.countries, country.matrix, by = c("iso2c" = "iso2c"))
joined.subsidiary.countries.orbis <- all.subsidiary.countries %>% group_by(Organization) %>%  summarise_if(is.numeric, sum, )
View(joined.subsidiary.countries.orbis)

