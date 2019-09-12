library(tidyverse)
library(readxl)
library(data.table)
orbis <- read_xlsx("data/orbis_many_lines.xlsx", sheet = 2)
orbis_match <- read_xlsx("data/orbis_many_lines_matchlist.xlsx", sheet = 1) 
orbis_match <- orbis_match[orbis_match$`Company name` != "NA",]
orbis_match <- orbis_match[!duplicated(orbis_match$`Matched bvdid`),]

setDT(orbis)
orbis[, organisation_forward_fill := `Company name`[1], .(cumsum(!is.na(`Company name`)))]

colnames(orbis) <- gsub("\n", " ", colnames(orbis))

orbis <- orbis[orbis$`DM Type of role` %in% c("BoD", "ExeB"),]
orbis[ , role := paste(`DM Type of role`, collapse = "|"), by = .(organisation_forward_fill, `DM UCI (Unique Contact Identifier)`)]

orbis_split <- split(orbis, f = factor(orbis$organisation_forward_fill, levels = unique(orbis$organisation_forward_fill)))

orbis_split <- lapply(orbis_split, function(x) x[!duplicated(x$`DM UCI (Unique Contact Identifier)`), ])

orbis2 <- bind_rows(orbis_split)

orbis2 <- orbis2[, grepl("DM |organisation_forward_fill|^role$", colnames(orbis2)), with = F]

orbis_red <- orbis[!is.na(orbis$`Company name`),]


cols <- c(setdiff(colnames(orbis_red), colnames(orbis2)), "organisation_forward_fill")

orbis_final <- left_join(orbis2, orbis_red[, cols, with = F], by = c("organisation_forward_fill"))


write.csv(orbis_final, "output/edge_list_w_attributes.csv")
