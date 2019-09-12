source("0_functions.R")
library(eliter)
eo  <- read_csv("data/edge_list_w_attributes.csv")

table(eo$`DM Full name`) %>% sort() %>% tail()
table(eo$organisation_forward_fill) %>% sort() %>% tail()

eo$Name   <- paste(eo$`DM Full name`, eo$`DM UCI (Unique Contact Identifier)`)
incidence <- xtabs(~Name + organisation_forward_fill, eo, sparse = TRUE)

adj.ind   <- incidence %*% t(incidence)
adj.org   <- t(incidence) %*% incidence

g.org     <- graph_from_adjacency_matrix(adj.org, weighted = TRUE, mode = "undirected", diag = FALSE)
g.ind     <- graph_from_adjacency_matrix(adj.ind, weighted = TRUE, mode = "undirected", diag = FALSE)


degree(g.org) %>% sort() %>% tail(20)
degree(g.ind) %>% sort() %>% tail(20)

graph.plot(g.org, edge.size = 0.5 , edge.color = "black", edge.alpha = 1, vertex.size = degree(g.org))

g.org.component <- largest.component(g.org)
graph.plot(g.org.component, edge.size = 0.5 , edge.color = "black", edge.alpha = 1,
           vertex.size = degree(g.org.component), text = TRUE, text.background = "white")

# Centrality
centrality <- tibble("Node"  = V(g.org)$name, "Degree" = degree(g.org),
                     "Reach" = reach(g.org), "Closeness" = closeness(g.org),
                     "Betweenness" = betweenness(g.org))

View(centrality)

# As a two-mode network
g.two <- graph_from_incidence_matrix(incidence)
g.two

g.two   <- largest.component(g.two)

V(g.two)$name[V(g.two)$type]

graph.plot(g.two, edge.size = 0.5, edge.color = "black", edge.alpha = 1,
           vertex.fill = V(g.two)$type) + scale_fill_manual(values = c("papayawhip", "darkblue"), name = "Is an Org?")

g.two <- g.two - which(degree(g.two) == 1)



graph.plot(g.two, edge.size = 0.5, edge.color = "black", edge.alpha = 1,
           vertex.fill = V(g.two)$type, vertex.size = betweenness(g.two)) + scale_fill_manual(values = c("papayawhip", "darkblue"), name = "Is an Org?")

betweenness(g.two) %>% sort() %>% tail(50)

# Coreness ----
coreness  <- graph.coreness(g.org)
graph.coreness(g.org) %>% table()
g.core <- g.org - which(coreness != max(coreness) )
graph.plot(g.core, edge.size = 0.5, text = TRUE, edge.color = "darkblue")

# Layouts
graph.plot(g.org.component, layout_in_circle(g.org.component), edge.color = "black", edge.alpha = 0.5, vertex.fill = "white", edge.size = 0.5)

graph.plot(g.org.component, layout_with_fr(g.org.component), edge.color = "black", edge.alpha = 0.5, vertex.fill = "white", edge.size = 0.5)
graph.plot(g.org.component, layout_with_kk(g.org.component), edge.color = "black", edge.alpha = 0.5, vertex.fill = "white", edge.size = 0.5)

graph.plot(g.two, layout_with_fr(g.two), edge.color = "black", edge.alpha = 0.5, vertex.fill = "white", edge.size = 0.5)
graph.plot(g.two, layout_with_kk(g.two), edge.color = "black", edge.alpha = 0.5, vertex.fill = "white", edge.size = 0.5)

