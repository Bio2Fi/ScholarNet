library(networkD3)
library(tidyverse)


Ids <- read_rds("IDS.rds")
coauthors <- read_rds("coauthors.rds")

  coauthor_network <- coauthors %>% 
    mutate(coauthors = ifelse(str_detect(coauthors,"Pdf"), "Brian J. Enquist", coauthors), author = ifelse(str_detect(author,"Pdf"), "Brian J. Enquist", author)) %>% dplyr::filter(!str_detect(author, "Sort By"),!str_detect(coauthors, "Sort By"))
  
  
Members <- data.frame(author = "BIOFI", coauthors = unique(Ids$Name))
  
coauthor_network <- bind_rows(Members, coauthor_network)


Nodes <- data.frame(name = unique(Members$coauthors), group = 2, size = 3)
Nodes_1 <- data.frame(name = "BIOFI", group = 1, size = 5)

Nodes <- bind_rows(Nodes_1, Nodes)

Non_Members <- unique(c(coauthor_network$author, coauthor_network$coauthors))
Non_Members <- Non_Members[!(Non_Members %in% Nodes$name)]

Nodes_2 <- data.frame(name = Non_Members, group = 3, size = 1)

Nodes <- bind_rows(Nodes, Nodes_2)

Nodes$ID <- 1:nrow(Nodes)
Nodes$ID <- Nodes$ID -1

ForLinksSource <- Nodes %>% rename(author = name, source = ID, group_source = group) %>% dplyr::select(-size)
Links <- ForLinksSource %>% right_join(coauthor_network)

ForLinksTarget <- Nodes %>% rename(coauthors = name, target = ID, group_target = group)
Links <- ForLinksTarget %>% right_join(Links) %>% mutate(value = group_source + group_target) %>% select(source, target, value)

forceNetwork(Links = Links, Nodes = Nodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 1, zoom = TRUE,
             charge = -10,
             legend = T)


p <- simpleNetwork(coauthor_network, height="100px", width="100px", charge = -10)
p


Pubs <- Ids$`User (After user= up to &hl)` %>% 
  purrr::map(scholar::get_publications)  %>% 
  reduce(bind_rows)


# Load data
data(MisLinks)
data(MisNodes)

forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 0.4, zoom = TRUE)

# Create graph with legend and varying node radius
forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Nodesize = "size",
             radiusCalculation = "Math.sqrt(d.nodesize)+6",
             Group = "group", opacity = 0.4, legend = TRUE)

# Create graph directed arrows
forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 0.4, arrows = TRUE)





hairColourData <- matrix(c(11975,  1951,  8010, 1013,
                           5871, 10048, 16145,  990,
                           8916,  2060,  8090,  940,
                           2868,  6171,  8045, 6907),
                         nrow = 4)

chordNetwork(Data = hairColourData, 
             width = 500, 
             height = 500,
             colourScale = c("#000000", 
                             "#FFDD89", 
                             "#957244", 
                             "#F26223"),
             labels = c("red", "brown", "blond", "gray"))
  