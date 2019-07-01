# package source: https://github.com/ModelOriented/DrWhy
library(dplyr)
library(visNetwork)

packages <- data.frame(available.packages(), stringsAsFactors = FALSE)

s2v <- function(x)
  gsub(pattern = ",", "", x) %>% 
  gsub(pattern = "\n", " ", .) %>% 
  gsub(pattern = "\\s*\\([^\\)]+\\)", "", .) %>% 
  strsplit(split = " ") %>% 
  unlist

drWhy_packages <- readLines("drWhy-packages.txt")

dep_df <- lapply(drWhy_packages, function(ith_package) {
  rbind(data.frame(package = ith_package, 
                   type = "imports",
                   other_packages = s2v(filter(packages, Package == ith_package)[["Imports"]]), stringsAsFactors = FALSE),
        data.frame(package = ith_package, 
                   type = "suggests",
                   other_packages = s2v(filter(packages, Package == ith_package)[["Suggests"]])), stringsAsFactors = FALSE)
}) %>% 
  do.call(rbind, .)

nodes <- dep_df[c("package", "other_packages")] %>% 
  unlist %>% 
  unique %>% 
  data.frame(id = 1L:length(.), label = ., stringsAsFactors = FALSE) %>% 
  mutate(shape = ifelse(label %in% drWhy_packages, "box", "ellipse"))

nodes_dic <- 1L:nrow(nodes) %>% setNames(nodes[["label"]])
edges <- mutate(dep_df, 
                from = nodes_dic[package],
                to = nodes_dic[other_packages],
                color = ifelse(type == "imports", "red", "blue")) %>% 
  select(from, to, color)

net <- visNetwork(nodes, edges, height = 1200, width = 1200) %>%
  visEdges(arrows = "from")  %>% 
  visLayout(randomSeed = 1000) 

visSave(net, file = "drwhy.html")
