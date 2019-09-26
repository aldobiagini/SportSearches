library(tidyverse)
library(jsonlite)
library(urltools)

searches <- fromJSON("searches.json", flatten=TRUE)
searchHits <- searches$hits

#claning up the data
searchSimple <- searchHits %>%
  mutate(t = `_source.@timestamp`, q = `_source.@fields.request`) %>%
  mutate(market = str_match(q, "market=(.*?)&")[,2], locale = str_match(q, "locale=(.*?) ")[,2]) %>%
  filter(is.na(q) == FALSE) %>%
  mutate(q = url_decode(q)) %>%
  mutate(q = str_replace(q, "GET ", "")) %>%
  mutate(q = str_match(q, "variables=(.*?)&")[,2]) %>%
  mutate(q = str_replace(q, "(\\{\"query\":\")", "")) %>%
  mutate(q = str_replace(q, "(\"\\})", "")) %>%
  mutate(t = str_replace(t, "T", " ")) %>%
  mutate(t = as.integer(as.integer(as.POSIXct(t)) / 60), len = str_length(q)) %>%
  select(t, q, len, market, locale) %>%
  group_by(t) %>%
  filter(len == max(len)) %>%
  arrange(t)

#calculate distances, increasing the substitution cost to 3
e <- adist(searchSimple$q, ignore.case = TRUE, costs = c(i=1,d=1,s=3)) #i = insertion, d = deletion, s = substitution
rownames(e) <- searchSimple$q

#splitting the data into 600 clusters
hc <- hclust(as.dist(e))
df <- as_tibble(data.frame(searchSimple$q, cutree(hc, k=600), searchSimple$market, searchSimple$locale))
colnames(df) <- c("query", "cluster", "market", "locale")

#Count results per cluster
topClusters <- df %>% 
  group_by(cluster) %>% 
  summarise(n = n()) %>%
  top_n(n = 50)

# filter searches of top clusters
filteredSearches <- df %>%
  inner_join(topClusters, by = "cluster") %>%
  arrange(-n, cluster)

#export
write_csv(filteredSearches, "Top sport searches last week.csv")

