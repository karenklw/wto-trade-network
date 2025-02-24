library(igraph)
library(dplyr)
library(ggplot2)
library(tidygraph)
library(ggraph)
library(scales)
library(tidyverse)
library(gridExtra)
library(grid)
library(gtable)
library(btergm)
library(intergraph)

data <- read.csv("wto-trade-network/WtoData_20250209133853.csv")
head(data)

info <- data.frame(
  "Variable" = names(data),
  "Type" = sapply(data, class),
  "Unique Values" = sapply(data, function(x) length(unique(x))),
  "Missing Values" = sapply(data, function(x) sum(is.na(x)))
)
info

countries <- unique(c(data$Reporting.Economy.ISO3A.Code, data$Partner.Economy.ISO3A.Code))
length(countries) # 218 unique countries
# filter out non-country entities (keep the 200 from Tismos_codes.xlsx)
members <- c(
  "ARE", "AFG", "ATG", "AIA", "ALB", "ARM", "ANT", "AGO", "ARG", "AUT", "AUS", "ABW", "AZE", 
  "BIH", "BRB", "BGD", "BEL", "BFA", "BGR", "BHR", "BDI", "BEN", "BMU", "BRN", "BOL", "BRA", 
  "BHS", "BTN", "BWA", "BLR", "BLZ", "CAN", "COD", "CAF", "COG", "CHE", "CIV", "CHL", "CMR", 
  "CHN", "COL", "CRI", "CUB", "CPV", "CUW", "CYP", "CZE", "DEU", "DJI", "DNK", "DMA", "DOM", 
  "DZA", "ECU", "EST", "EGY", "ERI", "ESP", "ETH", "FIN", "FJI", "FRO", "FRA", "GAB", "GBR", 
  "GRD", "GEO", "GHA", "GMB", "GIN", "GNQ", "GRC", "GTM", "GNB", "GUY", "HKG", "HND", "HRV", 
  "HTI", "HUN", "IDN", "IRL", "ISR", "IND", "IRQ", "IRN", "ISL", "ITA", "JAM", "JOR", "JPN", 
  "KEN", "KGZ", "KHM", "KIR", "COM", "KNA", "PRK", "KOR", "KWT", "CYM", "KAZ", "LAO", "LBN", 
  "LCA", "LKA", "LBR", "LSO", "LTU", "LUX", "LVA", "LBY", "MAR", "MDA", "MNE", "MDG", "MKD", 
  "MLI", "MMR", "MNG", "MAC", "MRT", "MSR", "MLT", "MUS", "MDV", "MWI", "MEX", "MYS", "MOZ", 
  "NAM", "NCL", "NER", "NGA", "NIC", "NLD", "NOR", "NPL", "NZL", "OMN", "PAN", "PER", "PYF", 
  "PNG", "PHL", "PAK", "POL", "PRT", "PRY", "QAT", "ROM", "SRB", "RUS", "RWA", "SAU", "SLB", 
  "SYC", "SDN", "SWE", "SGP", "SVN", "SVK", "SLE", "SEN", "SOM", "SUR", "STP", "SLV", "SXM", 
  "SYR", "SWZ", "TCA", "TCD", "TGO", "THA", "TJK", "TLS", "TKM", "TUN", "TON", "TUR", "TTO", 
  "TUV", "TWN", "TZA", "UKR", "UGA", "USA", "URY", "UZB", "VCT", "VEN", "VNM", "VUT", "WSM", 
  "YEM", "SCG", "ZAF", "ZMB", "ZWE"
)
n <- length(members)
filtered_data <- data %>% filter(data$Reporting.Economy.ISO3A.Code %in% members & data$Partner.Economy.ISO3A.Code %in% members)

# check if all 'Unit.Code' values are the same
unique_unit_codes <- unique(filtered_data$Unit.Code)
print(unique_unit_codes) # filtered reports are in USD, not USM (million USD)

# check if 'Unit' is "US$" for all reports
unique_units <- unique(filtered_data$Unit)
print(unique_units)  # filtered reports are in 'US$', not 'Million US dollar'

unique_period <- unique(filtered_data$Period)
print(unique_period)

# normalize to be between 1 and 10 for readability of plot
filtered_data$ScaledEdgeWidth <- (filtered_data$Value - min(filtered_data$Value)) / 
                                 (max(filtered_data$Value) - min(filtered_data$Value)) * 9 + 1

categories <- unique(filtered_data$Indicator.Category)
print(categories) # only imports, so reporting economeis are the importers

str(filtered_data)

# rearrange so first 2 cols are the country codes
filtered_data <- filtered_data %>%
  select(Reporting.Economy.ISO3A.Code, Partner.Economy.ISO3A.Code, everything())

# remove self-edges
filtered_data <- filtered_data %>% filter(Reporting.Economy.ISO3A.Code != Partner.Economy.ISO3A.Code)

# remove data from years 2023 and 2024
filtered_data <- filtered_data %>% filter(Year != 2023 & Year != 2024)

trade <- graph_from_data_frame(filtered_data, directed = TRUE)
print(trade)
summary(trade)
cat("Number of nodes:", vcount(trade), "\n")
cat("Number of edges:", ecount(trade), "\n")


# find the 18 biggest importers and exporters for each year and plot their subgraphs
years <- unique(filtered_data$Year)
for (year in years) {
  yearly_data <- filtered_data %>% filter(Year == year)

  format_billions <- function(x) {
    paste0(format(round(x / 1e9, 3), nsmall = 3), "B")
  }
  
  top_reporting <- yearly_data %>%
    group_by(Reporting.Economy.ISO3A.Code, Reporting.Economy) %>%
    summarise(TotalValue = sum(Value), .groups = 'drop') %>%
    arrange(desc(TotalValue)) %>%
    slice(1:18) %>%
    mutate(TotalValue = format_billions(TotalValue)) %>%
    rename(`ISO3A Code` = Reporting.Economy.ISO3A.Code,
           `Country Name` = Reporting.Economy,
           `Total Value Imported (Billions $USD)` = TotalValue)

  top_partner <- yearly_data %>%
    group_by(Partner.Economy.ISO3A.Code, Partner.Economy) %>%
    summarise(TotalValue = sum(Value), .groups = 'drop') %>%
    arrange(desc(TotalValue)) %>%
    slice(1:18) %>%
    mutate(TotalValue = format_billions(TotalValue)) %>%
    rename(`ISO3A Code` = Partner.Economy.ISO3A.Code,
           `Country Name` = Partner.Economy,
           `Total Value Exported (Billions $USD)` = TotalValue)

  top_reporting_table <- tableGrob(top_reporting, rows = NULL)
  top_partner_table <- tableGrob(top_partner, rows = NULL)

  title_exporters <- textGrob(paste("Top 18 Exporters in", year), gp = gpar(fontsize = 14, fontface = "bold"))
  title_importers <- textGrob(paste("Top 18 Importers in", year), gp = gpar(fontsize = 14, fontface = "bold"))

  exporters_grob <- gtable_add_rows(top_partner_table, heights = unit(1, "lines"), pos = 0)
  exporters_grob <- gtable_add_grob(exporters_grob, title_exporters, t = 1, l = 1, r = ncol(exporters_grob))

  importers_grob <- gtable_add_rows(top_reporting_table, heights = unit(1, "lines"), pos = 0)
  importers_grob <- gtable_add_grob(importers_grob, title_importers, t = 1, l = 1, r = ncol(importers_grob))

  yearly_top_data <- yearly_data %>%
    filter(Reporting.Economy.ISO3A.Code %in% top_reporting$`ISO3A Code` & 
           Partner.Economy.ISO3A.Code %in% top_partner$`ISO3A Code`)
  
  yearly_trade <- graph_from_data_frame(yearly_top_data, directed = TRUE)
  
  V(yearly_trade)$name <- unique(c(yearly_top_data$Reporting.Economy.ISO3A.Code, yearly_top_data$Partner.Economy.ISO3A.Code))
  V(yearly_trade)$label <- V(yearly_trade)$name
  V(yearly_trade)$size <- 5
  V(yearly_trade)$color <- "yellow"
  E(yearly_trade)$weight <- yearly_top_data$Value
  E(yearly_trade)$ScaledEdgeWidth <- (E(yearly_trade)$weight - min(E(yearly_trade)$weight)) / 
                                     (max(E(yearly_trade)$weight) - min(E(yearly_trade)$weight)) * 9 + 1

  plot_filename <- paste0("wto-trade-network/images/trade_network_", year, ".png")
  png(plot_filename, width = 800, height = 600)
  plot(yearly_trade, 
       edge.width = E(yearly_trade)$ScaledEdgeWidth, 
       edge.arrow.size = 0.3,
       edge.color = "black",
       vertex.size = 5, 
       vertex.label.cex = 0.5, 
       vertex.label.color = "blue",
       main = paste("Top 18 Importers and Exporters in WTO Trade Network for Year", year))
  dev.off()

  exporters_table_filename <- paste0("wto-trade-network/images/top_exporters_", year, ".png")
  png(exporters_table_filename, width = 400, height = 500)
  grid.draw(exporters_grob)
  dev.off()

  importers_table_filename <- paste0("wto-trade-network/images/top_importers_", year, ".png")
  png(importers_table_filename, width = 400, height = 500)
  grid.draw(importers_grob)
  dev.off()
}

# clusters in 2022
yearly_data_2022 <- filtered_data %>% filter(Year == 2022)
yearly_trade_2022 <- graph_from_data_frame(yearly_data_2022, directed = TRUE)
  
V(yearly_trade_2022)$name <- unique(c(yearly_data_2022$Reporting.Economy.ISO3A.Code, yearly_data_2022$Partner.Economy.ISO3A.Code))
V(yearly_trade_2022)$label <- unique(c(yearly_data_2022$Reporting.Economy.ISO3A.Code, yearly_data_2022$Partner.Economy.ISO3A.Code))
V(yearly_trade_2022)$size <- 5
E(yearly_trade_2022)$weight <- yearly_data_2022$Value
E(yearly_trade_2022)$Frequency <- yearly_data_2022$Frequency
E(yearly_trade_2022)$color <- "gray"

clusters_2022 <- cluster_leading_eigen(yearly_trade_2022, weights = E(yearly_trade_2022)$Value)
V(yearly_trade_2022)$color <- membership(clusters_2022)

cluster_2022_filename <- "wto-trade-network/clustering/clusters_2022.png"
png(cluster_2022_filename, width = 800, height = 600)
plot(yearly_trade_2022, 
  edge.width = 1,
  edge.arrow.size = 0.2,
  edge.color = E(yearly_trade_2022)$color,
  vertex.size = 3, 
  vertex.label = NA, 
  main = paste("Clusters in WTO Arms Trade Network for 2022")
)
dev.off()

# tables of 2022 clusters
membership_df <- data.frame(
  ISO3A_Code = V(yearly_trade_2022)$name,
  Cluster = membership(clusters_2022)
)

reporting_names <- yearly_data_2022 %>%
  select(ISO3A_Code = Reporting.Economy.ISO3A.Code, Country_Name = Reporting.Economy) %>%
  distinct()

partner_names <- yearly_data_2022 %>%
  select(ISO3A_Code = Partner.Economy.ISO3A.Code, Country_Name = Partner.Economy) %>%
  distinct()

country_names <- bind_rows(reporting_names, partner_names) %>% distinct()
membership_df <- left_join(membership_df, country_names, by = "ISO3A_Code")
membership_df <- unique(membership_df)
cluster_ids <- unique(membership_df$Cluster)

for (cluster_id in cluster_ids) {
  cluster_table <- membership_df %>%
    filter(Cluster == cluster_id) %>%
    select(ISO3A_Code, Country_Name) %>%
    arrange(Country_Name)

  table_grob <- tableGrob(cluster_table, rows = NULL)

  title <- textGrob(paste("Cluster", cluster_id, "Countries (2022)"), gp = gpar(fontsize = 14, fontface = "bold"))

  table_with_title <- gtable_add_rows(table_grob, heights = unit(1.5, "lines"), pos = 0)
  table_with_title <- gtable_add_grob(table_with_title, title, t = 1, l = 1, r = ncol(table_with_title))

  table_filename <- paste0("wto-trade-network/clustering/cluster_", cluster_id, "_2022.png")
  png(table_filename, width = 500, height = 1500)
  grid.draw(table_with_title)
  dev.off()
}

# clustering for 2011
yearly_data_2011 <- filtered_data %>% filter(Year == 2011)
yearly_trade_2011 <- graph_from_data_frame(yearly_data_2011, directed = TRUE)
  
V(yearly_trade_2011)$name <- unique(c(yearly_data_2011$Reporting.Economy.ISO3A.Code, yearly_data_2011$Partner.Economy.ISO3A.Code))
V(yearly_trade_2011)$label <- unique(c(yearly_data_2011$Reporting.Economy.ISO3A.Code, yearly_data_2011$Partner.Economy.ISO3A.Code))
V(yearly_trade_2011)$size <- 5
E(yearly_trade_2011)$weight <- yearly_data_2011$Value
E(yearly_trade_2011)$Frequency <- yearly_data_2011$Frequency
E(yearly_trade_2011)$color <- "gray"

clusters_2011 <- cluster_leading_eigen(yearly_trade_2011, weights = E(yearly_trade_2011)$Value)
V(yearly_trade_2011)$color <- membership(clusters_2011)

cluster_2011_filename <- "wto-trade-network/clustering/clusters_2011.png"
png(cluster_2011_filename, width = 800, height = 600)
plot(yearly_trade_2011, 
  edge.width = 1,
  edge.arrow.size = 0.2,
  edge.color = E(yearly_trade_2011)$color,
  vertex.size = 3, 
  vertex.label = NA, 
  main = paste("Clusters in WTO Arms Trade Network for 2011")
)
dev.off()

# tables of 2011 clusters
membership_df <- data.frame(
  ISO3A_Code = V(yearly_trade_2011)$name,
  Cluster = membership(clusters_2011)
)

reporting_names <- yearly_data_2011 %>%
  select(ISO3A_Code = Reporting.Economy.ISO3A.Code, Country_Name = Reporting.Economy) %>%
  distinct()

partner_names <- yearly_data_2011 %>%
  select(ISO3A_Code = Partner.Economy.ISO3A.Code, Country_Name = Partner.Economy) %>%
  distinct()

country_names <- bind_rows(reporting_names, partner_names) %>% distinct()
membership_df <- left_join(membership_df, country_names, by = "ISO3A_Code")
membership_df <- unique(membership_df)
cluster_ids <- unique(membership_df$Cluster)

for (cluster_id in cluster_ids) {
  cluster_table <- membership_df %>%
    filter(Cluster == cluster_id) %>%
    select(ISO3A_Code, Country_Name) %>%
    arrange(Country_Name)

  table_grob <- tableGrob(cluster_table, rows = NULL)

  title <- textGrob(paste("Cluster", cluster_id, "Countries (2011)"), gp = gpar(fontsize = 14, fontface = "bold"))

  table_with_title <- gtable_add_rows(table_grob, heights = unit(1.5, "lines"), pos = 0)
  table_with_title <- gtable_add_grob(table_with_title, title, t = 1, l = 1, r = ncol(table_with_title))

  table_filename <- paste0("wto-trade-network/clustering/cluster_", cluster_id, "_2011.png")
  png(table_filename, width = 500, height = 1500)
  grid.draw(table_with_title)
  dev.off()
}

# statistics of trade network across years (num edges, num nodes, density, cluster coefficient)
stats <- data.frame(
  year = 2011:2022,
  num_nodes = integer(length(years)),
  num_edges = integer(length(years)),
  density = numeric(length(years)),
  cluster_coef = numeric(length(years))
)

for (year in years) {
  net <- filtered_data %>% filter(Year == year)
  netgraph <- graph_from_data_frame(net, directed = TRUE)
  stats[stats$year == year, "num_nodes"] <- vcount(netgraph)
  stats[stats$year == year, "num_edges"] <- ecount(netgraph)
  stats[stats$year == year, "density"] <- round(edge_density(netgraph), 3)
  stats[stats$year == year, "cluster_coef"] <- round(transitivity(netgraph, type = "global"), 3)
}
stats_table <- tableGrob(stats, rows = NULL)
stats_title <- textGrob("Summary of Arms Trade Network from 2011-2022", gp = gpar(fontsize = 14, fontface = "bold"))
stats_table_with_title <- gtable_add_rows(stats_table, heights = unit(1.5, "lines"), pos = 0)
stats_table_with_title <- gtable_add_grob(stats_table_with_title, stats_title, t = 1, l = 1, r = ncol(stats_table_with_title))

stats_filename <- "wto-trade-network/results/stats.png"
png(stats_filename, width = 800, height = 600)
grid.draw(stats_table_with_title)
dev.off()

# scatterplot to show changes in density and transitivity
stats_long <- stats %>%
  pivot_longer(cols = c(density, cluster_coef), names_to = "metric", values_to = "value")

ggplot(stats_long, aes(x = year, y = value, color = metric)) +
  geom_point(size = 3) +
  geom_line() +
  scale_x_continuous(breaks = stats$year) +
  labs(title = "Density and Transitivity by Year",
       x = "Year",
       y = "Value",
       color = "Metric") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        legend.background = element_rect(fill = "white", color = "white"),
        legend.key = element_rect(fill = "white", color = "white"))

plot_filename <- "wto-trade-network/results/density_transitivity_plot.png"
ggsave(plot_filename, width = 10, height = 6)

# temporal ERGM models

network_list <- list()

for (year in years) {
  yearly_data <- filtered_data %>%
    filter(Year == year) %>%
    group_by(Reporting.Economy.ISO3A.Code, Partner.Economy.ISO3A.Code) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")  # sum trade values because trade data has duplicate edges
  
  yearly_network <- network(yearly_data[, c("Reporting.Economy.ISO3A.Code", "Partner.Economy.ISO3A.Code")],
                            directed = TRUE, matrix.type = "edgelist")
  
  set.edge.attribute(yearly_network, "weight", yearly_data$Value)
  
  network_list[[as.character(year)]] <- yearly_network
}

# without covariates
model <- btergm(network_list ~ edges + mutual,
                R = 500)  # 500 MCMC simulations (bootstrap replications)
summary(model)


# importing World Bank data to add as covariates
wdi <- read.csv("wto-trade-network/worldbank/WDI.csv")
wdi_filtered <- wdi %>% filter(Country.Code %in% members)
valid_members <- unique(wdi_filtered$Country.Code)
length(valid_members)

# set GDP as vertex attribute directly
gdp_data <- wdi_filtered %>% filter(Series.Code == "NY.GDP.MKTP.CD")

for (year in names(network_list)) {
  yearly_network <- network_list[[year]]
  nodes <- network.vertex.names(yearly_network)

  gdp_values <- gdp_data %>%
    select(Country.Code, !!paste0("X", year, "..YR", year, ".")) %>%
    rename(GDP = !!paste0("X", year, "..YR", year, "."))

  gdp_vector <- setNames(as.numeric(gdp_values$GDP), gdp_values$Country.Code)

  gdp_for_network <- setNames(rep(0, length(nodes)), nodes)
  matching_nodes <- intersect(names(gdp_vector), nodes)
  gdp_for_network[matching_nodes] <- gdp_vector[matching_nodes]

  gdp_for_network[is.na(gdp_for_network)] <- 0 # hard-coded some GDPs to 0 due to missing data

  yearly_network <- set.vertex.attribute(yearly_network, "gdp", value = gdp_for_network)

  network_list[[year]] <- yearly_network
}
for (year in names(network_list)) {
  cat("Year:", year, "\n")
  print(sum(is.na(get.vertex.attribute(network_list[[year]], "gdp"))))  # Should be 0
}

# model with GDP as node covariate
model_gdp <- btergm(network_list ~ edges + mutual + nodecov("gdp"), R = 500)
summary(model_gdp)

# try log values of GDP
for (year in names(network_list)) {
    yearly_network <- network_list[[year]]
    gdp_values <- get.vertex.attribute(yearly_network, "gdp")
    set.vertex.attribute(yearly_network, "gdp_log", log1p(gdp_values))
    network_list[[year]] <- yearly_network
}
model_gdp_log <- btergm(network_list ~ edges + mutual + nodecov("gdp") + nodecov("gdp_log"), R = 500)
summary(model_gdp_log)

# model with GDP differences as node covariate
model_gdp_diff <- btergm(network_list ~ edges + mutual + nodecov("gdp") + nodecov("gdp_log") + absdiff("gdp") + absdiff("gdp_log"), R = 500)
summary(model_gdp_diff)

# include effects of forming mutual allies
model_gwesp <- btergm(network_list ~ edges + nodecov("gdp_log") + gwesp(0.5, fixed = TRUE), R = 500)
summary(model_gwesp)

# include temporal effects
model_gwesp_temporal <- btergm(network_list ~ edges + nodecov("gdp_log") + gwesp(0.5, fixed = TRUE) + timecov(), R = 500)
summary(model_gwesp_temporal)
