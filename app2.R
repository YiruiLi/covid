library(tidyverse)
library(vroom)
library(sf)
library(tigris)
library(leaflet)
library(htmlwidgets)

download.file(url = "https://github.com/nychealth/coronavirus-data/archive/master.zip",
              destfile = "coronavirus-data-master.zip")
unzip(zipfile = "coronavirus-data-master.zip")

percentpos <- vroom("coronavirus-data-master/trends/percentpositive-by-modzcta.csv")
caserate <- vroom("coronavirus-data-master/trends/caserate-by-modzcta.csv")
testrate <- vroom("coronavirus-data-master/trends/testrate-by-modzcta.csv")
modzcta <- st_read("coronavirus-data-master/Geography-resources/MODZCTA_2010.shp")
zcta_conv <- vroom("coronavirus-data-master/Geography-resources/ZCTA-to-MODZCTA.csv",delim = ",")

caserates <- caserate %>% select(-c(2:7))
caserates_long <- caserates %>% pivot_longer(2:178,names_to = "modzcta",names_prefix = "CASERATE_",
                                            values_to = "caserate")

percentpositives <- percentpos %>% select(-c(2:7))
percentpos_long <- percentpositives %>% pivot_longer(2:178,names_to = "modzcta",names_prefix = "PCTPOS_",
                                            values_to = "pctpos")

testrates <- testrate %>% select(-c(2:7))
testrates_long <- testrates %>% pivot_longer(2:178,names_to = "modzcta",names_prefix = "TESTRATE_",
                                             values_to = "testrate")

all <- caserates_long %>% 
  left_join(percentpos_long, by = c("week_ending", "modzcta")) %>%
  left_join(testrates_long, by = c("week_ending", "modzcta"))

all_modzcta <- geo_join(modzcta,all,"MODZCTA","modzcta",how = "inner")

all_modzcta$week_ending <- as.Date(all_modzcta$week_ending, format = "%m%d%y")

saveRDS(all_modzcta, "all_modzcta.RDS")

all_modzcta %>% ggplot(aes(x=as.numeric(caserate)))+
  geom_histogram(bins = 20, fill = "#69b3a2", color = "blue")

labels <- sprintf("<strong>%s</strong><br/%g> cases per 100,000 people",
                  all_modzcta$MODZCTA, all_modzcta$caserate) %>%
                  lapply(htmltools::HTML)

pal <- colorBin(palette = "OrRd", 10, domain = all_modzcta$caserate)

map_interactive <- all_modzcta %>% 
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet() %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(label = labels,
              stroke = FALSE,
              smoothFactor = 0.5,
              apacity = 1,
              fillOpacity = 0.7,
              fillColor = ~ pal(caserate),
              highlightOptions = hilightOptions(weight = 5,
                                                fillOpacity = 1,
                                                color = "black",
                                                apacity = 1,
                                                bringToFront = TRUE)) %>%
  addLegend("bottomright", 
            pal = pal,
            values = ~ caserate,
            title = "Cases Per 100,000",
            apacity = 0.7)
  
saveWidget(l, "covid_caserate_map.html")
  
  