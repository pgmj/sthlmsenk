# test KOLADA API

### https://github.com/Hypergene/kolada

# Data queries are on the following forms, the form where all entities are given:
#   
#   /v2/data/kpi/<KPI>/municipality/<MUNICIPALITY_ID>/year/<PERIOD>
#   
#   Example: http://api.kolada.se/v2/data/kpi/N00945/municipality/1860
# 
# KPI, MUNICIPALITY_ID and PERIOD can all be comma separated strings. 
# The URL length is the limit which differs across browsers.

### https://github.com/Hypergene/kolada/blob/master/examples/kommun_i_korthet.rst

# https://api.kolada.se/v2/municipality?title=vaxholm
# {"count": 1, "values": [{"id": "0187", "title": "Vaxholm", "type": "K"}]}
# {"count": 1, "values": [{"id": "0115", "title": "Vallentuna", "type": "K"}]}
# {"count": 2, "values": [{"id": "0001", "title": "Region Stockholm", "type": "L"}, {"id": "0180", "title": "Stockholm", "type": "K"}]}
# {"count": 1, "values": [{"id": "0162", "title": "Danderyd", "type": "K"}]}
# {"count": 1, "values": [{"id": "0160", "title": "Täby", "type": "K"}]}

# https://themockup.blog/posts/2020-12-13-extracting-json-from-websites-and-public-apis-with-r/
# N15428	Elever i åk 9 som är behöriga till yrkesprogram, hemkommun, andel (%)  
# https://themockup.blog/posts/2020-12-13-extracting-json-from-websites-and-public-apis-with-r/#create-a-function

library(httr)
library(arrow)
library(tidyverse)
library(rjson)
library(glue)

# get list of all KPI
kpiList = GET("http://api.kolada.se/v2/kpi_groups")
allKPI <- rjson::fromJSON(rawToChar(kpiList$content))

kpi <- allKPI$values %>% 
  enframe() %>%
  unnest_auto(value) %>% 
  unnest_auto(members) %>% 
  unnest_auto(members) %>% 
  select(member_id,member_title)

kpiLista <- as.character(expression(N11821,N11041,N15030,N17813,N15813,N17473,N17538,N15428,N17625,N17620,N15557,N15613,N15533,N15643))

finns <- kpi %>% 
  filter(member_id %in% kpiLista) %>% 
  distinct(member_id,member_title, .keep_all = TRUE)
  
saknas <- setdiff(kpiLista,finns$member_id)

# library(xlsx)
# write list to file and manually add the three missing KPI's, then we can just read the file if needed
# write.xlsx(finns, "data/koladaKPIlist.xls")

allaKPI <- read.xlsx("../DIDapp/data/koladaKPIlist.xls", sheetIndex = 1)

KPInamn <- allaKPI$kpiDesc
names(KPInamn) <- allaKPI$kpiNr

# list of municipalities and their code id's retrived from SCB:
# https://www.scb.se/hitta-statistik/regional-statistik-och-kartor/regionala-indelningar/lan-och-kommuner/lan-och-kommuner-i-kodnummerordning/
# allaKommunID <- read.xlsx("../data/registerdata/kommunlankod_2023.xls", sheetIndex = 1)
# # make the file useful by removing rows and replacing variable names
# allaKommunID <- allaKommunID[-c(1:4),] %>% 
#   janitor::row_to_names(1)


# start here --------------------------------------------------------------



# vector of correct id's
Kommunnamn <- c("Vaxholm","Vallentuna","Stockholm","Danderyd","Täby","Södertälje")
# # vector with incorrect id's (c1 to c100) is input as names of vector with correct id's
names(Kommunnamn) <- c("0187","0115","0180","0162","0160","0181")

# get data from KOLADA
didkolada = GET("http://api.kolada.se/v2/data/kpi/N11821,N11041,N15030,N17813,N15813,N17473,N17538,N15428,N17625,N17620,N15557,N15613,N15533,N15643/municipality/0187,0115,0180,0162,0160,0181/year/2014,2015,2016,2017,2018,2019,2020,2021,2022")

# decode
kolada <- rjson::fromJSON(rawToChar(didkolada$content))

# read and transform data to readable df
kolada$values %>% 
  enframe() %>%
  unnest_auto(value) %>% 
  unnest_auto(values) %>% 
  unnest_auto(values) %>% 
  mutate(Kommun = dplyr::recode(municipality, !!!Kommunnamn), .before = "kpi") %>%
  mutate(KPI = dplyr::recode(kpi, !!!KPInamn), .before = "kpi") %>%
  select(!any_of(c("name", "status", "municipality", "count"))) %>%
  mutate(Kön = dplyr::recode(gender, 
                             K = "Flicka",
                             M = "Pojke",
                             T = "Alla")) %>%
  select(!gender) %>% 
  rename(År = period,
         Andel = value) %>% 
  as.data.frame() %>% 
  write_parquet(., glue("../data/{Sys.Date()}_koladaData.parquet"))

library(rKolada)
# https://lchansson.github.io/rKolada/articles/a-quickstart-rkolada.html
# KPI codes
#kpis <- get_kpi(cache = FALSE)
#write_parquet(kpis, glue("../data/{Sys.Date()}_koladaKPIs.parquet"))
allKPI <- read_parquet("../data/2023-01-10_koladaKPIs.parquet")
#munic <- get_municipality(cache = FALSE)
#write_parquet(munic, glue("../data/{Sys.Date()}_koladaKommunID.parquet"))
allaKommunID <- read_parquet("../data/2023-01-10_koladaKommunID.parquet")

# sample filtering on content, where we look for the words skola or Skola in the
# variable "operating_area", and store them in a vector
operating_area_Skola <- allKPI %>% 
  distinct(operating_area) %>% 
  filter(grepl('skola|Skola',operating_area)) %>% 
  pull()
# then we filter to retrieve all KPI related to the operating_areas identified
# and write output (KPI description and ID) to an excel file
allKPI %>% 
  filter(operating_area %in% operating_area_Skola) %>% 
  select(id,description) %>% 
  write.xlsx("../data/KOLADAskolKPIer.xls")

# this could maybe be done more easily using the rKolada package?
allKPI %>% 
  kpi_search("skola") %>% 
  kpi_minimize(remove_undocumented_columns = TRUE, remove_monotonous_data = TRUE)

# more on filtering on string content:
# https://www.geeksforgeeks.org/filtering-row-which-contains-a-certain-string-using-dplyr-in-r/