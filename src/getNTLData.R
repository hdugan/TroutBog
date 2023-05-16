# GET LTER data
library(NTLlakeloads)
library(tidyverse)
library(lubridate)

##### Get secchi #####
tb.secchi = loadLTERsecchi() |> 
  filter(lakeid == 'TB')

ggplot(tb.secchi) +
  geom_point(aes(x = sampledate, y = secnview))

##### Get nutrients #####
tb.nutrients = loadLTERnutrients() |> 
  filter(lakeid == 'TB')

ggplot(tb.nutrients) +
  geom_point(aes(x = sampledate, y = doc))

##### Get temp/DO #####
tb.temp = loadLTERtemp() |> 
  filter(lakeid == 'TB')

ggplot(tb.temp |>  filter(depth == 6)) +
  geom_point(aes(x = sampledate, y = wtemp))


##### Get zooplankton #####
# Download northern lake zooplankton data from EDI #
# Package ID: knb-lter-ntl.37.36 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Zooplankton - Trout Lake Area 1982 - current.

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/37/36/c4b652eea76cd431ac5fd3562b1837ee" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")
tb.zoops =  read_csv(infile1) |> 
  filter(lakeid == 'TB') |> 
  mutate(code = floor(species_code/10000)) |>
  mutate(zoopGroup = case_when(code == 1 ~ 'copepod nauplii',
                               code == 2 ~ 'copepod',
                               code == 3 ~ 'calanoid',
                               code == 4 ~ 'harpacticoid',
                               code == 5 ~ 'cladocera',
                               code == 6 ~ 'rotifer',
                               code == 7 ~ 'unknown',
                               code == 8 ~ 'unknown',
                               code == 9 ~ 'unknown')) |> 
  # filter(code %in% c(2,3,4,5)) |>  # cladocera and copepods
  rename(sampledate = sample_date)

tb.total.zoops = tb.zoops |> group_by(sampledate, zoopGroup) |>
  summarise(density.total = sum(density))

ggplot(tb.total.zoops) +
  geom_col(aes(x = sampledate, y = density.total, fill = zoopGroup), width = 30)

##### Get ions #####
tb.ions = loadLTERions() |> 
  filter(lakeid == 'TB')

ggplot(tb.ions) +
  geom_point(aes(x = sampledate, y = ca))



