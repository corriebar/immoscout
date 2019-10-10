library(tidyverse)
library(naniar)   # for handling NA values
library(here)

read_all_files <- function(path) {
    file_list <- list.files(path, full.names = TRUE)



    # str_extract(file_list, "page \\d+.csv") %>% head

    # note: not all columns are contained in all csv-files, 
    # especially columns that are barely filled
    # such as eg energyEfficiencyClass

    df <- file_list %>% 
      map_df(read_csv, 
             col_types = cols(`zipCode` = col_character(),
                              `geo_plz` = col_character(),
                              `houseNumber` = col_character(),
                              `energyEfficiencyClass` = col_character(),
                              `objectnumber` = col_character() ),
             na=c("", "NA", "null")) %>%
      select(c("regio1", "serviceCharge", "heatingType", "telekomTvOffer",
                           "telekomHybridUploadSpeed", "newlyConst", "balcony", "electricityBasePrice",
                           "picturecount", "pricetrend", "telekomUploadSpeed",
                           "totalRent", "yearConstructed", "electricityKwhPrice",
                           "scoutId", "noParkSpaces", "firingTypes", "hasKitchen",
                           "geo_bln", "cellar", "yearConstructedRange", "baseRent", "houseNumber",
                           "livingSpace", "geo_krs", "zipCode", "condition", "interiorQual",
                           "petsAllowed", "streetPlain", "lift", "baseRentRange",
                           "typeOfFlat", "geo_plz", "noRooms", "assistedLiving",
                           "thermalChar", "geo_land", "floor", "numberOfFloors",
                           "noRoomsRange", "garden", "livingSpaceRange", "regio2", "regio3",
                           "description", "facilities", "heatingCosts", "energyEfficiencyClass",
                           "lastRefurbish")) %>%
      distinct(scoutId, .keep_all = TRUE)
    
}

df18 <- read_all_files(here::here("data/sep_2018"))

df18$date <- "Sep18"

# actually, there were some new columns added between september and may, but I'll ignore them
df19 <- read_all_files(here::here("data/may_2019"))

df19$date <- "May19"

df_oct19 <- read_all_files(here::here("data/oct_2019"))

df_oct19$date <- "Oct19"


df <- rbind(df18, df19, df_oct19) %>% 
  sample_frac(size=1) %>% 
  distinct(scoutId, .keep_all = TRUE)

rm(df18)
rm(df19)
rm(df_oct19)

skimr::skim(df)


# Change logical cols from character "y"/"n" to `TRUE`/`FALSE`.

logical_cols <- c("newlyConst", "balcony", "hasKitchen", "cellar", "lift", "assistedLiving", "garden")

df <- df %>%
  mutate_at(.vars=logical_cols, .funs=recode, y=TRUE, n=FALSE)

# Replace `no_information` with `NA`.

no_inf_cols <- c("heatingType", "firingTypes", "houseNumber", "condition", "interiorQual", "petsAllowed", "streetPlain", "typeOfFlat")
df <- df %>%
  mutate_at(.vars=no_inf_cols, .funs=~na_if(., "no_information" ) )


nrow(df)
# Throwing away unreasonable data:

df <- df %>%
  filter(lastRefurbish >= yearConstructed |
           is.na(lastRefurbish) |
           is.na(yearConstructed)) %>%     # refurbished before construction -> throw away
  select(-assistedLiving, -geo_land)  %>%      # throw away columns with just one value
  select(-zipCode)              # information already included in geo_plz

nrow(df)


# Recode Internet:

internet_cols <- c("telekomHybridUploadSpeed", "telekomUploadSpeed")
df <- df %>%
  mutate_at(.vars=internet_cols, .funs=recode, 
            `10 MBit/s`=10,
            `40 MBit/s`=40,
            `2,4 MBit/s`=2.4,
            `5 MBit/s` =5,
            `1 MBit/s`=1,
            `4 MBit/s`=4,
            `100 MBit/s`=100) 



write_csv(df, here::here("data/immo_data.csv"))







