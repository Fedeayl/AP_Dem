library(tidyverse)
library(stringdist)


#### MERGING PARTY CODES ####
# download and read Party Facts mapping table
file_name <- "partyfacts-mapping.csv"
if( ! file_name %in% list.files()) {
        url <- "https://partyfacts.herokuapp.com/download/external-parties-csv/"
        download.file(url, file_name)
}
partyfacts_raw <- read_csv(file_name, guess_max = 50000)
partyfacts <- partyfacts_raw |> filter(! is.na(partyfacts_id))

# show and select available datasets
cat(paste(unique(partyfacts$dataset_key), collapse='\n'))
dataset_1 <- 'vparty'
dataset_2 <- 'cses'

# merge two datasets abd select variables
first <- partyfacts[partyfacts$dataset_key == dataset_1, ]
second <- partyfacts[partyfacts$dataset_key == dataset_2, ]

merge_table <- merge(first, second, by='partyfacts_id', all=TRUE)

df <- merge_table[!is.na(merge_table$dataset_key.y),]
df <- merge_table[!is.na(merge_table$dataset_key.x),]

df <- df[!is.na(df$dataset_key.y),] # Only cases with existing values on CSES

df <- dplyr::select(df, country.x, dataset_key.x, dataset_party_id.x, name_short.x, name_english.x,
              year_first.x, year_last.x, share.x, share_year.x,
              partyfacts_id,
              country.y, dataset_key.y, dataset_party_id.y, name_short.y, name_english.y,
              year_first.y, year_last.y,  share.y, share_year.y)


## Verify matched cases

# Matching similars
threshold <- 15  # lower means more similar

# Create a logical vector where country matches and party names are similar by distance
matches <- df$country.x == df$country.y & 
        stringdist(df$name_english.x, df$name_english.y, method = "lv") <= threshold

# I will save the not-matching cases to manually classify them
dfdiff <- df[!matches, ]
dfdiff <- select(dfdiff,country.x, dataset_party_id.x, name_english.x, name_english.y, dataset_party_id.y)
rio::export(dfdiff, here::here("Aux Data","difereces_on_matching.xlsx", format="xlsx"))

# After manual labeling on differences on matching:
manual <- rio::import(here::here("Aux Data","difereces_on_matching_MANUAL CLASS.xlsx"))

# Now, I will delete from the original matching database the ones that felt in the verification process
delete <- manual[manual$match_manual == 0,]
delete <- select(delete, dataset_party_id.x, dataset_party_id.y)


df <- df[!paste(df$dataset_party_id.x, df$dataset_party_id.y) %in%
                 paste(delete$dataset_party_id.x, delete$dataset_party_id.y), ]


# We ended with 469 parties 
df <- rio::export(df,here::here("Aux Data","CSES+VPARTY_matched_final.csv"), format = "csv")



