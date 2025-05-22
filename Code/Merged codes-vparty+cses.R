
# download and read Party Facts mapping table
file_name <- 'partyfacts-mapping.csv'
if( ! file_name %in% list.files())
        url <- 'https://partyfacts.herokuapp.com/download/external-parties-csv/'
download.file(url, file_name)
partyfacts <- read.csv(file_name, as.is=TRUE)  # maybe conversion of character encoding

# show and select available datasets
cat(paste(unique(partyfacts$dataset_key), collapse='\n'))
dataset_1 <- 'vparty'
dataset_2 <- 'cses'

# merge two datasets selected
first <- partyfacts[partyfacts$dataset_key == dataset_1, ]
second <- partyfacts[partyfacts$dataset_key == dataset_2, ]
merge_table <- dplyr::right_join(first, second, by='partyfacts_id')
df <- merge_table[!is.na(merge_table$dataset_key.y),]
df <- merge_table[!is.na(merge_table$dataset_key.x),]

df <- dplyr::select(df, country.x, dataset_key.x, dataset_party_id.x, name_short.x, name_english.x,
              year_first.x, year_last.x, share.x, 
              partyfacts_id,
              country.y, dataset_key.y, dataset_party_id.y, name_short.y, name_english.y,
              year_first.y, year_last.y,  share.y, share_year.y)


# write results into file with dataset names in file name
rio::export(df, "merged_codes.xlsx", format = "xlsx")



