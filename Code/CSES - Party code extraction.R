#### Party code extraction for CSES - Integrated module 1-5 #####
## May 22 - 2025

library(stringr)


# Read all lines
filepath <- here::here("CSES data", "cses_imd_codebook_part4_parties_coalitions_leaders_alphabetical_codes.txt")

# Read all lines
lines <- readLines(filepath, encoding = "UTF-8")

# Initialize
country <- NA
df <- data.frame(Country=character(), Party_Code=character(), Party_Name=character(), stringsAsFactors=FALSE)

# Regex patterns
country_pattern <- "^>>> ALPHABETICAL PARTY/COALITION CODES BY CSES MODULE: (.+)$"
# Pattern to capture party name
party_pattern <- "^([0-9]{7})\\.\\s+(.+?)\\s{2,}[-A-Z\\s\\-/]+$"

df <- data.frame(Country=character(), Party_Code=character(), Party_Name=character(), stringsAsFactors=FALSE)
country <- NA

for (line in lines) {
        if (str_detect(line, country_pattern)) {
                country <- str_match(line, country_pattern)[,2]
        }
        if (!is.na(country) && str_detect(line, party_pattern)) {
                parts <- str_match(line, party_pattern)
                code <- parts[,2]
                name <- parts[,3]
                df <- rbind(df, data.frame(Country=country, Party_Code=code, Party_Name=name, stringsAsFactors=FALSE))
        }
}



head(df, 10)


## Manual corrections

# Country names
df[df$Country == "CZECH REPUBLIC/",]$Country <- "CZECH REPUBLIC"

# Party names

# Remove a few trailing dashes
df$Party_Name <- sub("\\s*-\\s*$", "", df$Party_Name)

#Remove final spaces
df$Party_Name <- sub("\\s+$", "", df$Party_Name)

# Incomplete names in extraction. Corrections
df[df$Party_Name == "Arise the Republic (DLR) /",]$Party_Name <- "Arise the Republic (DLR) / France Arise (DLF)"
df[df$Party_Name == "Palmer United Party (PUP) /",]$Party_Name <- "Palmer United Party (PUP) / United Australia Party (UAP)"


# Save data
rio::export(df, here::here("CSES_Party Codes.xlsx"), format = "xlsx")



