cses <- rio::import(here::here("CSES data/cses_imd.rdata"))

library(tidyr)
library(dplyr)

######## SYSTEM LEVEL POLARIZATION #######
# Select variables
data <- cses

data <- select(data, IMD1004, IMD1006_UNALPHA2, IMD1006_NAM, IMD1008_YEAR, # identifiers
               IMD3008_A, IMD3008_B, IMD3008_C, IMD3008_D, IMD3008_E, IMD3008_F, IMD3008_G, IMD3008_H, IMD3008_I, # like/dislike parties
               IMD3009_A, IMD3009_B, IMD3009_C, IMD3009_D, IMD3009_E, IMD3009_F, IMD3009_G, IMD3009_H, IMD3009_I, # like/dislike leader
               IMD3005_1, IMD3005_2, IMD3005_3, # feel close/a little closer to a party
               IMD5000_A, IMD5000_B, IMD5000_C, IMD5000_D, IMD5000_E, IMD5000_F, IMD5000_G, IMD5000_H, IMD5000_I, # party ident. number
               IMD5001_A, IMD5001_B, IMD5001_C, IMD5001_D, IMD5001_E, IMD5001_F, IMD5001_G, IMD5001_H, IMD5001_I, # vote share (to lower house)
)

data <- data %>%
        mutate(across(starts_with("IMD5001_"), ~ ifelse(. > 99, 0, .)))



vote_summary <- data %>%
        group_by(IMD1004) %>%
        slice(1) %>%  # Take one row per election
        ungroup() %>%
        select(IMD1004, IMD5001_A, IMD5001_B, IMD5001_C, IMD5001_D,
               IMD5001_E, IMD5001_F, IMD5001_G, IMD5001_H, IMD5001_I) %>%
        rowwise() %>%
        mutate(Total_Vote_Share = sum(c_across(starts_with("IMD5001_")), na.rm = TRUE)) %>%
        ungroup() %>% select(IMD1004, Total_Vote_Share)

delete <- vote_summary[vote_summary$Total_Vote_Share >100,]$IMD1004

data <- data[!data$IMD1004%in% delete,]


# STEP 1: filter by respondents of IMD3005_1 and IMD3005_2.
# We code as party supporters both those who feel close 
# and those who feel a little closer to the relevant party.(Addams et al., 2020:14)

data <- filter(data, IMD3005_1 ==1 | IMD3005_2 == 1)

# STEP 2: 

nas <- unique(data$IMD3005_3)
nas <- as.numeric(nas[grepl("^9", as.character(nas))])

# Identifying each party by their code
A. <- unique(data[!data$IMD5000_A %in% nas, ]$IMD5000_A)
B. <- unique(data[!data$IMD5000_B %in% nas, ]$IMD5000_B)
C. <- unique(data[!data$IMD5000_C %in% nas, ]$IMD5000_C)
D. <- unique(data[!data$IMD5000_D %in% nas, ]$IMD5000_D)
E. <- unique(data[!data$IMD5000_E %in% nas, ]$IMD5000_E)
F. <- unique(data[!data$IMD5000_F %in% nas, ]$IMD5000_F)
G. <- unique(data[!data$IMD5000_G %in% nas, ]$IMD5000_G)
H. <- unique(data[!data$IMD5000_H %in% nas, ]$IMD5000_H)
I. <- unique(data[!data$IMD5000_I %in% nas, ]$IMD5000_I)


# Replacing party code by "IMD5000_X"
data$IMD3005_3 <- ifelse(data$IMD3005_3 %in% A., "IMD5000_A", data$IMD3005_3)
data$IMD3005_3 <- ifelse(data$IMD3005_3 %in% B., "IMD5000_B", data$IMD3005_3)
data$IMD3005_3 <- ifelse(data$IMD3005_3 %in% C., "IMD5000_C", data$IMD3005_3)
data$IMD3005_3 <- ifelse(data$IMD3005_3 %in% D., "IMD5000_D", data$IMD3005_3)
data$IMD3005_3 <- ifelse(data$IMD3005_3 %in% E., "IMD5000_E", data$IMD3005_3)
data$IMD3005_3 <- ifelse(data$IMD3005_3 %in% F., "IMD5000_F", data$IMD3005_3)
data$IMD3005_3 <- ifelse(data$IMD3005_3 %in% G., "IMD5000_G", data$IMD3005_3)
data$IMD3005_3 <- ifelse(data$IMD3005_3 %in% H., "IMD5000_H", data$IMD3005_3)
data$IMD3005_3 <- ifelse(data$IMD3005_3 %in% I., "IMD5000_I", data$IMD3005_3)

# Only parties included in questions
p. <- c("IMD5000_A", "IMD5000_B", "IMD5000_C", "IMD5000_D", "IMD5000_E", 
        "IMD5000_F", "IMD5000_G", "IMD5000_H", "IMD5000_I")

data <- data[data$IMD3005_3 %in% p.,]


# STEP 3. Calculation of average in-party rating

# Before calculating, let's change the direction of the variable (Addams et al., 2020:15)
# To Make interpretations intuitive, we present results in which the feeling thermometer is reversed 
# such that a rating of 10 denotes to most intense negative party evaluation and 0 the most positive.

remove10 <- function(x){ifelse(x>10,NA,x)}
data$IMD3008_A <- remove10(data$IMD3008_A)
data$IMD3008_B <- remove10(data$IMD3008_B)
data$IMD3008_C <- remove10(data$IMD3008_C)
data$IMD3008_D <- remove10(data$IMD3008_D)
data$IMD3008_E <- remove10(data$IMD3008_E)
data$IMD3008_F <- remove10(data$IMD3008_F)
data$IMD3008_G <- remove10(data$IMD3008_G)
data$IMD3008_H <- remove10(data$IMD3008_H)
data$IMD3008_I <- remove10(data$IMD3008_I)


# Like scores fuera de rango → NA
data <- data %>%
        mutate(across(starts_with("IMD3008_"), ~ ifelse(. >= 0 & . <= 10, ., NA)))

# Vote share fuera de rango → NA
data <- data %>%
        mutate(across(starts_with("IMD5001_"), ~ ifelse(. >= 0 & . <= 100, ., NA)))

# Vector de sufijos de partidos
parties <- LETTERS[1:9]

# Normalizar vote shares por fila
normalize_votes <- function(row) {
        shares <- as.numeric(row[grepl("IMD5001_", names(row))])
        total <- sum(shares, na.rm = TRUE)
        
        if (is.na(total) || total == 0) return(rep(NA, length(shares)))
        
        return(shares / total * 100)  # mantener en escala 0–100
}

vote_cols <- grep("^IMD5001_", names(data), value = TRUE)

#data[vote_cols] <- t(apply(data[vote_cols], 1, normalize_votes))



# Función que calcula AP para una fila (individuo)
calc_affective_polarization <- function(row) {
        in_party_col <- row[["IMD3005_3"]]  # Ej: "IMD5000_B"
        if (is.na(in_party_col)) return(NA)
        
        # Obtener sufijo del partido propio (ej: "B")
        suffix <- gsub("IMD5000_", "", in_party_col)
        
        # Obtener feeling y voto del partido propio
        like_in <- suppressWarnings(as.numeric(row[[paste0("IMD3008_", suffix)]]))
        vote_in <- suppressWarnings(as.numeric(row[[paste0("IMD5001_", suffix)]]))
        
        # Validaciones
        if (is.na(like_in) || is.na(vote_in)) return(NA)
        if (vote_in <= 0 || vote_in >= 100) return(NA)
        if (like_in < 0 || like_in > 10) return(NA)
        
        # Convertir a proporción
        vote_in <- vote_in / 100
        
        # Inicializar suma
        suma <- 0
        
        for (s in parties) {
                if (s == suffix) next  # saltar in-party
                
                like_out <- suppressWarnings(as.numeric(row[[paste0("IMD3008_", s)]]))
                vote_out <- suppressWarnings(as.numeric(row[[paste0("IMD5001_", s)]]))
                
                # Validaciones
                if (is.na(like_out) || is.na(vote_out)) next
                if (vote_out <= 0 || vote_out >= 100) next
                if (like_out < 0 || like_out > 10) next
                
                vote_out <- vote_out / 100
                
                # Sumar diferencia ponderada
                peso <- vote_out / (1 - vote_in)
                suma <- suma + abs(like_in - like_out) * peso # minima dif
                # suma <- suma + like_in - like_out * peso
        }
        
        return(as.numeric(suma))
}


data_ap <- data %>%
        filter(!is.na(IMD3005_3)) %>%
        rowwise() %>%
        mutate(affective_polarization = calc_affective_polarization(cur_data())) %>%
        ungroup()

data_ap <- data_ap %>%
        mutate(
                affective_polarization = ifelse(affective_polarization < 0, 0, affective_polarization)
        )

data_ap <- data_ap %>%
        mutate(
                in_suffix = gsub("IMD5000_", "", IMD3005_3),
                in_vote = case_when(
                        in_suffix == "A" ~ IMD5001_A,
                        in_suffix == "B" ~ IMD5001_B,
                        in_suffix == "C" ~ IMD5001_C,
                        in_suffix == "D" ~ IMD5001_D,
                        in_suffix == "E" ~ IMD5001_E,
                        in_suffix == "F" ~ IMD5001_F,
                        in_suffix == "G" ~ IMD5001_G,
                        in_suffix == "H" ~ IMD5001_H,
                        in_suffix == "I" ~ IMD5001_I,
                        TRUE ~ NA_real_
                ),
                in_vote = in_vote / 100  # pasar a proporción
        )


agg_AP <- data_ap %>%
        filter(!is.na(in_vote) & !is.na(affective_polarization)) %>%
        group_by(IMD1004, IMD1006_NAM, IMD1008_YEAR) %>%
        summarise(
                AP_System = sum(affective_polarization * in_vote) / sum(in_vote),
                .groups = "drop"
        )


print(agg_AP, n=221)


######## PARTY LEVEL POLARIZATION #######



# Select variables
data <- cses

data <- select(data, IMD1004, IMD1006_UNALPHA2, IMD1006_NAM, IMD1008_YEAR, # identifiers
               IMD3008_A, IMD3008_B, IMD3008_C, IMD3008_D, IMD3008_E, IMD3008_F, IMD3008_G, IMD3008_H, IMD3008_I, # like/dislike parties
               IMD3009_A, IMD3009_B, IMD3009_C, IMD3009_D, IMD3009_E, IMD3009_F, IMD3009_G, IMD3009_H, IMD3009_I, # like/dislike leader
               IMD3005_1, IMD3005_2, IMD3005_3, # feel close/a little closer to a party
               IMD5000_A, IMD5000_B, IMD5000_C, IMD5000_D, IMD5000_E, IMD5000_F, IMD5000_G, IMD5000_H, IMD5000_I, # party ident. number
               IMD5001_A, IMD5001_B, IMD5001_C, IMD5001_D, IMD5001_E, IMD5001_F, IMD5001_G, IMD5001_H, IMD5001_I, # vote share (to lower house)
)

data <- data %>%
        mutate(across(starts_with("IMD5001_"), ~ ifelse(. > 99, 0, .)))



vote_summary <- data %>%
        group_by(IMD1004) %>%
        slice(1) %>%  # Take one row per election
        ungroup() %>%
        select(IMD1004, IMD5001_A, IMD5001_B, IMD5001_C, IMD5001_D,
               IMD5001_E, IMD5001_F, IMD5001_G, IMD5001_H, IMD5001_I) %>%
        rowwise() %>%
        mutate(Total_Vote_Share = sum(c_across(starts_with("IMD5001_")), na.rm = TRUE)) %>%
        ungroup() %>% select(IMD1004, Total_Vote_Share)

delete <- vote_summary[vote_summary$Total_Vote_Share >100,]$IMD1004

data <- data[!data$IMD1004%in% delete,]


# STEP 1: filter by respondents of IMD3005_1 and IMD3005_2.
# We code as party supporters both those who feel close 
# and those who feel a little closer to the relevant party.(Addams et al., 2020:14)

data <- filter(data, IMD3005_1 ==1 | IMD3005_2 == 1)

# STEP 2: 

nas <- unique(data$IMD3005_3)
nas <- as.numeric(nas[grepl("^9", as.character(nas))])

# Identifying each party by their code
A. <- unique(data[!data$IMD5000_A %in% nas, ]$IMD5000_A)
B. <- unique(data[!data$IMD5000_B %in% nas, ]$IMD5000_B)
C. <- unique(data[!data$IMD5000_C %in% nas, ]$IMD5000_C)
D. <- unique(data[!data$IMD5000_D %in% nas, ]$IMD5000_D)
E. <- unique(data[!data$IMD5000_E %in% nas, ]$IMD5000_E)
F. <- unique(data[!data$IMD5000_F %in% nas, ]$IMD5000_F)
G. <- unique(data[!data$IMD5000_G %in% nas, ]$IMD5000_G)
H. <- unique(data[!data$IMD5000_H %in% nas, ]$IMD5000_H)
I. <- unique(data[!data$IMD5000_I %in% nas, ]$IMD5000_I)


# Replacing party code by "IMD5000_X"
data$IMD3005_3 <- ifelse(data$IMD3005_3 %in% A., "IMD5000_A", data$IMD3005_3)
data$IMD3005_3 <- ifelse(data$IMD3005_3 %in% B., "IMD5000_B", data$IMD3005_3)
data$IMD3005_3 <- ifelse(data$IMD3005_3 %in% C., "IMD5000_C", data$IMD3005_3)
data$IMD3005_3 <- ifelse(data$IMD3005_3 %in% D., "IMD5000_D", data$IMD3005_3)
data$IMD3005_3 <- ifelse(data$IMD3005_3 %in% E., "IMD5000_E", data$IMD3005_3)
data$IMD3005_3 <- ifelse(data$IMD3005_3 %in% F., "IMD5000_F", data$IMD3005_3)
data$IMD3005_3 <- ifelse(data$IMD3005_3 %in% G., "IMD5000_G", data$IMD3005_3)
data$IMD3005_3 <- ifelse(data$IMD3005_3 %in% H., "IMD5000_H", data$IMD3005_3)
data$IMD3005_3 <- ifelse(data$IMD3005_3 %in% I., "IMD5000_I", data$IMD3005_3)

# Only parties included in questions
p. <- c("IMD5000_A", "IMD5000_B", "IMD5000_C", "IMD5000_D", "IMD5000_E", 
        "IMD5000_F", "IMD5000_G", "IMD5000_H", "IMD5000_I")

data <- data[data$IMD3005_3 %in% p.,]


# STEP 3. Calculation of average in-party rating

# Before calculating, let's change the direction of the variable (Addams et al., 2020:15)
# To Make interpretations intuitive, we present results in which the feeling thermometer is reversed 
# such that a rating of 10 denotes to most intense negative party evaluation and 0 the most positive.

remove10 <- function(x){ifelse(x>10,NA,x)}
data$IMD3008_A <- remove10(data$IMD3008_A)
data$IMD3008_B <- remove10(data$IMD3008_B)
data$IMD3008_C <- remove10(data$IMD3008_C)
data$IMD3008_D <- remove10(data$IMD3008_D)
data$IMD3008_E <- remove10(data$IMD3008_E)
data$IMD3008_F <- remove10(data$IMD3008_F)
data$IMD3008_G <- remove10(data$IMD3008_G)
data$IMD3008_H <- remove10(data$IMD3008_H)
data$IMD3008_I <- remove10(data$IMD3008_I)


# Like scores fuera de rango → NA
data <- data %>%
        mutate(across(starts_with("IMD3008_"), ~ ifelse(. >= 0 & . <= 10, ., NA)))

# Vote share fuera de rango → NA
data <- data %>%
        mutate(across(starts_with("IMD5001_"), ~ ifelse(. >= 0 & . <= 100, ., NA)))

# Vector de sufijos de partidos
parties <- LETTERS[1:9]

# Normalizar vote shares por fila
normalize_votes <- function(row) {
        shares <- as.numeric(row[grepl("IMD5001_", names(row))])
        total <- sum(shares, na.rm = TRUE)
        
        if (is.na(total) || total == 0) return(rep(NA, length(shares)))
        
        return(shares / total * 100)  # mantener en escala 0–100
}

vote_cols <- grep("^IMD5001_", names(data), value = TRUE)

#data[vote_cols] <- t(apply(data[vote_cols], 1, normalize_votes))



# Función que calcula AP para una fila (individuo)
calc_affective_polarization <- function(row) {
        in_party_col <- row[["IMD3005_3"]]  # Ej: "IMD5000_B"
        if (is.na(in_party_col)) return(NA)
        
        # Obtener sufijo del partido propio (ej: "B")
        suffix <- gsub("IMD5000_", "", in_party_col)
        
        # Obtener feeling y voto del partido propio
        like_in <- suppressWarnings(as.numeric(row[[paste0("IMD3008_", suffix)]]))
        vote_in <- suppressWarnings(as.numeric(row[[paste0("IMD5001_", suffix)]]))
        
        # Validaciones
        if (is.na(like_in) || is.na(vote_in)) return(NA)
        if (vote_in <= 0 || vote_in >= 100) return(NA)
        if (like_in < 0 || like_in > 10) return(NA)
        
        # Convertir a proporción
        vote_in <- vote_in / 100
        
        # Inicializar suma
        suma <- 0
        
        for (s in parties) {
                if (s == suffix) next  # saltar in-party
                
                like_out <- suppressWarnings(as.numeric(row[[paste0("IMD3008_", s)]]))
                vote_out <- suppressWarnings(as.numeric(row[[paste0("IMD5001_", s)]]))
                
                # Validaciones
                if (is.na(like_out) || is.na(vote_out)) next
                if (vote_out <= 0 || vote_out >= 100) next
                if (like_out < 0 || like_out > 10) next
                
                vote_out <- vote_out / 100
                
                # Sumar diferencia ponderada
                peso <- vote_out / (1 - vote_in)
                suma <- suma + abs(like_in - like_out) * peso # minima dif
                # suma <- suma + like_in - like_out * peso
        }
        
        return(as.numeric(suma))
}


data_ap <- data %>%
        filter(!is.na(IMD3005_3)) %>%
        rowwise() %>%
        mutate(affective_polarization = calc_affective_polarization(cur_data())) %>%
        ungroup()

data_ap <- data_ap %>%
        mutate(
                affective_polarization = ifelse(affective_polarization < 0, 0, affective_polarization)
        )



data_ap <- data_ap %>%
        mutate(
                in_party_code = as.character(IMD3005_3),
                in_party_code = ifelse(grepl("^9{1}\\d{6}$", in_party_code), NA, in_party_code)
        )

agg_AP_party <- data_ap %>%
        filter(!is.na(in_party_code) & !is.na(affective_polarization)) %>%
        group_by(IMD1004, IMD1008_YEAR, in_party_code) %>%
        summarise(
                AP_Party = mean(affective_polarization, na.rm = TRUE),
                n = n(),
                .groups = "drop"
        )

# Optionally add party name
agg_AP_party <- agg_AP_party %>%
        left_join(
                data_ap %>% select(in_party_code) %>% distinct(),
                by = "in_party_code"
        )

print(agg_AP_party, n=50)




## Recovering party codes

lookup <- cses %>%
        select(IMD1004,
               IMD5000_A, IMD5000_B, IMD5000_C, IMD5000_D, IMD5000_E,
               IMD5000_F, IMD5000_G, IMD5000_H, IMD5000_I) %>%
        distinct(IMD1004, .keep_all = TRUE) %>%
        pivot_longer(
                cols = starts_with("IMD5000_"),
                names_to = "in_party_code",
                values_to = "cses_party_code"
        )


agg_AP_party <- agg_AP_party %>%
        left_join(lookup, by = c("IMD1004", "in_party_code"))


rio::export(agg_AP_party, file= here::here("party_AP_cses.xlsx"), format = "xlsx")













