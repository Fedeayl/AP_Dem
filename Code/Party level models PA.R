
library(dplyr)
library(ggplot2)
library(sandwich)
library(lmtest)
library(modelsummary)


merging <- rio::import(here::here("merged_codes.xlsx"))
merging <- select(merging, dataset_party_id.x, dataset_party_id.y, name_english.y)
names(merging) <- c("vparty_code", "cses_party_code", "party_name")
merging$cses_party_code <- as.numeric(merging$cses_party_code)

partyAP <- rio::import(here::here("party_AP_cses.xlsx"))

data <- left_join(partyAP, merging)
data <- data[!is.na(data$vparty_code),]

names(data)[1:2] <- c("country", "year")


# Select relevant variables from V-Party
vparty <- vdemdata::vparty

vparty_selected <- vparty %>%
        select( year          = year,             # Year
                vparty_code    = v2paid,          # Party unique code
                vote_share    = v2pavote,         # Vote share
                seat_share    = v2paseatshare,    # Seat share
                personalism   = v2paind_ord,          # Personalization of party
                ideology_lr   = v2pariglef,       # Economic left-right
                populism      = v2xpa_popul,      # Populism index
                antipluralism = v2xpa_antiplural, # Anti-pluralism index
                antielitism   = v2paanteli,       # Anti-elitism
                welfare       = v2pawelf,         # Welfare orientation
                clientelism   = v2paclient,       # Clientelism
                gov_support   = v2pagovsup,       # Government support (incumbency)
                lgbt_rights   = v2palgbt,         # LGBT social equality
                gender_eq     = v2pagender,       # Gender equality
                religion      = v2parelig,        # Religious principles
                cohesion      = v2padisa,         # Internal cohesion
                resources     = v2pafunds_nr,         # Party resources
                min_rights    = v2paminor,
                # For PI index
                candidate_nom  = v2panom, 
                p_cont         = v2paelcont,
                local_offices  = v2palocoff,
                org_strength   = v2paactcom )


vparty_selected$vparty_code <- as.character(vparty_selected$vparty_code)

data <- left_join(data, vparty_selected)

# Add V-Dem PSI
vdem <- vdemdata::vdem
vdem_inst <- select(vdem, country_text_id, year, v2xps_party)
vdem_inst$country <- paste0(vdem_inst$country_text_id, "_", vdem_inst$year)
vdem_inst <- select(vdem_inst, country, v2xps_party)
names(vdem_inst) <- c("country", "PSI")

data2 <- left_join(data, vdem_inst)

# Filter cases with a few answers
data2 <- data2[data2$n> 20 & data2$n < 1200,]

# INDEX of P.INST

# If the original range is 1 to 5, with 5=leader control, do:
data2$candidate_nom_rec <- -1 * data2$candidate_nom

data2 <- data2 %>%
        mutate(value_infusion = rowMeans(select(., candidate_nom, p_cont), na.rm = TRUE),
                routinization  = rowMeans(select(., local_offices, org_strength), na.rm = TRUE)) %>%
        mutate(PI_index = rowMeans(select(., value_infusion, routinization), na.rm = TRUE))


# Add partisanship

country_data <- rio::import("/Users/Fede/Desktop/Affective personalism/DATA PAPER/AffectivePol_dataset.xlsx")
names(country_data)[1] <- "country"
names(country_data)[3] <- "year"
data2 <- left_join(data2, country_data)
data2$country_code <- substr(data2$country, 1, 3)



############   Estimate model ################ 

model <- lm(AP_Party ~ personalism*PI_index + ideology_lr + Partisanship + PSI + Ideological_pol +
                    factor(country_code) + factor(year),data = data2)


# Clustered standard errors by country
vcov_country <- vcovCL(model,, type = "HC1", cluster = data2$country_code)
coeftab <- coeftest(model, vcov = vcov_country)

# Remove fixed effects (country/year)
coeftab <- coeftab[!grepl("^factor\\(country_code\\)", rownames(coeftab)) & 
                           !grepl("^factor\\(year\\)", rownames(coeftab)), ]

# Print clean results table
print(coeftab)
summary(model)

########## Standarized coefficient plot #############
 

# INTERACTION PLOT

 library(interactions)
# Convert 'year' to factor properly
data2$year <- as.factor(round(data2$year))  # Round if needed to remove decimals
interact_plot(model, pred = "personalism", modx = "PI_index", data=data2 )



# MIXED EFFECTS MODEL

library(lmerTest)
library(clubSandwich)

model_lmer <- lmer(
        AP_Party ~ personalism*PI_index + ideology_lr + Partisanship + PSI + Ideological_pol +
                (1 | country_code / vparty_code), 
        data = data2, REML = FALSE
)

summary(model_lmer)


# Extract data used in the model (only complete cases used)
model_data <- model.frame(model_lmer)

# Make sure cluster vector matches these rows
cluster <- model_data$country_code

# Then pass this to vcovCR
vcov_cl <- vcovCR(model_lmer, cluster = cluster, type = "CR2")

# Coefficient test with robust SE
coef_test(model_lmer, vcov = vcov_cl)

length(cluster)      # Should equal number of rows in model_data
nrow(model_data)     # Should be the same



## LAGGED DV

library(dplyr)

data2 <- data2 %>%
        arrange(vparty_code, year) %>%
        group_by(vparty_code) %>%
        mutate(AP_Party_lag = lag(AP_Party, order_by = year)) %>%
        ungroup()

data_lagged <- data2 %>% filter(!is.na(AP_Party_lag))



model_lag <- lm(AP_Party ~ AP_Party_lag + personalism*PI_index + ideology_lr + Partisanship + PSI + Ideological_pol +
                        factor(country_code) + factor(year), data = data2)


# Clustered standard errors by country
vcov_country <- vcovCL(model_lag,, type = "HC1", cluster = data2$country_code)
coeftab <- coeftest(model_lag, vcov = vcov_country)

# Remove fixed effects (country/year)
coeftab <- coeftab[!grepl("^factor\\(country_code\\)", rownames(coeftab)) & 
                           !grepl("^factor\\(year\\)", rownames(coeftab)), ]

# Print clean results table
print(coeftab)



## VIF

# Ajusta el modelo (ya lo tienes, pero por si acaso)
model <- lm(AP_Party ~ personalism*PI_index + ideology_lr + Partisanship + PSI + Ideological_pol,
            data = data2)

# Calcula el VIF
vif_values <- car::vif(model)
print(vif_values)

cor.test(data2$PSI, data2$PI_index)
