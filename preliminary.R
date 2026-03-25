# Installing Packages

# Libraries
library(tidyverse)
library(readxl)
library(glue)
library(purrr)
library(roll)
library(zoo)
library(stargazer)
library(varhandle)
library(plm)
library(lmtest)
library(sandwich)
library(broom)
library(rstudioapi)


######################################
# Directory Work
######################################
current_path <- getSourceEditorContext()$path
current_directory <- strsplit(current_path, '/')[[1]][1:length(strsplit(current_path, '/')[[1]]) - 1]
current_directory <- paste(as.character(current_directory), collapse = '/')
current_directory

get_file_path <- function(path) {
  return(paste(current_directory, path, sep="/"))
}

######################################
# World Integrated Trade Solutions Data
# Weighted Average Revealed Competitive Advantage Index
######################################
# Getting all WITS Files
wits_files <- list.files(path = get_file_path("Data/WITS/EXP"), pattern="*.xlsx", full.names=TRUE, recursive=FALSE)
wits_rca_files <- list.files(path = get_file_path("Data/WITS/RCA"), pattern="*.xlsx", full.names=TRUE, recursive=FALSE)
all_wits_tibbles = wits_files %>% map( ~read_excel(.x, sheet = "Product-TimeSeries-Product"))
all_rca_tibbles = wits_rca_files %>% map( ~read_excel(.x, sheet = "Product-TimeSeries-Product"))

# Finding Countries that don't exist in all datasets
original_countries <- data.frame(all_wits_tibbles[1])$Reporter.Name

find_inconsistent_nations <- function(list_of_tibbles) {
  all_to_remove = ''
  
  for (i in 2:length(list_of_tibbles)) {
    print(wits_files[i])
    
    b <- 1; j <- 1;  # b -> original; l -> current_check
    
    while(j <= length(data.frame(list_of_tibbles[i])$Reporter.Name) && b <= length(data.frame(list_of_tibbles[1])$Reporter.Name)) {
      
      if (data.frame(list_of_tibbles[1])$Reporter.Name[b] != data.frame(list_of_tibbles[i])$Reporter.Name[j]) {
        
        if (data.frame(list_of_tibbles[1])$Reporter.Name[b + 1] == data.frame(list_of_tibbles[i])$Reporter.Name[j]) {
          j <- j - 1;
          all_to_remove = paste(all_to_remove, data.frame(list_of_tibbles[1])$Reporter.Name[b], sep = ':')
          print(glue('- {data.frame(list_of_tibbles[1])$Reporter.Name[b]}'));
        } else if (data.frame(list_of_tibbles[1])$Reporter.Name[b] == data.frame(list_of_tibbles[i])$Reporter.Name[j + 1]) {
          b <- b - 1 
          all_to_remove = paste(all_to_remove, data.frame(list_of_tibbles[i])$Reporter.Name[j], sep = ':')
          print(glue('+ {data.frame(list_of_tibbles[i])$Reporter.Name[j]}'));
        } else {
          print('Double Issue. Investigate Further'); break;
        }
      }; j <- j + 1; b <- b + 1
      
    }
  }
  countries_to_remove <- as.list(unlist(strsplit(all_to_remove, ':')))
  return(countries_to_remove)
}



# Filtering out Countries to Remove
countries_to_remove <- find_inconsistent_nations(all_wits_tibbles)
countries_to_remove <- unique(unlist(countries_to_remove, use.names = FALSE))
countries_to_remove <- countries_to_remove[2:length(countries_to_remove)]
print(countries_to_remove)

rca_countries_to_remove <- find_inconsistent_nations(all_rca_tibbles)
rca_countries_to_remove <- unique(unlist(rca_countries_to_remove, use.names = FALSE))
rca_countries_to_remove <- rca_countries_to_remove[2:length(rca_countries_to_remove)]
print(rca_countries_to_remove)

all_countries_to_remove = unique(c(countries_to_remove, rca_countries_to_remove))
print(all_countries_to_remove)

all_wits_tibbles_binded <- bind_rows(all_wits_tibbles)
all_rca_tibbles_binded <- bind_rows(all_rca_tibbles)

colnames(all_wits_tibbles_binded)[1] <- 'reporter_name'
colnames(all_wits_tibbles_binded)[2] <- 'partner_name'
colnames(all_wits_tibbles_binded)[3] <- 'trade_flow'
colnames(all_wits_tibbles_binded)[4] <- 'product_group'
colnames(all_wits_tibbles_binded)[5] <- 'indicator'
colnames(all_rca_tibbles_binded)[1] <- 'reporter_name'
colnames(all_rca_tibbles_binded)[2] <- 'partner_name'
colnames(all_rca_tibbles_binded)[3] <- 'trade_flow'
colnames(all_rca_tibbles_binded)[4] <- 'product_group'
colnames(all_rca_tibbles_binded)[5] <- 'indicator'

all_wits_tibbles_binded_wo_removing_countries <- all_wits_tibbles_binded %>% 
  filter(! (reporter_name %in% all_countries_to_remove))
View(all_wits_tibbles_binded_wo_removing_countries)

all_rca_tibbles_binded_wo_removing_countries <- all_rca_tibbles_binded %>% 
  filter(! (reporter_name %in% all_countries_to_remove))
View(all_rca_tibbles_binded_wo_removing_countries)

# Scaling Data to sum to 100
countries <- as.list(unique(unlist(as.list(all_wits_tibbles_binded_wo_removing_countries$reporter_name))))
scaled_wits_tibbles <- NULL
for (country in countries) {
  a_df <- tibble(reporter_name = '', partner_name = '', trade_flow = '', product_group = '', indicator = '')[0,]
  for (i in 1:length(all_wits_tibbles)) {
    a_df <- a_df %>% 
      add_row(reporter_name = "", partner_name = "", trade_flow = "", product_group = "", indicator = "")
  }
    
  b_df <- all_wits_tibbles_binded_wo_removing_countries %>% 
    filter(reporter_name == country) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(where(is.character), ~"Total"))) %>% 
    select(!c(reporter_name, partner_name, trade_flow, product_group, indicator))
  b_df <- mapply('/', b_df, b_df[length(all_wits_tibbles) + 1, ])
  
  a_df$reporter_name <- (all_wits_tibbles_binded_wo_removing_countries %>% filter(reporter_name == country))$reporter_name
  a_df$partner_name <- (all_wits_tibbles_binded_wo_removing_countries %>% filter(reporter_name == country))$partner_name
  a_df$trade_flow <- (all_wits_tibbles_binded_wo_removing_countries %>% filter(reporter_name == country))$trade_flow
  a_df$product_group <- (all_wits_tibbles_binded_wo_removing_countries %>% filter(reporter_name == country))$product_group
  a_df$indicator <- (all_wits_tibbles_binded_wo_removing_countries %>% filter(reporter_name == country))$indicator
  
  b_df <- head(b_df, -1)
  c_df <- cbind(a_df, b_df)
  
  if (i == 1) {
    scaled_wits_tibbles <- c_df
  } else {
    scaled_wits_tibbles = rbind(scaled_wits_tibbles, c_df)
  }
}

setdiff(scaled_wits_tibbles$reporter_name, scaled_rca_tibbles$reporter_name)
scaled_wits_tibbles <- scaled_wits_tibbles %>% 
  filter(reporter_name != 'European Union')
View(scaled_wits_tibbles)

scaled_rca_tibbles <- all_rca_tibbles_binded_wo_removing_countries %>% 
  arrange(reporter_name)
View(scaled_rca_tibbles)

# Multiplying the Matrices to get the weighted average
all_numeric_wits <- scaled_wits_tibbles %>% 
  select(!c(reporter_name, partner_name, trade_flow, product_group, indicator))
all_numeric_rca <- scaled_rca_tibbles %>% 
  select(!c(reporter_name, partner_name, trade_flow, product_group, indicator))

View(all_numeric_wits)
View(all_numeric_rca)

multiplied_rca_wits <- all_numeric_wits * all_numeric_rca
View(multiplied_rca_wits)

all_countries <- unique(scaled_rca_tibbles$reporter_name)
length(all_countries)
list_of_countries <- c(22 * length(all_countries))
j <- 0
for (country in all_countries) {
  for (i in 1:22) {
    list_of_countries[j * 22 + i] <- country
  }; j <- j + 1
}
print(list_of_countries)

final_rca_dataframe_1 <- cbind(list_of_countries, multiplied_rca_wits)
colnames(final_rca_dataframe_1)[1] <- 'reporter_name'
View(final_rca_dataframe_1)

# Summming across product categories for final averages
final_rca_dataframe_2 <- final_rca_dataframe_1 %>% 
  group_by(reporter_name) %>% 
  summarise_each(list(sum))
View(final_rca_dataframe_2)



######################################
# Working with Country Names
######################################
# Countries of se_size Dataset
se_size_base <- read_csv(get_file_path('Extracting PDF Data/size_of_the_shadow_economy.csv'))
colnames(se_size_base)[1] <- 'country'
se_dataset_countries <- unique(se_size_base$country)[-length(unique(se_size_base$country))]
se_dataset_countries <- data.frame(matrix(unlist(se_dataset_countries), nrow=length(se_dataset_countries), byrow=TRUE))
colnames(se_dataset_countries)[1] <- 'country'
View(se_dataset_countries)

# Countries of WDI Database
population_base <- read_csv(get_file_path('Data/WDI - Population/API_SP.POP.TOTL_DS2_en_csv_v2_61.csv'))
colnames(population_base)[1] <- 'country'; colnames(population_base)[2] <- 'code'
wdi_countries_and_codes <- population_base %>% select('country', 'code')
View(wdi_countries_and_codes)

# Creating Correction Dataframe Pt. 1
merge_1 <- merge(wdi_countries_and_codes, se_dataset_countries, by = c('country'), all = TRUE)
View(merge_1)
View(merge_1 %>% filter(is.na(code)))

# Countries of wa_rca Dataset
named_country_wca_list_raw <- scan(get_file_path('Extracting PDF Data/country_name_changes_wca_dataset.txt'))
named_country_wca_list <- strsplit(named_country_wca_list_raw, "[[:space:]]->[[:space:]]")
names(named_country_wca_list) <- sapply(named_country_wca_list, `[[`, 1)
named_country_wca_list <- lapply(named_country_wca_list, `[`, -1)
named_country_wca_list

# Fixing the names of the wa_rca dataset
final_rca_dataframe_3 <- final_rca_dataframe_2
final_rca_dataframe_3$reporter_name <- lapply(final_rca_dataframe_3$reporter_name, function(name) { ifelse(name %in% names(named_country_wca_list), named_country_wca_list[name], name) } )
final_rca_dataframe_3 <- final_rca_dataframe_3 %>% 
  rename('country' = 'reporter_name') %>% 
  arrange(country)
final_rca_dataframe_3 <- as.data.frame(lapply(final_rca_dataframe_3, unlist))

# Getting Updated wa_rca Countries
wa_rca_countries <- unique(final_rca_dataframe_3$country)
wa_rca_countries <- data.frame(matrix(unlist(wa_rca_countries), nrow=length(wa_rca_countries), byrow=TRUE))
colnames(wa_rca_countries)[1] <- 'country'
wa_rca_countries

# Creating Correction Dataframe Pt. 2
merge_2 <- merge(merge_1, wa_rca_countries, by = c('country'), all = TRUE)
View(merge_2)
View(merge_2 %>% filter(is.na(code)))

# Perfecting the wa_rca Dataset
final_rca_dataframe_4 <- merge(final_rca_dataframe_3, merge_2, by = c('country'), all.x = TRUE)
final_rca_dataframe_4 <- final_rca_dataframe_4 %>% select(country, code, 'X1988':'X2022')
colnames(final_rca_dataframe_4)[-1:-2] <- as.numeric(1988:2022)
final_rca_dataframe_4 <- final_rca_dataframe_4 %>% 
  filter(!is.na(code))
View(final_rca_dataframe_4)

# IMF Countries Dataset
named_country_imf_list_raw <- scan(get_file_path('Extracting PDF Data/country_name_changes_imf_dataset.txt'), what="", sep="\n")
named_country_imf_list <- strsplit(named_country_imf_list_raw, "[[:space:]]->[[:space:]]")
names(named_country_imf_list) <- sapply(named_country_imf_list, `[[`, 1)
named_country_imf_list <- lapply(named_country_imf_list, `[`, -1)
named_country_imf_list

all_government_revenue_base <- read_csv(get_file_path('Data/IMF - World Revenue/dataset_2026-03-23T17_40_40.193148454Z_DEFAULT_INTEGRATION_IMF.FAD_WORLD_3.0.1.csv'))
all_government_revenue_base <- all_government_revenue_base %>% 
  select( c(c('COUNTRY', 'INDICATOR'), as.character(as.list(1991:2015))) )
colnames(all_government_revenue_base)[1] <- 'country'; colnames(all_government_revenue_base)[2] <- 'indicator'
all_government_revenue_base$country  <- lapply(all_government_revenue_base$country, function(name) { ifelse(name %in% names(named_country_imf_list), named_country_imf_list[name], name) } )
all_government_revenue_base <- all_government_revenue_base %>% arrange(country)
all_government_revenue_base <- as.data.frame(lapply(all_government_revenue_base, unlist))
all_government_revenue_base <- all_government_revenue_base %>% select(country, indicator, 'X1991':'X2015')
colnames(all_government_revenue_base)[-1:-2] <- as.numeric(1991:2015)
View(all_government_revenue_base)

imf_countries <- unique(all_government_revenue_base$country)
imf_countries <- data.frame(matrix(unlist(imf_countries), nrow=length(imf_countries), byrow=TRUE))
colnames(imf_countries)[1] <- 'country'

merge_3 <- merge(merge_2, imf_countries, by = c('country'), all = TRUE)
View(merge_3)

all_government_revenue_base <- merge(all_government_revenue_base, merge_3, by = c('country'), all.x = TRUE)
all_government_revenue_base <- all_government_revenue_base %>% 
  filter(!is.na(code)) %>% 
  select(country, code, indicator, '1991':'2015')
View(all_government_revenue_base)


# Fixing the se_size Dataset
se_size_base <- merge(se_size_base, merge_2, by = c('country'), all.x = TRUE)
se_size_base <- se_size_base %>% 
  select(country, code, '1991':'2015') %>% 
  filter(country != 'Av. over countries')
View(se_size_base)

countries_to_keep <- unique(se_size_base$country)
countries_to_keep

######################################
# Elongating Variables
######################################
# Size of the Shadow Economy
se_size = pivot_longer(se_size_base, cols = colnames(se_size_base)[3:27], names_to = "year", values_to = 'se_size')
se_size <- se_size %>% 
  filter(year %in% as.list(1991:2015))
View(se_size)


# Adjustment Columns for per Capita Measures, Percentage of Income Measures , and Currency Conversions 
# Population (for per capita measures)
population_base <- population_base[1:(length(population_base) - 1)]
population <- pivot_longer(population_base, cols = colnames(population_base)[5:70], names_to = "year", values_to = 'population')
colnames(population) <- c('country', 'code', 'indicator_name', 'indicator_code', 'year', 'population')
population <- population %>% 
  select('country', 'code', 'year', 'population') %>% 
  filter(year %in% as.list(1991:2015)) %>% filter(country %in% countries_to_keep)

View(population)

# Net Adjusted Income per Capita
adjusted_income_base <- read_csv(get_file_path('Data/WDI - Adjusted Net National Income per Capita/API_NY.ADJ.NNTY.PC.KD_DS2_en_csv_v2_7018.csv'))
adjusted_income_base <- adjusted_income_base[1:(length(adjusted_income_base) - 1)]
adjusted_income <- pivot_longer(adjusted_income_base, cols = colnames(adjusted_income_base)[5:70], names_to = "year", values_to = 'adjusted_income')
colnames(adjusted_income) <- c('country', 'code', 'indicator_name', 'indicator_code', 'year', 'adjusted_income')
adjusted_income <- adjusted_income %>% 
  select('country', 'code', 'year', 'adjusted_income') %>% 
  filter(year %in% as.list(1991:2015)) %>% filter(country %in% countries_to_keep)
View(adjusted_income)

# Deflating All Money to 2015 Values
deflator_base <- read_csv(get_file_path('Data/WDI - GDP Deflator/API_NY.GDP.DEFL.KD.ZG_DS2_en_csv_v2_104.csv'))
deflator_base <- deflator_base[1:(length(deflator_base) - 1)]
deflator_us <- deflator_base[deflator_base[['Country Code']] == 'USA',]
deflator_us <- pivot_longer(deflator_us, cols = colnames(deflator_us)[5:70], names_to = "year", values_to = 'deflator')
deflator_us <- deflator_us %>% 
  filter(year %in% as.list(1991:2015)) %>% 
  arrange(desc(year)) %>% 
  mutate(division_factor = 1 + deflator / 100)
deflator_product <- list(nrow(deflator_us)); deflator_product[1] <- 1
for (i in 2:nrow(deflator_us)) {
  deflator_product[i] =  prod(deflator_us$division_factor[2:i])
}
deflator_us$prod_division_factor <- deflator_product
colnames(deflator_us) <- c('country', 'code', 'indicator_name', 'indicator_code', 'year', 'deflator', 'division_factor', 'prod_division_factor')
deflator_us <- deflator_us %>%
  arrange(year) %>% 
  select(-c('indicator_name', 'indicator_code'))
View(deflator_us)

# Converting all LCUs to USD 2015
exchange_rate_base <- read_csv(get_file_path('Data/WDI - Official Exchange Rate/API_PA.NUS.FCRF_DS2_en_csv_v2_58.csv'))
exchange_rate_base <- exchange_rate_base[1:(length(exchange_rate_base) - 1)]
exchange_rate <- pivot_longer(exchange_rate_base, cols = colnames(exchange_rate_base)[5:70], names_to = "year", values_to = 'lcu_to_usd')
colnames(exchange_rate) <- c('country', 'code', 'indicator_name', 'indicator_code', 'year', 'lcu_to_usd')
exchange_rate <- exchange_rate %>% 
  select(-c('indicator_name', 'indicator_code')) %>% 
  filter(year %in% as.list(1991:2015)) %>% filter(country %in% countries_to_keep) %>% 
  mutate(usd2015_deflation_factor = lcu_to_usd)

for (i in 1:length(rownames(exchange_rate))) {
  def_index = if (i %% length(deflator_us$prod_division_factor) == 0) length(deflator_us$prod_division_factor) else i %% length(deflator_us$prod_division_factor)
  exchange_rate[i, "usd2015_deflation_factor"] = deflator_us$prod_division_factor[def_index]
}

exchange_rate <- exchange_rate %>% 
  mutate(lcu_to_usd2015 = lcu_to_usd / usd2015_deflation_factor)
View(exchange_rate)



# international taxes
taxes_on_intl_base <- read_csv(get_file_path('Data/WDI - Taxes on International Trade/API_GC.TAX.INTT.CN_DS2_en_csv_v2_12651.csv'))
taxes_on_intl_base <- taxes_on_intl_base[1:(length(taxes_on_intl_base) - 1)]
taxes_on_intl = pivot_longer(taxes_on_intl_base, cols = colnames(taxes_on_intl_base)[5:70], names_to = "year", values_to = 'intl_taxes_collected')
colnames(taxes_on_intl) <- c('country', 'code', 'indicator_name', 'indicator_code', 'year', 'intl_taxes_collected')
taxes_on_intl <- taxes_on_intl %>% 
  select(-c('indicator_name', 'indicator_code')) %>% 
  filter(year %in% as.list(1991:2015)) %>% filter(country %in% countries_to_keep) %>% 
  mutate(log_taxes_collected = log(intl_taxes_collected))
View(taxes_on_intl)

taxes_on_intl_with_conversions <- merge(taxes_on_intl, exchange_rate %>% select(-c('country')),
                                        by = c('code', 'year'), all.x = TRUE)
View(taxes_on_intl_with_conversions)
taxes_on_intl_with_conversions <- taxes_on_intl_with_conversions %>% 
  mutate(taxes_collected_usd2015 = intl_taxes_collected / lcu_to_usd2015)
View(taxes_on_intl_with_conversions)

taxes_on_intl_per_capita <- merge(taxes_on_intl_with_conversions, population %>% select(-c('country')), 
                                  by = c('code', 'year'), all.x = TRUE)
View(taxes_on_intl_per_capita)
taxes_on_intl_per_capita <- taxes_on_intl_per_capita %>% 
  mutate(taxes_collected_usd2015_per_cap = taxes_collected_usd2015 / population)
View(taxes_on_intl_per_capita)

taxes_on_intl_percentage <- merge(taxes_on_intl_per_capita, adjusted_income %>% select(-c('country')),
                                  by = c('code', 'year'), all.x = TRUE)
taxes_on_intl_percentage <- taxes_on_intl_percentage %>% 
  mutate(taxes_collected_as_percentage_of_income = taxes_collected_usd2015_per_cap / adjusted_income * 100)
View(taxes_on_intl_percentage)

# Social Contributions
social_contributions_base <- read_csv(get_file_path('Data/WDI - Social Contributions/API_GC.REV.SOCL.CN_DS2_en_csv_v2_12643.csv'))
social_contributions_base <- social_contributions_base[1:(length(social_contributions_base) - 1)]
social_contributions = pivot_longer(social_contributions_base, cols = colnames(social_contributions_base)[5:70], names_to = "year", values_to = 'social_contributions')
colnames(social_contributions) <- c('country', 'code', 'indicator_name', 'indicator_code', 'year', 'social_contributions')
social_contributions <- social_contributions %>% 
  filter(year %in% as.list(1991:2015)) %>% filter(country %in% countries_to_keep) 
View(social_contributions)

social_contributions_with_conversions <- merge(social_contributions, exchange_rate %>% select(-c('country')), 
                                               by = c('code', 'year'), all.x = TRUE)
social_contributions_with_conversions <- social_contributions_with_conversions %>% 
  mutate(social_contributions_usd2015 = social_contributions / lcu_to_usd2015)
View(social_contributions_with_conversions)

social_contributions_per_capita <- merge(social_contributions_with_conversions, population %>% select(-c('country')), 
                                         by = c('code', 'year'), all.x = TRUE)
social_contributions_per_capita <- social_contributions_per_capita %>% 
  mutate(social_contributions_usd2015_per_cap = social_contributions_usd2015 / population)
View(social_contributions_per_capita)

social_contributions_percentage <- merge(social_contributions_per_capita, adjusted_income %>% select(-c('country')),
                                  by = c('code', 'year'), all.x = TRUE)
social_contributions_percentage <- social_contributions_percentage %>% 
  mutate(social_contributions_as_percentage_of_income = social_contributions_usd2015_per_cap / adjusted_income * 100)
View(social_contributions_percentage)


# effectively applied weighted average tariffs
tariff_rate_base <- read_csv(get_file_path('Data/WDI - Tariff Rate, Applied, Weighted Mean, All Products/API_TM.TAX.MRCH.WM.AR.ZS_DS2_en_csv_v2_440.csv'))
tariff_rate_base <- tariff_rate_base[1:(length(tariff_rate_base) - 1)]
tariff_rate = pivot_longer(tariff_rate_base, cols = colnames(tariff_rate_base)[5:70], names_to = "year", values_to = 'applied_tariff_rate')
colnames(tariff_rate) <- c('country', 'code', 'indicator_name', 'indicator_code', 'year', 'applied_tariff_rate')
tariff_rate <- tariff_rate %>% 
  filter(year %in% as.list(1991:2015)) %>% filter(country %in% countries_to_keep)
View(tariff_rate)


# exports
exports_base <- read_csv(get_file_path('Data/WDI - Exports of Goods and Services (constant 2015 US)/API_NE.EXP.GNFS.KD_DS2_en_csv_v2_7684.csv'))
exports_base <- exports_base[1:(length(exports_base) - 1)]
exports = pivot_longer(exports_base, cols = colnames(exports_base)[5:70], names_to = "year", values_to = 'exports')
colnames(exports) <- c('country', 'code', 'indicator_name', 'indicator_code', 'year', 'exports')
exports <- exports %>% 
  filter(year %in% as.list(1991:2015)) %>% filter(country %in% countries_to_keep)

exports_with_per_cap <- merge(exports, population %>% select(-c('country')), 
                              by = c('code', 'year'), all.x = TRUE)
exports_with_per_cap <- exports_with_per_cap %>% 
  mutate(exports_per_cap = exports / population)
View(exports_with_per_cap)

# imports
imports_base <- read_csv(get_file_path('Data/WDI - Imports of Goods and Services (constant 2015 US)/API_NE.IMP.GNFS.KD_DS2_en_csv_v2_14087.csv'))
imports_base <- imports_base[1:(length(imports_base) - 1)]
imports = pivot_longer(imports_base, cols = colnames(imports_base)[5:70], names_to = "year", values_to = 'imports')
colnames(imports) <- c('country', 'code', 'indicator_name', 'indicator_code', 'year', 'imports')
imports <- imports %>% 
  filter(year %in% as.list(1991:2015)) %>% filter(country %in% countries_to_keep)

imports_with_per_cap <- merge(imports, population %>% select(-c('country')), 
                              by = c('code', 'year'), all.x = TRUE)
imports_with_per_cap <- imports_with_per_cap %>% 
  mutate(imports_per_cap = imports / population)
View(imports_with_per_cap)


# Regulatory Strength, Corruption, and Institutions (range from -2.5 to 2.5)
# Control of Corruption
wgi_coc_base <- read_csv(get_file_path('Data/WDI - WGI Control of Corruption/API_CC.EST_DS2_en_csv_v2_4455.csv'))
wgi_coc_base <- wgi_coc_base[1:(length(wgi_coc_base) - 1)]
wgi_coc = pivot_longer(wgi_coc_base, cols = colnames(wgi_coc_base)[5:70], names_to = "year", values_to = 'control_corruption')
wgi_coc <- wgi_coc %>% mutate(control_corruption = control_corruption * 10) %>% select(!c('Indicator Name', 'Indicator Code'))
colnames(wgi_coc) <- c('country', 'code', 'year', 'control_corruption')
# Government Effectiveness
wgi_goveff_base <- read_csv(get_file_path('Data/WDI - WGI Government Effectiveness/API_GE.EST_DS2_en_csv_v2_4300.csv'))
wgi_goveff_base <- wgi_goveff_base[1:(length(wgi_goveff_base) - 1)]
wgi_goveff = pivot_longer(wgi_goveff_base, cols = colnames(wgi_goveff_base)[5:70], names_to = "year", values_to = 'govern_effective')
wgi_goveff <- wgi_goveff %>% mutate(govern_effective = govern_effective * 10) %>% select(!c('Indicator Name', 'Indicator Code'))
colnames(wgi_goveff) <- c('country', 'code', 'year', 'govern_effective')
# Regulatory Quality
wgi_regqua_base <- read_csv(get_file_path('Data/WDI - WGI Regulatory Quality/API_RQ.EST_DS2_en_csv_v2_8225.csv'))
wgi_regqua_base <- wgi_regqua_base[1:(length(wgi_regqua_base) - 1)]
wgi_regqua = pivot_longer(wgi_regqua_base, cols = colnames(wgi_regqua_base)[5:70], names_to = "year", values_to = 'regulatory_quality')
wgi_regqua <- wgi_regqua %>% mutate(regulatory_quality = regulatory_quality * 10) %>% select(!c('Indicator Name', 'Indicator Code'))
colnames(wgi_regqua) <- c('country', 'code', 'year', 'regulatory_quality')
# Rule of Law
wgi_rulelaw_base <- read_csv(get_file_path('Data/WDI - WGI Rule of Law/API_RL.EST_DS2_en_csv_v2_5814.csv'))
wgi_rulelaw_base <- wgi_rulelaw_base[1:(length(wgi_rulelaw_base) - 1)]
wgi_rulelaw = pivot_longer(wgi_rulelaw_base, cols = colnames(wgi_rulelaw_base)[5:70], names_to = "year", values_to = 'rule_of_law')
wgi_rulelaw <- wgi_rulelaw %>% mutate(rule_of_law = rule_of_law * 10) %>% select(!c('Indicator Name', 'Indicator Code'))
colnames(wgi_rulelaw) <- c('country', 'code', 'year', 'rule_of_law')
# Composite
composite_wgi <- merge(wgi_coc, wgi_goveff %>% select(-c('country')), by = c('code', 'year'), all.x = TRUE)
composite_wgi <- merge(composite_wgi, wgi_regqua %>% select(-c('country')), by = c('code', 'year'), all.x = TRUE)
composite_wgi <- merge(composite_wgi, wgi_rulelaw %>% select(-c('country')), by = c('code', 'year'), all.x = TRUE)
composite_wgi <- composite_wgi %>% rowwise() %>% mutate(composite_wgi = mean(c(control_corruption, govern_effective, regulatory_quality, rule_of_law)))
composite_wgi <- composite_wgi %>% 
  filter(year %in% as.list(1991:2015)) %>% filter(country %in% countries_to_keep)
View(composite_wgi)


# Weighted Average Revealed Competitive Advantage
wa_rca = pivot_longer(final_rca_dataframe_4, cols = as.character(1988:2022), names_to = "year", values_to = "wa_rca")
colnames(wa_rca)[1] <- 'country'
wa_rca <- wa_rca %>% 
  filter(year %in% as.list(1991:2015)) %>% filter(country %in% countries_to_keep)
View(wa_rca)

# Index of Economic Freedom
econ_freedom <- read_csv(get_file_path('Data/Index of Economic Freedom/heritage-index-of-economic-freedom-2026-03-12_2236.csv'))
colnames(econ_freedom)[2] <- 'year'
for (i in 1:length(colnames(econ_freedom))) {
  colnames(econ_freedom)[i] <- tolower(gsub(' ', '_', colnames(econ_freedom)[i]))
}
econ_freedom <- econ_freedom %>% 
  filter(year %in% as.list(1991:2015)) %>% filter(country %in% countries_to_keep) %>% 
  arrange(country, year)
econ_freedom <- merge(econ_freedom, merge_3, by = c('country'), all.x = TRUE)
View(econ_freedom %>% filter(is.na(code)))
View(econ_freedom)

# GDP per Capita PPP (Current International $)
gdp_per_cap_base <- read_csv(get_file_path('Data/WDI - GDP per Capita PPP/API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_35.csv'))
gdp_per_cap_base <- gdp_per_cap_base[1:(length(gdp_per_cap_base) - 1)]
gdp_per_cap = pivot_longer(gdp_per_cap_base, cols = colnames(gdp_per_cap_base)[5:70], names_to = "year", values_to = 'gdp_per_cap')
colnames(gdp_per_cap) <- c('country', 'code', 'indicator_name', 'indicator_code', 'year', 'gdp_per_cap')
gdp_per_cap <- gdp_per_cap %>% 
  select(-c('indicator_name', 'indicator_code')) %>% 
  filter(year %in% as.list(1991:2015)) %>% filter(country %in% countries_to_keep)
View(gdp_per_cap)

# Taxes on Goods and Services
vat_base <- read_csv(get_file_path('Data/WDI - Taxes on Goods and Services (Percentage VAT)/API_GC.TAX.GSRV.VA.ZS_DS2_en_csv_v2_12285.csv'))
vat_base <- vat_base[1:(length(vat_base) - 1)]
vat <- pivot_longer(vat_base, cols = colnames(vat_base)[5:70], names_to = "year", values_to = 'vat')
colnames(vat) <- c('country', 'code', 'indicator_name', 'indicator_code', 'year', 'vat')
vat <- vat %>% 
  select(-c('indicator_name', 'indicator_code')) %>% 
  filter(year %in% as.list(1991:2015)) %>% filter(country %in% countries_to_keep)
View(vat)

vat_lcu_base <- read_csv(get_file_path('Data/WDI - Taxes on Goods and Services (Current LCU)/API_GC.TAX.GSRV.CN_DS2_en_csv_v2_7514.csv'))
vat_lcu_base <- vat_lcu_base[1:(length(vat_lcu_base) - 1)]
vat_lcu <- pivot_longer(vat_lcu_base, cols = colnames(vat_lcu_base)[5:70], names_to = "year", values_to = 'vat_lcu')
colnames(vat_lcu) <- c('country', 'code', 'indicator_name', 'indicator_code', 'year', 'vat_lcu')
vat_lcu <- vat_lcu %>% 
  select(-c('indicator_name', 'indicator_code')) %>% 
  filter(year %in% as.list(1991:2015)) %>% filter(country %in% countries_to_keep)
View(vat_lcu)

income_tax_base <- read_csv(get_file_path('Data/WDI - Taxes on Income, Profits, and Capital Gains/API_GC.TAX.YPKG.CN_DS2_en_csv_v2_12653.csv'))
income_tax_base <- income_tax_base[1:(length(income_tax_base) - 1)]
income_tax <- pivot_longer(income_tax_base, cols = colnames(income_tax_base)[5:70], names_to = "year", values_to = 'income_tax')
colnames(income_tax) <- c('country', 'code', 'indicator_name', 'indicator_code', 'year', 'income_tax')
income_tax <- income_tax %>% 
  select(-c('indicator_name', 'indicator_code')) %>% 
  filter(year %in% as.list(1991:2015)) %>% filter(country %in% countries_to_keep)
View(income_tax)

# Tax Variables
all_income_taxes <- all_government_revenue_base %>% 
  filter(indicator == 'Taxes on income, profits, and capital gains, Percent of GDP') %>% filter(country %in% countries_to_keep)
all_income_taxes <- pivot_longer(all_income_taxes, cols = colnames(all_income_taxes)[4:28], names_to = "year", values_to = 'income_tax')
View(all_income_taxes)

all_trade_taxes <- all_government_revenue_base %>% 
  filter(indicator == 'Taxes on international trade and transactions, Percent of GDP') %>% filter(country %in% countries_to_keep)
all_trade_taxes <- pivot_longer(all_trade_taxes, cols = colnames(all_trade_taxes)[4:28], names_to = "year", values_to = 'trade_tax')
View(all_trade_taxes)

all_vat_taxes <- all_government_revenue_base %>% 
  filter(indicator == 'Taxes on goods and services, Percent of GDP') %>% filter(country %in% countries_to_keep)
all_vat_taxes <- pivot_longer(all_vat_taxes, cols = colnames(all_vat_taxes)[4:28], names_to = "year", values_to = 'vat_tax')
View(all_vat_taxes)

all_social_contributions <- all_government_revenue_base %>% 
  filter(indicator == 'Social security contributions, Percent of GDP') %>% filter(country %in% countries_to_keep)
all_social_contributions <- pivot_longer(all_social_contributions, cols = colnames(all_social_contributions)[4:28], names_to = "year", values_to = 'social_contributions')
View(all_social_contributions)


######################################
# Checking Datasets
######################################
# All Variables: taxes_on_intl_percentage, social_contributions_percentage, tariff_rate, composite_wgi, wa_rca, 
#   exports_with_per_cap, imports_with_per_cap, econ_freedom, gdp_per_cap, vat

year_start_range <- 2000
year_end_range <- 2015

# Independent Variables

# 1995: 0 -> 17, <6 -> 76
# 2000: 0 -> 44, <3 -> 101, <6 -> 128
tariffs_left <- tariff_rate %>% 
       filter(year %in% as.list(year_start_range:year_end_range)) %>% 
       group_by(code) %>% 
       summarize(NAs = sum(is.na(applied_tariff_rate))) %>% 
       filter(NAs < 6)


# 1995: 0 -> 106, <6 -> 111
# 2000: 0 -> 111, <6 -> 118
exports_per_cap_left <- exports_with_per_cap %>% 
       filter(year %in% as.list(year_start_range:year_end_range)) %>% 
       group_by(code) %>% 
       summarize(NAs = sum(is.na(exports_per_cap))) %>% 
       filter(NAs < 6)
# 1995: 0 -> 106, <6 -> 111
# 2000: 0 -> 111, <6 -> 118
imports_per_cap_left <- imports_with_per_cap %>% 
       filter(year %in% as.list(year_start_range:year_end_range)) %>% 
       group_by(code) %>% 
       summarize(NAs = sum(is.na(imports_per_cap))) %>% 
       filter(NAs < 6)

# 1995: 0 -> 61, <6 -> 109
# 2000: 0 -> 96, <6 -> 118
wa_rca_left <- wa_rca %>% 
       filter(year %in% as.list(year_start_range:year_end_range)) %>% 
       group_by(code) %>% 
       summarize(NAs = sum(is.na(wa_rca))) %>% 
       filter(NAs < 6)

# Control Variables

# 1995: every country has 4 missing values
# 2000: every country has 1 missing value
composite_wgi_left <- composite_wgi %>% 
       filter(year %in% as.list(year_start_range:year_end_range)) %>% 
       group_by(code) %>% 
       summarize(NAs = sum(is.na(composite_wgi))) %>% 
       filter(NAs < 6)


# 1995: 0 -> 143, <6 -> 146
# 2000: 0 -> 143, <6 -> 146
gdp_per_cap_left <- gdp_per_cap %>% 
       filter(year %in% as.list(year_start_range:year_end_range)) %>% 
       group_by(code) %>% 
       summarize(NAs = sum(is.na(gdp_per_cap))) %>% 
       filter(NAs == 0)


# 1995: 0 -> 139, <6 -> 139
# 2000: 0 -> 139, <6 -> 139
# Tax Burden
tax_burden_left <- econ_freedom %>% 
       filter(year %in% as.list(year_start_range:year_end_range)) %>% 
       group_by(code) %>% 
       summarize(NAs = sum(is.na(tax_burden))) %>% 
       filter(NAs == 0)


# 1995: 0 -> 90, <6 -> 123
# 2000: 0 -> 122, <6 -> 140
income_taxes_left <- all_income_taxes %>% 
       filter(year %in% as.list(year_start_range:year_end_range)) %>% 
       group_by(code) %>% 
       summarize(NAs = sum(is.na(income_tax))) %>% 
       filter(NAs < 8)

# 1995: 0 -> 84, <6 -> 118
# 2000: 0 -> 116, <6 -> 138
trade_taxes_left <- all_trade_taxes %>% 
       filter(year %in% as.list(year_start_range:year_end_range)) %>% 
       group_by(code) %>% 
       summarize(NAs = sum(is.na(trade_tax))) %>% 
       filter(NAs < 6)


# 1995: 0 -> 85, <6 -> 119
# 2000: 0 -> 117, <6 -> 137
vat_taxes_left <- all_vat_taxes %>% 
       filter(year %in% as.list(year_start_range:year_end_range)) %>% 
       group_by(code) %>% 
       summarize(NAs = sum(is.na(vat_tax))) %>% 
       filter(NAs < 6)


# 1995: 0 -> 59, <6 -> 82
# 2000: 0 -> 82, <6 -> 92
social_contributions_left <- all_social_contributions %>% 
       filter(year %in% as.list(year_start_range:year_end_range)) %>% 
       group_by(code) %>% 
       summarize(NAs = sum(is.na(social_contributions))) %>% 
       filter(NAs < 6)


######################################
# Filter and Combining Data
######################################
# Determining Which Countries Satisfy all of the above
testing_country_combination <- merge(tariffs_left, exports_per_cap_left, by = c('code')) %>% rename('NAs.tariffs' = 'NAs.x', 'NAs.exports' = 'NAs.y')
testing_country_combination <- merge(testing_country_combination, imports_per_cap_left, by = c('code')) %>% rename('NAs.imports' = 'NAs')
testing_country_combination <- merge(testing_country_combination, wa_rca_left, by = c('code')) %>% rename('NAs.wa_rca' = 'NAs')
testing_country_combination <- merge(testing_country_combination, composite_wgi_left, by = c('code')) %>% rename('NAs.composite_wgi' = 'NAs')
testing_country_combination <- merge(testing_country_combination, gdp_per_cap_left, by = c('code')) %>% rename('NAs.gdp_per_cap' = 'NAs')
testing_country_combination <- merge(testing_country_combination, tax_burden_left, by = c('code')) %>% rename('NAs.tax_burden' = 'NAs')
testing_country_combination <- merge(testing_country_combination, income_taxes_left, by = c('code')) %>% rename('NAs.income_taxes' = 'NAs')
testing_country_combination <- merge(testing_country_combination, trade_taxes_left, by = c('code')) %>% rename('NAs.trade_taxes' = 'NAs')
testing_country_combination <- merge(testing_country_combination, vat_taxes_left, by = c('code')) %>% rename('NAs.vat_taxes' = 'NAs')
nrow(testing_country_combination)
# testing_country_combination <- merge(testing_country_combination, social_contributions_left, by = c('code')) %>% rename('NAs.social_contributions' = 'NAs')
# nrow(testing_country_combination)
remaining_country_codes <- testing_country_combination$code

se_size_final <- se_size %>% select(code, year, se_size) %>% 
  filter(code %in% remaining_country_codes) %>% filter(year %in% as.character(2000:2015))
tariff_rate_final <- tariff_rate  %>% select(code, year, applied_tariff_rate) %>%
  filter(code %in% remaining_country_codes) %>% filter(year %in% as.character(2000:2015))
exports_per_cap_final <- exports_with_per_cap  %>% select(code, year, exports_per_cap) %>%
  filter(code %in% remaining_country_codes) %>% filter(year %in% as.character(2000:2015))
imports_per_cap_final <- imports_with_per_cap  %>% select(code, year, imports_per_cap) %>%
  filter(code %in% remaining_country_codes) %>% filter(year %in% as.character(2000:2015))
wa_rca_final <- wa_rca  %>% select(code, year, wa_rca) %>%
  filter(code %in% remaining_country_codes) %>% filter(year %in% as.character(2000:2015))
composite_wgi_final <- composite_wgi %>% select(code, year, composite_wgi) %>% 
  filter(code %in% remaining_country_codes) %>% filter(year %in% as.character(2000:2015))
gdp_per_cap_final <- gdp_per_cap  %>% select(code, year, gdp_per_cap) %>% 
  filter(code %in% remaining_country_codes) %>% filter(year %in% as.character(2000:2015))
tax_burden_final <- econ_freedom %>% select(code, year, tax_burden) %>% 
  filter(code %in% remaining_country_codes) %>% filter(year %in% as.character(2000:2015))
income_tax_final <- all_income_taxes  %>% select(code, year, income_tax)%>% 
  filter(code %in% remaining_country_codes) %>% filter(year %in% as.character(2000:2015))
trade_taxes_final <- all_trade_taxes  %>% select(code, year, trade_tax) %>%
  filter(code %in% remaining_country_codes) %>% filter(year %in% as.character(2000:2015))
vat_taxes_final <- all_vat_taxes  %>% select(code, year, vat_tax) %>%
  filter(code %in% remaining_country_codes) %>% filter(year %in% as.character(2000:2015))

all_final_dfs <- list(se_size_final, tariff_rate_final, exports_per_cap_final, imports_per_cap_final, wa_rca_final, 
                      composite_wgi_final, gdp_per_cap_final, tax_burden_final, income_tax_final, trade_taxes_final, vat_taxes_final)
trade_and_the_se_df <- merge(all_final_dfs[1], all_final_dfs[2], by = c('code', 'year'))
for (i in 3:length(all_final_dfs)) {
  trade_and_the_se_df <- merge(trade_and_the_se_df, all_final_dfs[i], by = c('code', 'year'))
}
View(trade_and_the_se_df)


######################################
# Interpolating Data
######################################
colSums(is.na(trade_and_the_se_df))

# composite WGI is missing all 2001 years, so interpolating shouldn't be a huge problem (it cannot change that suddenly)
# It is probably worth doing variance analyses on these to determine if the changes can be sudden
trade_and_the_se_df$composite_wgi <- na.approx(trade_and_the_se_df$composite_wgi)
colSums(is.na(trade_and_the_se_df))

# Taxes
trade_and_the_se_df$income_tax <- na.approx(trade_and_the_se_df$income_tax) # 13 / 1234 missing values
trade_and_the_se_df$trade_tax <- na.approx(trade_and_the_se_df$trade_tax) # 28 / 1234 missing values
trade_and_the_se_df$vat_tax <- na.approx(trade_and_the_se_df$vat_tax) # 14 / 1234 missing values
colSums(is.na(trade_and_the_se_df))

# wa_rca
trade_and_the_se_df$wa_rca <- na.approx(trade_and_the_se_df$wa_rca) # 29 / 1234 missing values, look at summary country statistics

# applied_tariff rate
trade_and_the_se_df$applied_tariff_rate <- na.approx(trade_and_the_se_df$applied_tariff_rate) # 88 / 1234 missing values, this one needs a close look because some countries are missing large values
colSums(is.na(trade_and_the_se_df))

# This undoes the regression's tendency to treat tax_burden as a factor for fixed effects
trade_and_the_se_df$tax_burden <- factor(trade_and_the_se_df$tax_burden) 
trade_and_the_se_df$tax_burden <- unfactor(trade_and_the_se_df$tax_burden) 
View(trade_and_the_se_df)

write.csv2(trade_and_the_se_df, file = get_file_path('trade_and_the_se.csv'))


######################################
# Regressions
######################################
options(scipen=999)

# Pooled OLS
# w/o control variables
pooled_ols_wo_controls <- lm(se_size ~ trade_tax + applied_tariff_rate + exports_per_cap + imports_per_cap + wa_rca, data = trade_and_the_se_df)
coeftest(pooled_ols_wo_controls, vcov = vcovHC(pooled_ols_wo_controls, type = "HC0"))
summary(pooled_ols_wo_controls)
stargazer(pooled_ols_wo_controls, type = "text")

# w/ control variables
pooled_ols_with_controls <- lm(se_size ~ trade_tax + applied_tariff_rate + exports_per_cap + imports_per_cap + wa_rca + 
                                 composite_wgi + gdp_per_cap + income_tax + vat_tax + tax_burden, data = trade_and_the_se_df)
coeftest(pooled_ols_with_controls, vcov = vcovHC(pooled_ols_with_controls, type = "HC0"))
summary(pooled_ols_with_controls)
stargazer(pooled_ols_with_controls, type = "text")

# Fixed and Random Effects
form_wo_controls <- se_size ~ trade_tax + applied_tariff_rate + exports_per_cap + imports_per_cap + wa_rca
form_with_controls <- se_size ~ trade_tax + applied_tariff_rate + exports_per_cap + imports_per_cap + wa_rca + 
                  composite_wgi + gdp_per_cap + income_tax + vat_tax + tax_burden

random_effects_wo_controls <- plm(form_wo_controls, data = trade_and_the_se_df, model = 'random')
coeftest(random_effects_wo_controls, vcov = vcovHC(random_effects_wo_controls, type = "HC0"))
summary(random_effects_wo_controls)
stargazer(random_effects_wo_controls, type = "text")


random_effects_with_controls <- plm(form_with_controls, data = trade_and_the_se_df, model = 'random')
coeftest(random_effects_with_controls, vcov = vcovHC(random_effects_with_controls, type = "HC0"))
summary(random_effects_with_controls)
stargazer(random_effects_with_controls, type = "text")


fixed_effects_wo_controls <- plm(form_wo_controls, data = trade_and_the_se_df, model = 'within')
coeftest(fixed_effects_wo_controls, vcov = vcovHC(fixed_effects_wo_controls, type = "HC0"))
summary(fixed_effects_wo_controls)
stargazer(fixed_effects_wo_controls, type = "text")

fixed_effects_with_controls <- plm(form_with_controls, data = trade_and_the_se_df, model = 'within')
coeftest(fixed_effects_with_controls, vcov = vcovHC(fixed_effects_with_controls, type = "HC0"))
summary(fixed_effects_with_controls)
stargazer(fixed_effects_with_controls, type = "text")


time_fixed_form_wo_controls <- se_size ~ trade_tax + applied_tariff_rate + exports_per_cap + imports_per_cap + wa_rca + factor(year)
time_fixed_form_wo_controls <- plm(country_fixed_form_wo_controls, data = trade_and_the_se_df, model = 'within')
coeftest(time_fixed_form_wo_controls, vcov = vcovHC(time_fixed_form_wo_controls, type = "HC0"))
summary(time_fixed_form_wo_controls)
stargazer(time_fixed_form_wo_controls, type = "text")



# Hausman Test
phtest(fixed_effects, random_effects)
# We should use fixed effects








