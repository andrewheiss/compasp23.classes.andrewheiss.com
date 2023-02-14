# devtools::install_github("vdeminstitute/vdemdata")
library(dplyr)
library(readr)
library(ggplot2)
library(vdemdata)
library(countrycode)

small_or_contested <- c(
  43,   # Kosovo
  128,  # Palestine: West Bank
  132,  # Eswatini
  138,  # Palestine: Gaza
  139,  # Somaliland
  167,  # Hong Kong
  236   # Zanzibar
)

regimes <- tribble(
  ~regime, ~regime_type,
  0,       "Closed autocracy",
  1,       "Electoral autocracy",
  2,       "Electoral democracy",
  3,       "Liberal democracy",
) %>% 
  mutate(regime_type = factor(regime_type, ordered = TRUE))

vdem_2015 <- vdem %>% 
  select(country_name, country_text_id, country_id, year,
         population = e_pop, gdpPercap = e_gdppc, gdp = e_gdp, 
         govt_effectiveness = e_wbgi_gee,
         life_exp = e_pelifeex,
         impartial_ord = v2clrspct_ord, 
         pct_authority = v2svstterr, 
         meritocratic_hiring = v2stcritrecadm,
         regime = v2x_regime) %>% 
  filter(!(country_id %in% small_or_contested)) %>% 
  filter(year == 2015) %>% 
  mutate(population = population * 10000,
         gdp = gdp * 10000000,
         gdpPercap = gdpPercap * 1000) %>% 
  mutate(democracy = case_when(
    regime %in% 0:1 ~ "Autocracy",
    regime %in% 2:3 ~ "Democracy"
  )) %>%
  left_join(regimes, by = "regime") %>% 
  mutate(impartial = case_when(
    impartial_ord %in% 0:1 ~ FALSE,
    impartial_ord %in% 2:4 ~ TRUE
  )) %>% 
  mutate(impartial_num = as.numeric(impartial)) %>%
  mutate(impartial_ord = factor(impartial_ord, ordered = TRUE)) |> 
  mutate(continent = countrycode(
    country_id, origin = "vdem", destination = "continent")
  ) %>% 
  select(country_name, country_text_id, year, continent, govt_effectiveness,
         impartial, impartial_num, impartial_ord, meritocratic_hiring, pct_authority, 
         regime_type, democracy, population, gdp, gdpPercap, life_exp)

saveRDS(vdem_2015, "data/vdem_2015.rds")
write_csv(vdem_2015, "data/vdem_2015.csv")
haven::write_dta(vdem_2015, "data/vdem_2015.dta")
