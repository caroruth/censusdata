library(tidyverse)
library(tidycensus)
library(janitor)

census_api_key("...")


acs_vars <- read_csv(".../variables acs5dec.csv") %>%
  remove_empty("cols") %>%
  clean_names()

vars_all_years <- list()

for (i in 1:13) {
  
  vars <- acs_vars %>%
    select(c(1, 1 + i)) %>%
    na.omit()
  
  unnamed_vars <- vars %>% pull(2)
  names(unnamed_vars) <- vars$variable_name
  vars_all_years[[i]] <- unnamed_vars
  
}


library(sf)
library(units)

acs2020 <- get_acs(geography = "county", variables = vars_all_years[[1]], year = 2020, survey = "acs5", show_call=TRUE, geometry = T) %>%
  select(-moe) %>% 
  pivot_wider(names_from = variable, values_from = estimate) %>% 
  mutate(year = 2020, .after = NAME,
         area = 0.00000038610*st_area(geometry),
         density = drop_units(total_pop/area))


acs2019 <- get_acs(geography = "county", variables = vars_all_years[[2]], year = 2019, survey = "acs5", show_call=TRUE, geometry = T) %>%
  select(-moe) %>% 
  pivot_wider(names_from = variable, values_from = estimate) %>% 
  mutate(year = 2019, .after = NAME,
         area = 0.00000038610*st_area(geometry),
         density = drop_units(total_pop/area))

acs2018 <- get_acs(geography = "county", variables = vars_all_years[[3]], year = 2018, survey = "acs5", show_call=TRUE, geometry = T) %>%
  select(-moe) %>% 
  pivot_wider(names_from = variable, values_from = estimate) %>% 
  mutate(year = 2018, .after = NAME,
         area = 0.00000038610*st_area(geometry),
         density = drop_units(total_pop/area))

acs2017 <- get_acs(geography = "county", variables = vars_all_years[[4]], year = 2017, survey = "acs5", show_call=TRUE, geometry = T) %>%
  select(-moe) %>% 
  pivot_wider(names_from = variable, values_from = estimate) %>% 
  mutate(year = 2017, .after = NAME,
         area = 0.00000038610*st_area(geometry),
         density = drop_units(total_pop/area))

acs2016 <- get_acs(geography = "county", variables = vars_all_years[[5]], year = 2016, survey = "acs5", show_call=TRUE, geometry = T) %>%
  select(-moe) %>% 
  pivot_wider(names_from = variable, values_from = estimate) %>% 
  mutate(year = 2016, .after = NAME,
         area = 0.00000038610*st_area(geometry),
         density = drop_units(total_pop/area))

acs2015 <- get_acs(geography = "county", variables = vars_all_years[[6]], year = 2015, survey = "acs5", show_call=TRUE, geometry = T) %>%
  select(-moe) %>% 
  pivot_wider(names_from = variable, values_from = estimate) %>% 
  mutate(year = 2015, .after = NAME)

acs2014 <- get_acs(geography = "county", variables = vars_all_years[[7]], year = 2014, survey = "acs5", show_call=TRUE, geometry = T) %>%
  select(-moe) %>% 
  pivot_wider(names_from = variable, values_from = estimate) %>% 
  mutate(year = 2014, .after = NAME,
         area = 0.00000038610*st_area(geometry),
         density = drop_units(total_pop/area))

acs2013 <- get_acs(geography = "county", variables = vars_all_years[[8]], year = 2013, survey = "acs5", show_call=TRUE, geometry = T) %>%
  select(-moe) %>% 
  pivot_wider(names_from = variable, values_from = estimate) %>% 
  mutate(year = 2013, .after = NAME,
         area = 0.00000038610*st_area(geometry),
         density = drop_units(total_pop/area))

acs2012 <- get_acs(geography = "county", variables = vars_all_years[[9]], year = 2012, survey = "acs5", show_call=TRUE, geometry = T) %>%
  select(-moe) %>% 
  pivot_wider(names_from = variable, values_from = estimate) %>% 
  mutate(year = 2012, .after = NAME)

acs2011 <- get_acs(geography = "county", variables = vars_all_years[[10]], year = 2011, survey = "acs5", show_call=TRUE, geometry = T) %>%
  select(-moe) %>% 
  pivot_wider(names_from = variable, values_from = estimate) %>% 
  mutate(year = 2011, .after = NAME)

acs2010 <- get_acs(geography = "county", variables = vars_all_years[[11]], year = 2010, survey = "acs5", show_call=TRUE, geometry = T) %>%
  select(-moe) %>% 
  pivot_wider(names_from = variable, values_from = estimate) %>% 
  mutate(year = 2010, .after = NAME,
         area = 0.00000038610*st_area(geometry),
         density = drop_units(total_pop/area))


dec2000_sf1 <- get_decennial(geography = "county", variables = vars_all_years[[12]], sumfile = "sf1", year = 2000, show_call=TRUE, geometry = T) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(year = 2000, .after = NAME,
         area = 0.00000038610*st_area(geometry),
         density = drop_units(total_pop/area))

dec2000_sf3 <- get_decennial(geography = "county", variables = vars_all_years[[13]], sumfile = "sf3", year = 2000, show_call=TRUE) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(year = 2000, .after = NAME,
         male_white_lths = male_white_lths_a + male_white_lths_b,
         male_white_somecoll = male_white_somecoll_a + male_white_somecoll_b,
         male_white_collplus = male_white_collplus_a + male_white_collplus_b,
         female_white_lths = female_white_lths_a + female_white_lths_b,
         female_white_somecoll = female_white_somecoll_a + female_white_somecoll_b,
         female_white_collplus = female_white_collplus_a + female_white_collplus_b,
         male_black_lths = male_black_lths_a + male_black_lths_b,
         male_black_somecoll = male_black_somecoll_a + male_black_somecoll_b,
         male_black_collplus = male_black_collplus_a + male_black_collplus_b,
         female_black_lths = female_black_lths_a + female_black_lths_b,
         female_black_somecoll = female_black_somecoll_a + female_black_somecoll_b,
         female_black_collplus = female_black_collplus_a + female_black_collplus_b,
         male_aian_lths = male_aian_lths_a + male_aian_lths_b,
         male_aian_somecoll = male_aian_somecoll_a + male_aian_somecoll_b,
         male_aian_collplus = male_aian_collplus_a + male_aian_collplus_b,
         female_aian_lths = female_aian_lths_a + female_aian_lths_b,
         female_aian_somecoll = female_aian_somecoll_a + female_aian_somecoll_b,
         female_aian_collplus = female_aian_collplus_a + female_aian_collplus_b,
         male_asian_lths = male_asian_lths_a + male_asian_lths_b,
         male_asian_somecoll = male_asian_somecoll_a + male_asian_somecoll_b,
         male_asian_collplus = male_asian_collplus_a + male_asian_collplus_b,
         female_asian_lths = female_asian_lths_a + female_asian_lths_b,
         female_asian_somecoll = female_asian_somecoll_a + female_asian_somecoll_b,
         female_asian_collplus = female_asian_collplus_a + female_asian_collplus_b,
         male_nhopi_lths = male_nhopi_lths_a + male_nhopi_lths_b,
         male_nhopi_somecoll = male_nhopi_somecoll_a + male_nhopi_somecoll_b,
         male_nhopi_collplus = male_nhopi_collplus_a + male_nhopi_collplus_b,
         female_nhopi_lths = female_nhopi_lths_a + female_nhopi_lths_b,
         female_nhopi_somecoll = female_nhopi_somecoll_a + female_nhopi_somecoll_b,
         female_nhopi_collplus = female_nhopi_collplus_a + female_nhopi_collplus_b,
         male_other_lths = male_other_lths_a + male_other_lths_b,
         male_other_somecoll = male_other_somecoll_a + male_other_somecoll_b,
         male_other_collplus = male_other_collplus_a + male_other_collplus_b,
         female_other_lths = female_other_lths_a + female_other_lths_b,
         female_other_somecoll = female_other_somecoll_a + female_other_somecoll_b,
         female_other_collplus = female_other_collplus_a + female_other_collplus_b,
         male_multi_lths = male_multi_lths_a + male_multi_lths_b,
         male_multi_somecoll = male_multi_somecoll_a + male_multi_somecoll_b,
         male_multi_collplus = male_multi_collplus_a + male_multi_collplus_b,
         female_multi_lths = female_multi_lths_a + female_multi_lths_b,
         female_multi_somecoll = female_multi_somecoll_a + female_multi_somecoll_b,
         female_multi_collplus = female_multi_collplus_a + female_multi_collplus_b,
         male_white_nohisp_lths = male_white_nohisp_lths_a + male_white_nohisp_lths_b,
         male_white_nohisp_somecoll = male_white_nohisp_somecoll_a + male_white_nohisp_somecoll_b,
         male_white_nohisp_collplus = male_white_nohisp_collplus_a + male_white_nohisp_collplus_b,
         female_white_nohisp_lths = female_white_nohisp_lths_a + female_white_nohisp_lths_b,
         female_white_nohisp_somecoll = female_white_nohisp_somecoll_a + female_white_nohisp_somecoll_b,
         female_white_nohisp_collplus = female_white_nohisp_collplus_a + female_white_nohisp_collplus_b,
         male_hisp_lths = male_hisp_lths_a + male_hisp_lths_b,
         male_hisp_somecoll = male_hisp_somecoll_a + male_hisp_somecoll_b,
         male_hisp_collplus = male_hisp_collplus_a + male_hisp_collplus_b,
         female_hisp_lths = female_hisp_lths_a + female_hisp_lths_b,
         female_hisp_somecoll = female_hisp_somecoll_a + female_hisp_somecoll_b,
         female_hisp_collplus = female_hisp_collplus_a + female_hisp_collplus_b,
         est_disability_civilian = est_disability_civilian_1 + est_disability_civilian_2 + est_disability_civilian_3 + est_disability_civilian_4 + est_disability_civilian_5 +
           est_disability_civilian_6 + est_disability_civilian_7 + est_disability_civilian_8 + est_disability_civilian_9 + est_disability_civilian_10,
         est_not_labor = est_not_labor_a + est_not_labor_b,
         n_labor = n_labor_a + n_labor_b,
         n_unemp = n_unemp_a + n_unemp_b,
         est_ind_constr = est_ind_constr_a + est_ind_constr_b,
         est_ind_manufact = est_ind_manufact_a + est_ind_manufact_b,
         est_ind_transp = est_ind_transp_a + est_ind_transp_b,
         est_labor_white = est_labor_white_a + est_labor_white_b,
         est_labor_black = est_labor_black_a + est_labor_black_b,
         est_labor_aian = est_labor_aian_a + est_labor_aian_b,
         est_labor_asian = est_labor_asian_a + est_labor_asian_b,
         est_labor_nhopi = est_labor_nhopi_a + est_labor_nhopi_b,
         est_labor_other = est_labor_other_a + est_labor_other_b,
         est_labor_multi = est_labor_multi_a + est_labor_multi_b,
         est_labor_hisp = est_labor_hisp_a + est_labor_hisp_b,
         est_labor_white_nohisp = est_labor_white_nohisp_a + est_labor_white_nohisp_b,
         est_unemp_white = est_unemp_white_a + est_unemp_white_b,
         est_unemp_black = est_unemp_black_a + est_unemp_black_b,
         est_unemp_aian = est_unemp_aian_a + est_unemp_aian_b,
         est_unemp_asian = est_unemp_asian_a + est_unemp_asian_b,
         est_unemp_nhopi = est_unemp_nhopi_a + est_unemp_nhopi_b,
         est_unemp_other = est_unemp_other_a + est_unemp_other_b,
         est_unemp_multi = est_unemp_multi_a + est_unemp_multi_b,
         est_unemp_hisp = est_unemp_hisp_a + est_unemp_hisp_b,
         est_unemp_white_nohisp = est_unemp_white_nohisp_a + est_unemp_white_nohisp_b) %>%
  select(-ends_with(c("_a", "_b", "_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8", "_9", "_10")))

dec2000 <- full_join(dec2000_sf1, dec2000_sf3) %>%
  mutate_at(vars(starts_with(c("male", "female", "white", "black", "hisp", "est", "pov", "soc", "supp", "fam"))), ~(100* . / total_pop)) %>%
  rename_at(vars(starts_with(c("male", "female", "white", "black", "hisp", "est", "pov", "soc", "supp", "fam"))), function(x){paste0("p_", x)}) %>%
  mutate(p_unemp = 100*n_unemp/n_labor) %>%
  select(-starts_with(c("est_labor", 	"est_unemp")))


acs <- bind_rows(acs2010, acs2011, acs2012, acs2013, acs2014, acs2015, acs2016, acs2017, acs2018, acs2019, acs2020) %>% 
  mutate_at(vars(starts_with(c("male", "female", "white", "black", "hisp", "est", "pov", "soc", "supp", "fam"))), ~(100* . / total_pop)) %>%
  rename_at(vars(starts_with(c("male", "female", "white", "black", "hisp", "est", "pov", "soc", "supp", "fam"))), function(x){paste0("p_", x)}) %>%
  mutate(p_unemp = 100*n_unemp/n_labor)


acs_dec <- bind_rows(dec2000, acs) %>%
  select(-geometry)

write.csv(acs_dec, file = ".../acs_2000_2020_090622.csv")





