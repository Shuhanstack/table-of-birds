library(tidyverse)
library(janitor)

# Life history data -----------------
life_history = read_csv(here::here("data", "ECOL_96_269", "Data_Files", "Amniote_Database_Aug_2015.csv"), na = "-999")

birds_life_history = life_history |>
  filter(class == "Aves") |>
  select(class:species, common_name, 
         birth_or_hatching_weight_g, adult_body_mass_g, 
         litter_or_clutch_size_n, litters_or_clutches_per_y,
         fledging_age_d, male_maturity_d,  female_maturity_d,
         maximum_longevity_y,
         adult_svl_cm) |> 
  drop_na() |>
  mutate(scientific_name = paste0(genus, " ", species))

# IUCN data -----------------
library(rio)
iucn_vernacularname = import(here::here("data", "iucn-2022-1", "vernacularname.txt")) |>
  filter(V2 == "eng") |>
  select(V1, V3) |>
  `colnames<-`(c("iucn_species_id", "common_name")) |>
  mutate(iucn_species_id = as.character(iucn_species_id))

iucn_df = import(here::here("data", "iucn-2022-1", "distribution.txt")) |>
  select(1,4) |>
  `colnames<-`(c("iucn_species_id", "iucn_red_list")) |>
  mutate(iucn_species_id = as.character(iucn_species_id))

# validate each common name only has one iucn_species_id
iucn_vernacularname |>
  filter(common_name %in% birds_life_history$common_name) |>
  distinct() |>
  count(common_name) |>
  filter(n > 1) 

birds_life_history_IUCN = iucn_vernacularname |>
  filter(common_name %in% birds_life_history$common_name) |>
  distinct() |>
  left_join(iucn_df, by = "iucn_species_id") |>
  left_join(birds_life_history, by = c("common_name" = "common_name")) |>
  filter(iucn_red_list != "Data Deficient") |>
  # Black Scoter has two iucn_species_id, filter one out
  filter(common_name != "Black Scoter" | iucn_red_list != "Least Concern") |>
  select(-iucn_species_id) |>
  select(iucn_red_list, everything())

# Diet data -----------------
# # install.packages("devtools")
# devtools::install_github("ahhurlbert/aviandietdb")
library(aviandietdb)

birds_diet = lapply(birds_life_history_IUCN$common_name, function(x) {
  diet_order = aviandietdb::dietSummary(x, by = "Class")
  if (is.null(diet_order)) {
    NULL
  } else {
    diet_order |>
      mutate(common_name = x) |>
      clean_names() |>
      select(common_name, prey_class = taxon, frac_diet)
  }
}) |>
  bind_rows()

birds_diet_common= birds_diet |>
  filter(! str_detect(prey_class, "Animalia")) |>
  filter(! str_detect(prey_class, "Chordata")) |> 
  filter(! str_detect(prey_class, "Unid. Arthropoda")) |>
  mutate(diet = case_when(
    prey_class == "Aves"                    ~ "bird",
    prey_class == "Reptilia"               ~ "reptile",
    prey_class == "Mammalia"             ~ "mammal",
    prey_class == "Euchelicerata"         ~ "spider",
    prey_class == "Amphibia"              ~ "amphibian",
    prey_class == "Teleostei"        ~ "fish",
    prey_class == "Insecta"          ~ "insect",
    prey_class == "Chilopoda"        ~ "centipede",
    prey_class == "Clitellata"       ~ "worm",
    prey_class == "Malacostraca"           ~ "crustacean",
    prey_class == "Gastropoda"             ~ "snail",
    prey_class == "Magnoliopsida"          ~ "plant",
    str_detect(prey_class, "Tracheophyta") ~ "plant",
    str_detect(prey_class, "Charophyceae") ~ "plant",
    str_detect(prey_class, "Mollusca")    ~ "snail",
    prey_class == "Bivalvia"               ~ "scallop",
    prey_class == "Polypodiopsida"            ~ "fern",
    prey_class == "Pinopsida"           ~ "conifer",
    prey_class == "Holostei"            ~ "fish",
    prey_class == "Cyanophyceae"          ~ "cyanobacteria",
    prey_class == "Lycopodiopsida"        ~ "moss",
    prey_class == "Diplopoda"             ~ "millipede",
  )) 

birds_diet_common |>
  filter(is.na(diet)) 

birds_diet_short = birds_diet_common |>
  distinct(common_name, diet)

# USGS data breeding bird survey data -----------------
usgs_species_list = import(here::here("data", "usgs_bird_survey", "SpeciesList.csv")) |>
  clean_names() |>
  mutate(english_common_name = str_to_title(english_common_name)) |>
  filter(english_common_name %in% birds_life_history_IUCN$common_name) |>
  mutate(aou = str_pad(aou, 5, pad = "0")) 

birds_life_history_IUCN_bbs = birds_life_history_IUCN |>
  inner_join(usgs_species_list |>
              select(aou, common_name = english_common_name),
            by = "common_name") |>
  select(iucn_red_list, common_name, scientific_name, genus, species, 
         birth_or_hatching_weight_g, adult_body_mass_g, litter_or_clutch_size_n, litters_or_clutches_per_y, 
         fledging_age_d, male_maturity_d, female_maturity_d,
         maximum_longevity_y, adult_svl_cm, aou)

## summarize annual observation for species in birds_life_history_IUCN_bbs ----
path = here::here("data", "usgs_bird_survey", "50-StopData")
csv_list = list.files(path, full.names = TRUE)
annual_num_observation = lapply(csv_list, function(x){
  df = read_csv(x) |>
    clean_names() |>
    # filter to only bird species in my_birds
    filter(aou %in% birds_life_history_IUCN_bbs$aou) 
  
  if(nrow(df) == 0){
    # skip further data processing if no bird species in my_birds and return NULL
    return(NULL)
  } else {
    # otherwise, convert to long format 
    df |>
      pivot_longer(cols = stop1:stop50,
                   names_to = "stop",
                   values_to = "num_observation") |>
      select(aou, year, num_observation)
  }
}) |>
  # combine all dataframes into one
  bind_rows() |>
  # summarize total number of observation by year and bird species
  summarize(num_observation = sum(num_observation, 
                                  na.rm = TRUE),
            .by = c(aou, year)) |>
  # arrange by species and year
  arrange(aou, year) |>
  filter(year >= 2000) 


# Bird call data -----------------
bird_call_media = import(here::here("data", "Xeno-canto", "multimedia.txt")) |>
  filter(type == "Sound") |>
  select(gbifID, identifier, description)
bird_call_verbatim = import(here::here("data", "Xeno-canto", "verbatim.txt"))  |>
  select(gbifID, scientificName, common_name = vernacularName, genus, taxonRank)
bird_call_joined = bird_call_media |>
  left_join(bird_call_verbatim, by = "gbifID") |>
  mutate(duration = str_extract(description, "[0-9]+") |>
           as.numeric())


bird_call_joined |>
  group_by(genus, common_name) |>
  summarize(duration = mean(duration, na.rm = TRUE),
            n = n()) |>
  ungroup() |>
  summary()

bird_call_shorten = bird_call_joined  |>
  mutate(duration_q1 = quantile(duration, 0.25, na.rm = TRUE),
         duration_q3 = quantile(duration, 0.75, na.rm = TRUE)) |>
  filter(duration <= duration_q3 & duration >= duration_q1) |>
  filter(taxonRank == "species") |>
  select(genus, scientificName, common_name, identifier) |>
  distinct(genus, common_name, scientificName, .keep_all = TRUE)|>
  clean_names() 

count(bird_call_shorten, common_name) |>
  filter(n > 1) # validate that each common name only has one media

birds_life_history_IUCN_bbs_birdcall = birds_life_history_IUCN_bbs |>
  left_join(bird_call_shorten, 
            by = c("genus" = "genus",
                   "common_name" = "common_name",
                   "scientific_name" = "scientific_name")) |>
  select(iucn_red_list, common_name, genus, species, scientific_name, everything())

skimr::skim(birds_life_history_IUCN_bbs_birdcall) # overall completion rate is high :)

birds_life_history_IUCN_bbs_birdcall |>
  count(common_name) |>
  filter(n > 1)

# Save data -----------------
write.csv(birds_life_history_IUCN_bbs_birdcall,
          here::here("data", "intermediate_data", "birds_life_history_IUCN_bbs_birdcall.csv"), 
          row.names = FALSE)

write.csv(annual_num_observation, 
          here::here("data", "intermediate_data", "annual_num_observation.csv"), 
          row.names = FALSE)

write.csv(birds_diet_short, 
          here::here("data", "intermediate_data", "birds_diet_short.csv"), 
          row.names = FALSE)
