library(tidyverse)

pfish <- read_csv("data/parrotfish.csv")
taxo <- read_csv("data/taxonomic.csv")

long <- pfish %>% 
  pivot_longer(cols = c("CRY ROSE":"SPA VIRI"), # Select columns to pivot (all species columns)
               names_to = "SPECIES_CD", 
               values_to = "density")

strat_mean <- long %>%
  group_by(YEAR, REGION, STRAT, PROT, PRIMARY_SAMPLE_UNIT) %>%
  summarise(pf_tot = sum(density, na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(YEAR, REGION, STRAT) %>%
  summarise(mean_pf = mean(pf_tot, na.rm = TRUE), .groups = "drop")

strat_mean2 <- long %>% 
  group_by(YEAR, REGION, STRAT) %>%
  summarise(sample_n = n_distinct(PRIMARY_SAMPLE_UNIT),
            sum_pf = sum(density, na.rm = TRUE),
            mean_pf = (sum_pf / sample_n), .groups = "drop")

wider <- long %>% 
  left_join(taxo %>% select(SPECIES_CD, SCINAME), by = "SPECIES_CD") %>% 
  pivot_wider(names_from = "SCINAME", values_from = "density", values_fill = 0) 


separated_data <- long %>% 
  separate(STRAT, into = c("HABITAT", "DEPTHCAT"), sep = "_", remove = FALSE)

united_data <- long %>%
  unite(col = ID, c(YEAR, PRIMARY_SAMPLE_UNIT), sep = "_", remove = FALSE)


richness <- separated_data %>%
  group_by(YEAR, REGION, DEPTHCAT, PRIMARY_SAMPLE_UNIT) %>%
  filter(density > 0) %>% 
  summarise(rich = n_distinct(SPECIES_CD), .groups = "drop") %>% 
  group_by(YEAR, REGION, DEPTHCAT) %>%
  summarise(mean_rich = mean(rich, na.rm = TRUE), .groups = "drop")
