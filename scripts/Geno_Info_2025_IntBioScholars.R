library(tidyverse)
pop_loc <- read_csv("input/WL2_2025_Data/2025_Pop_Loc_Info Updated.csv")
geno_info <- pop_loc %>% 
  select(Unique.ID, Genotype=pop.id) %>% 
  filter(!is.na(Unique.ID), Unique.ID!="buffer") %>% 
  mutate(Type=if_else(str_detect(Genotype, "\\) x"), "F2",
                      if_else(str_detect(Genotype, "x"), "F1", "Parent")))
write_csv(geno_info, "../2025_Genotype_Type_Info.csv")
