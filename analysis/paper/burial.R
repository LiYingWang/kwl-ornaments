# read data
kwl_burial <-
  readxl::read_excel(here("analysis", "data", "raw_data", "KWL_burial_ornament.xls"))

# tidy up for all ornaments from burials
kwl_burial_all_tidy <-
  kwl_burial %>%
  group_by(`編號/空間號`, `坑號`, `類別`) %>%
  count() %>%
  mutate(count = str_extract(`類別`, "[0-9]+")) %>%
  mutate(`類別` = str_remove(`類別`, "[0-9]+")) %>%
  mutate(count = as.numeric(count)) %>%
  mutate(count = ifelse(is.na(count), n, count))

# filter the ornaments from burials in sampling area
kwl_burial_sampling <-
  kwl_burial %>%
  filter(!is.na(`坑號`)) %>%
  group_by(`編號/空間號`, `坑號`, `類別`) %>%
  count() %>%
  mutate(count = str_extract(`類別`, "[0-9]+")) %>%
  mutate(`類別` = str_remove(`類別`, "[0-9]+")) %>%
  mutate(count = as.numeric(count)) %>%
  mutate(count = ifelse(is.na(count), n, count))

# summarize for each class, filter out the extreme value for golden bead
KWL_all_burial <-
  kwl_burial_all_tidy %>%
  filter(!n == 60) %>%
  group_by(`類別`) %>%
  summarise(mean = mean(count))

# for sampling area
KWL_sam_burial<-
  kwl_burial_sampling %>%
  filter(!n == 60) %>%
  group_by(`類別`) %>%
  summarise(mean = mean(count))

