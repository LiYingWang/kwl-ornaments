library(tidyverse)
library(stringr)

# read data
kwl_lp <-
  readxl::read_excel(here("analysis", "data", "raw_data",
                          "舊社陶坑層統計(950501)-製作坑層分佈圖用.xls"),
                     col_names = TRUE,
                     skip = 1)

# P051 would be excluded because irregular depths
the_pits <- c(
  "P040",
  "P041",
  "P042",
  "P051",
  "P052",
  "P053",
  "P054",
  "P058",
  "P059",
  "P060",
  "P061",
  "P062",
  "P063",
  "P064",
  "P065",
  "P066",
  "P067",
  "P070",
  "P071",
  "P072",
  "P073",
  "P074",
  "P075AB",
  "P075CD",
  "P076",
  "P077",
  "P078",
  "P079",
  "P080",
  "P082",
  "P083",
  "P084",
  "P085",
  "P086",
  "P087",
  "P088",
  "P089",
  "P090",
  "P091",
  "P092",
  "P093"
)

## clean up
kwl_lp_clean <-
  kwl_lp %>%
  mutate_at(vars("編號\n/層位", "重量小計",  "件數小計"),
            as.numeric) %>%
  filter(!is.na(`編號\n/層位`)) %>%
  mutate(`編號\n/ 現象號(P)` = zoo::na.locf(`編號\n/ 現象號(P)`)) %>%
  filter(`編號\n/ 現象號(P)` %in% the_pits) %>%
  separate(`海拔深度\n(cm)`,
           into = c("start", "end"),
           sep = "~") %>%
  mutate_at(vars(start, end),
            as.numeric) %>%
  mutate(`編號\n/層位`= ifelse(nchar(`編號\n/層位`) == 1,
                           paste0("0", `編號\n/層位`),
                           `編號\n/層位`)) %>%
  mutate(join_id = paste0(`編號\n/ 現象號(P)`,"-",`編號\n/層位`))

## read ornament data
kwl_upper <-
  readxl::read_excel(here("analysis", "data", "raw_data", "Kiwulan_Ornament_Upper.xlsx"))

## clean the ornament data, assign periods
ornaments_period <-
  kwl_upper %>%
  filter(!is.na(`6-layers`)) %>%
  mutate(period = case_when(
    `6-layers` %in% 1:2 ~ "Chinese Presence",
    `6-layers` == 4 ~ "European Presence",
    `6-layers` %in% 5:6 ~ "Before European Contact",
    TRUE ~ "other"
  )) %>%
  filter(period != "other") %>%
  mutate(period = factor(period,
                         level = c("Before European Contact",
                                   "European Presence",
                                   "Chinese Presence"))) %>%
  mutate(join_id = paste0(Pit_No,"-",Layer)) %>%
  filter(Categories != "Unknown Metal") %>%
  mutate(Categories = fct_lump(Categories,
                               n = 5))

## join with potsherds data
ornaments_potsherds <-
  ornaments_period %>%
  left_join(kwl_lp_clean, "join_id") %>%
  rename(potsherds_weight = "重量小計") %>%
  mutate(potsherds_weight = as.numeric(potsherds_weight)) %>%
  group_by(join_id, period, potsherds_weight) %>%
  count(Layer) %>%
  select(-Layer) %>%
  rename(ornament_piece = "n") %>%
  group_by(period) %>%
  summarise(sum_pot= sum(potsherds_weight, na.rm = TRUE),
            sum_orna= sum(ornament_piece, na.rm = TRUE))

## plot
plot_ornaments_potsherds <-
  ornaments_potsherds %>%
  pivot_longer(-period,
               names_to = "variable",
               values_to = "value") %>%
  ggplot() +
  geom_col(aes(variable, value)) +
  facet_wrap(~ period) +
  scale_y_log10() +
  labs(x = "Artifact",
       y = "Amount") +
  theme_minimal(base_size = 10)

## test
chisq.test(ornaments_potsherds)
library(Hmisc)
rcorr(ornaments_potsherds, type="pearson")
t.test(ornaments_potsherds$sum_orna, ornaments_potsherds$sum_pot)
