library(tidyverse)

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
            as.numeric)

## read ornament data
kwl_upper <-
  readxl::read_excel(here("analysis", "data", "raw_data", "Kiwulan_Ornament_Upper.xlsx"))

kwl_lp_clean_summary <-
  kwl_lp_clean %>%
  group_by(end) %>%
  summarise(mean_weight= mean(重量小計, na.rm = TRUE))

kwl_lp_clean_piece <-
  kwl_lp_clean %>%
  group_by(`編號\n/ 現象號(P)`) %>%
  summarise(sum_p= sum(件數小計, na.rm = TRUE))

# plot
ggplot(kwl_lp_clean,
       aes(x = end,
           y = 重量小計)) +
  geom_line() +
  facet_wrap(~`編號\n/ 現象號(P)`)

ggplot(kwl_lp_clean,
       aes(x = end,
           y = 重量小計,
           colour = `編號\n/ 現象號(P)`)) +
  geom_line()

ggplot() +
  geom_line(data = kwl_lp_clean,
            aes(x = end,
                y = 重量小計,
                colour = `編號\n/ 現象號(P)`),
            alpha = 0.4) +
  geom_line(data = kwl_lp_clean_summary,
            aes(end,
                mean_weight),
            size = 1.5) +
  labs(colour = "Units",
       x = "Depth (cm)",
       y = "Weights of Sherds") +
  theme_minimal()

ggsave("pottery-per-level.png", w = 7, h = 5.5, dpi = 600)

