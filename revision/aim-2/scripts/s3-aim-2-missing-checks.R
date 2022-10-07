# This script requires that you run s3-aim-2-longitudinal-plot.R first.

lib_loc <- 'E:\\Edie\\R\\Libraries'
assign('.lib.loc', lib_loc, envir=environment(.libPaths))


library(haven, lib.loc=lib_loc)
library(stringr, lib.loc=lib_loc)
library(reshape2, lib.loc=lib_loc)
library(tidyr, lib.loc=lib_loc)
library(dplyr, lib.loc=lib_loc)
library(ggplot2, lib.loc=lib_loc)
library(readr, lib.loc=lib_loc)


# Some checks pt. 1
death_waves <- long_data_v4 %>%
  group_by(HHIDPN) %>%
  filter(DEATH==1) %>%
  summarize(DEATH_WAVE=min(YEAR))

# IRENA'S Q1
tmp1 <- left_join(long_data_v4, death_waves) %>%
  filter(YEAR<DEATH_WAVE) %>%
  dplyr::bind_rows(long_data_v4 %>% filter(!any(DEATH==1))) %>%
  group_by(HHIDPN) %>%
  summarize(Missing=any(IWTYPE == 5 | is.na(IWTYPE))) %>%
  group_by(Missing) %>%
  count()

left_join(long_data_v2, death_waves) %>%
  filter(YEAR<DEATH_WAVE) %>%
  dplyr::bind_rows(long_data_v2 %>% filter(!any(DEATH==1))) %>%
  filter(is.na(IWTYPE)) %>%
  group_by(HHIDPN) %>%
  count()

left_join(long_data_v2, death_waves) %>%
  filter(YEAR<DEATH_WAVE) %>%
  dplyr::bind_rows(long_data_v2 %>% filter(!any(DEATH==1))) %>%
  filter(is.na(IWTYPE)) %>%
  group_by(HHIDPN) %>%
  count() %>%
  nrow()







long_data_missing <- long_data_v4 %>%
  group_by(HHIDPN) %>%
  mutate(ANCHOR=ifelse(!is.na(lag(SMOKEN,1)) & lag(SMOKEN,1)==1 & !is.na(SMOKEN) & SMOKEN==0, 1, 0))

# 10013010 has anchor year 2006
the_anchors <- long_data_missing %>%
  group_by(HHIDPN) %>%
  filter(ANCHOR==1) %>%
  select(HHIDPN, YEAR) %>%
  rename(ANCHOR_YEAR=YEAR)

# IRENA'S Q2A
long_data_missing_2 <- left_join(long_data_missing, the_anchors) %>%
  filter(!is.na(ANCHOR_YEAR)) %>%
  mutate(WHEN=ifelse(YEAR>ANCHOR_YEAR, 'Before First Report', 'After First Report')) %>%
  group_by(HHIDPN) %>%
  filter(IWTYPE == 5) %>%
  group_by(HHIDPN, WHEN) %>%
  count() %>%
  group_by(WHEN) %>%
  count

# QUESTION 2B
long_data_missing_2b <- left_join(long_data_missing, the_anchors) %>%
  filter(!is.na(ANCHOR_YEAR)) %>%
  mutate(WHEN=ifelse(YEAR>ANCHOR_YEAR, 'Before First Report', 'After First Report')) %>%
  group_by(HHIDPN) %>%
  filter(IWTYPE == 5) %>%
  group_by(HHIDPN, WHEN, Group) %>%
  count() %>%
  group_by(WHEN, Group) %>%
  count()

# QUESTION 2C
long_data_missing_2c <- left_join(long_data_missing, the_anchors) %>%
  filter(!is.na(ANCHOR_YEAR)) %>%
  mutate(WHEN=ifelse(YEAR>ANCHOR_YEAR, 'Before First Report', 'After First Report')) %>%
  group_by(HHIDPN) %>%
  filter(IWTYPE == 5) %>%
  filter(WHEN=='After First Report') %>%
  select(HHIDPN) %>%
  left_join(long_data_v4 %>% select(HHIDPN, YEAR, DDATE, SMOKEN, START_CESSATION, CESSATION, Group)) %>%
  left_join(the_anchors) %>%
  group_by(HHIDPN) %>%
  slice(tail(row_number(), 1)) %>%
  group_by(Group) %>%
  count()

View(long_data_missing_2c)



long_data_missing_3 <- left_join(long_data_missing, the_anchors) %>%
  filter(!is.na(ANCHOR_YEAR)) %>%
  mutate(WHEN=ifelse(YEAR>ANCHOR_YEAR, 'Before First Report', 'After First Report')) %>%
  right_join(long_data_v4) %>%
  filter(HHIDPN %in% the_anchors$HHIDPN) %>%
  group_by(HHIDPN, WHEN, Group) %>%
  count() %>%
  group_by(WHEN, Group) %>%
  count()




# 10114010  

long_data_v4 %>%
  group_by(HHIDPN) %>%
  filter(IWTYPE==5) %>%
  group_by(Group) %>%
  count()


# Some checks pt. 2
long_data_v4 %>%
  group_by(HHIDPN) %>%
  filter(DEATH != 1) %>%
  summarize(AtLeast1Missing=any(IWTYPE == 5))

long_data_v4 %>%
  group_by(HHIDPN) %>%
  summarize(n_missing=length(which(IWTYPE==5)))

long_data_v4 %>%
  group_by(HHIDPN) %>%
  filter(IWTYPE==5) %>%
  group_by(Group) %>%
  count()

final_wave_per_hhidpn <- long_data_v4 %>%
  group_by(HHIDPN) %>%
  filter(DEATH==1) %>%
  summarize(DEATH_WAVE=min(YEAR))


long_data_v4 %>%
  left_join(final_wave_per_hhidpn) %>%
  group_by(HHIDPN) %>%
  filter(lag(SMOKEN)==0 & lag(SMOKEN,2)==1 & lag(YEAR)==DEATH_WAVE & DEATH==1) %>%
  count() %>% View()


long_data_v4 %>%
  left_join(final_wave_per_hhidpn) %>%
  group_by(HHIDPN) %>%
  filter(lag(SMOKEN)==0 & lag(SMOKEN,2)==1 & lag(YEAR)<=DEATH_WAVE & DEATH==1) %>%
  ungroup() %>%
  count()


