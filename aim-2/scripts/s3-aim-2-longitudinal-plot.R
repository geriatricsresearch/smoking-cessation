lib_loc <- 'E:\\Edie\\R\\Libraries'
assign('.lib.loc', lib_loc, envir=environment(.libPaths))


library(haven, lib.loc=lib_loc)
library(stringr, lib.loc=lib_loc)
library(reshape2, lib.loc=lib_loc)
library(tidyr, lib.loc=lib_loc)
library(dplyr, lib.loc=lib_loc)
library(ggplot2, lib.loc=lib_loc)
library(readr, lib.loc=lib_loc)

long_data_v0 <- read_csv('V:/Health and Retirement Study/edie/projects/smoking-cessation/aim-2-long.csv')


long_data_v0 <- long_data_v0 %>% arrange(HHIDPN, YEAR)

# 2,496
# long_data_v0 %>% filter(SUBPOP==1) %>% pull(HHIDPN) %>% unique() %>% length()


unique_hhidpn <- long_data_v0 %>% filter(SUBPOP==1) %>% select(HHIDPN) %>% distinct() %>% pull(HHIDPN)
years         <- seq(1998, 2016, 2)
shell_table   <- cbind(HHIDPN=rep(unique_hhidpn, each=length(years)), YEAR=rep(years, length(unique_hhidpn)))
shell_table_2 <- data.frame(shell_table)


long_data_v1  <- left_join(shell_table_2,
                           long_data_v0 %>% filter(SUBPOP==1) %>% select(HHIDPN, YEAR, DEATH, START_CESSATION, CESSATION, DDATE, START, BDATE, IWTYPE, SMOKEN)) %>%
  mutate(START_CESSATION=as.Date(START_CESSATION, origin='1960-01-01')) %>%
  mutate(START=as.Date(START, format='%m/%d/%Y')) %>%
  mutate(CESSATION=ifelse(!is.na(START_CESSATION) & START_CESSATION<=START, 1, CESSATION))






long_data_v2 <- long_data_v1 %>%
  mutate(AGE_1998=difftime(as.Date('1998-01-01', format='%Y-%m-%d'),
                           as.Date(BDATE, format='%m/%d/%Y'))/325.25 %>% as.numeric()) %>%
  group_by(HHIDPN) %>%
  tidyr::fill(DDATE, AGE_1998) %>%
  tidyr::fill(DEATH, .direction='down') %>%
  tidyr::fill(CESSATION, .direction='down') %>%
  
  mutate(DEATH=ifelse(!is.na(IWTYPE) & IWTYPE %in% c(11, 15), 1, DEATH)) %>%
  
  mutate(DEATH=ifelse(!is.na(lag(IWTYPE)) & lag(IWTYPE %in% c(11, 15)) |
                      !is.na(lag(IWTYPE, 2)) & lag(IWTYPE %in% c(11, 15), 2) |
                      !is.na(lag(IWTYPE, 3)) & lag(IWTYPE %in% c(11, 15), 3), 1, DEATH))

long_data_v3 <- long_data_v2 %>%
  group_by(HHIDPN) %>%
  mutate(CESSATION=ifelse(!is.na(lag(CESSATION,1)),
                                 ifelse(lag(CESSATION,1)==1 & CESSATION==0, 1, CESSATION), CESSATION)) %>%
  
  mutate(CESSATION=ifelse(!is.na(lag(CESSATION,2)),
                          ifelse(lag(CESSATION,2)==1 & CESSATION==0, 1, CESSATION), CESSATION)) %>%
  
  mutate(CESSATION=ifelse(!is.na(lag(CESSATION,3)),
                          ifelse(lag(CESSATION,3)==1 & CESSATION==0, 1, CESSATION), CESSATION)) %>%
  
  mutate(CESSATION=ifelse(!is.na(lag(CESSATION,4)),
                          ifelse(lag(CESSATION,4)==1 & CESSATION==0, 1, CESSATION), CESSATION)) %>%
  
  mutate(CESSATION=ifelse(!is.na(lag(CESSATION,5)),
                          ifelse(lag(CESSATION,5)==1 & CESSATION==0, 1, CESSATION), CESSATION)) %>%
  
  mutate(CESSATION=ifelse(!is.na(lag(CESSATION,6)),
                          ifelse(lag(CESSATION,6)==1 & CESSATION==0, 1, CESSATION), CESSATION)) %>%
  
  mutate(CESSATION=ifelse(!is.na(lag(CESSATION,7)),
                          ifelse(lag(CESSATION,7)==1 & CESSATION==0, 1, CESSATION), CESSATION)) %>%
  
  mutate(CESSATION=ifelse(!is.na(lag(CESSATION,8)),
                          ifelse(lag(CESSATION,8)==1 & CESSATION==0, 1, CESSATION), CESSATION)) %>%
  
  mutate(CESSATION=ifelse(!is.na(lag(CESSATION,9)),
                          ifelse(lag(CESSATION,9)==1 & CESSATION==0, 1, CESSATION), CESSATION))
  
  
  
  
  
  
  
long_data_v4 <- long_data_v3 %>%
  group_by(HHIDPN) %>%
  mutate(ANY_QUIT=any(CESSATION==1, na.rm=TRUE)) %>%
  mutate(Group=ifelse(DEATH==0 & CESSATION==0, 'No Quit',
                      ifelse(DEATH==0 & CESSATION==1, 'Quit',
                             ifelse(DEATH==1 & ANY_QUIT==0, 'Dead, No Quit',
                                    ifelse(DEATH==1 & ANY_QUIT==1, 'Dead, Quit', NA))))) %>%
  arrange(HHIDPN, YEAR)


count_tab_1 <- long_data_v4 %>%
  group_by(YEAR, Group) %>%
  count() %>%
  reshape2::dcast(YEAR ~ Group, value.var='n')

count_tab_1[is.na(count_tab_1)] <- 0

write_csv(count_tab_1, '../tables/cohort-longitudinal-v1-counts.csv')


long_data_v4 %>%
  ggplot(aes(x=YEAR, fill=Group)) +
  geom_bar(position='fill') +
  scale_x_continuous(breaks=seq(1998, 2018, 2)) +
  theme_classic() +
  theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=0.25)) +
  scale_fill_manual(values=c('grey', '#7F8778', '#608341', 'pink', 'pink3')) +
  ggtitle(label='Cohort Distribution Over Study Period',
          subtitle='Aged 55+ in 1998') +
  xlab('Year') +
  ylab('Proportion')

ggsave(plot=last_plot(), filename='../plots/cohort-longitudinal-v1.png', dpi=300, height=7, width=10)


count_tab_2 <- long_data_v4 %>%
  filter(AGE_1998<=65) %>%
  group_by(YEAR, Group) %>%
  count() %>%
  reshape2::dcast(YEAR ~ Group, value.var='n')
count_tab_2[is.na(count_tab_2)] <- 0

write_csv(count_tab_2, '../tables/cohort-longitudinal-v2-counts.csv')


long_data_v4 %>%
  filter(AGE_1998<=65) %>%
  ggplot(aes(x=YEAR, fill=Group)) +
  geom_bar(position='fill') +
  scale_x_continuous(breaks=seq(1998, 2018, 2)) +
  theme_classic() +
  theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=0.25)) +
  scale_fill_manual(values=c('grey', '#7F8778', '#608341', 'pink', 'pink3')) +
  ggtitle(label='Cohort Distribution Over Study Period',
          subtitle='Aged 55-65 in 1998') +
  xlab('Year') +
  ylab('Proportion')

ggsave(plot=last_plot(), filename='../plots/cohort-longitudinal-v2.png', dpi=300, height=7, width=10)

  

# 10196010

long_data_v5 <- left_join(long_data_v4, long_data_v0 %>% filter(YEAR==1998) %>% select(HHIDPN, WGTR, SECU, STRATUM))
write_dta(long_data_v5, 'V:/Health and Retirement Study/edie/projects/smoking-cessation/aim-2-long-v2.dta')
