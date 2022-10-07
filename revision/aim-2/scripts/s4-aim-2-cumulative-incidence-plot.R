#  * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * 
# Script Setup ------------------------------------------- *
#
# | Author    | Edie Espejo
# | Study     | Smoking Cessation (Lauren Hunt)
# | Created   | 2021-12-07
# | Last Edit | 2022-06-03
# | Objective | Cumulative incidence plot





#  * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * 
# Libraries ---------------------------------------------- *

# lib_loc <- 'C:\\Users\\VHASFCEspejE\\Local Documents\\R\\Libraries'
lib_loc <- 'E:\\Edie\\R\\Libraries'
assign('.lib.loc', lib_loc, envir=environment(.libPaths))


library(haven, lib.loc=lib_loc)
library(stringr, lib.loc=lib_loc)
library(reshape2, lib.loc=lib_loc)
library(tidyr, lib.loc=lib_loc)
library(dplyr, lib.loc=lib_loc)
library(ggplot2, lib.loc=lib_loc)


stcurve_v1 <- read_dta('V:/Health and Retirement Study/edie/projects/smoking-cessation/2022-06-03 data/cif-stcurve-4.dta')

stcurve_v1 %>%
  rename(t=`_t`) %>%
  rename(`Less than 10`=ci1) %>%
  rename(`10 to 19`=ci2) %>%
  rename(`20 or more`=ci3) %>%
  reshape2::melt(id.vars='t') %>%
  mutate(t=55+t) %>%
  filter(variable %in% c('t', 'Less than 10', '10 to 19', '20 or more')) %>%
  rename(`Cigarettes Per Day`=variable) %>%
  ggplot(aes(x=t, y=value, group=`Cigarettes Per Day`)) +
  geom_point(aes(col=`Cigarettes Per Day`)) +
  geom_line(aes(col=`Cigarettes Per Day`)) +
  xlab('Age') +
  ylab('Cumulative Incidence') +
  ggtitle(label='Cumulative Incidence Plot',
          subtitle='Pamaretric, using `stcurve, cif`') +
  scale_color_manual(values=c('#608341', '#3d3d3d', 'pink3')) +
  theme_bw() +
  xlim(c(55+min(stcurve_v1$`_t`), 90))

ggsave(plot=last_plot(), filename='../plots/cif-stcurve.png', dpi=300, height=7, width=10)




stcurve_overall <- read_dta('V:/Health and Retirement Study/edie/projects/smoking-cessation/2022-06-03 data/stcurve-overall-4.dta')

stcurve_full <- left_join(stcurve_v1  %>%
                            rename(t=`_t`) %>%
                            rename(`Less than 10`=ci1) %>%
                            rename(`10 to 19`=ci2) %>%
                            rename(`20 or more`=ci3),
                          stcurve_overall %>% rename(t=`_t`) %>% rename(Overall=ci1))

stcurve_full %>%
  reshape2::melt(id.vars='t') %>%
  mutate(t=55+t) %>%
  filter(variable %in% c('t', 'Less than 10', '10 to 19', '20 or more', 'Overall')) %>%
  rename(`Cigarettes Per Day`=variable) %>%
  ggplot(aes(x=t, y=value, group=`Cigarettes Per Day`)) +
  geom_point(aes(col=`Cigarettes Per Day`), alpha=0.7) +
  geom_line(aes(col=`Cigarettes Per Day`), alpha=0.7) +
  xlab('Age') +
  ylab('Cumulative Incidence') +
  scale_color_manual(values=c('#608341', '#3d3d3d', 'pink3', 'cornflowerblue')) +
  theme_bw() +
  xlim(c(56.83504, 90)) +
  theme(axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        legend.text=element_text(size=15),
        axis.title.x=element_text(face='bold', size=15),
        axis.title.y=element_text(face='bold', size=15),
        legend.title=element_text(face='bold', size=15),
        title=element_text(face='bold', size=18))

ggsave(plot=last_plot(), filename='../plots/cif-stcurve-overall.png', dpi=300, height=7, width=10)






# stcurve_v2 <- read_dta('../data/stcurve-v2.dta')
# stcurve_v2 %>%
#   rename(t=`_t`) %>%
#   rename(`Men`=ci1) %>%
#   rename(`Women`=ci2) %>%
#   reshape2::melt(id.vars='t') %>%
#   mutate(t=55+t) %>%
#   rename(`Gender`=variable) %>%
#   ggplot(aes(x=t, y=value, group=`Gender`)) +
#   geom_point(aes(col=`Gender`)) +
#   geom_line(aes(col=`Gender`)) +
#   xlab('Age') +
#   ylab('Cumulative Incidence') +
#   theme_bw()



stcompet_curve <- readr::read_csv('V:/Health and Retirement Study/edie/projects/smoking-cessation/2022-06-03 data/cif-stcompet-data-4.csv')
stcompet_curve_v2 <- stcompet_curve %>%
  select(cif_cig0, cif_cig1, cif_cig2, `_t`) %>%
  filter(!is.na(cif_cig0) | !is.na(cif_cig1) | !is.na(cif_cig2)) %>%
  rename(`Less than 10`=cif_cig0) %>%
  rename(`10 to 19`=cif_cig1) %>%
  rename(`20 or more`=cif_cig2) %>%
  rename(t=`_t`) %>%
  mutate(t=t+55)

stcompet_curve_v3 <- reshape2::melt(stcompet_curve_v2, id.vars=c('t')) %>%
  filter(!is.na(value))

stcompet_curve_v3 %>%
  filter(t<=90) %>%
  rename(`Cigarettes Per Day`=variable) %>%
  ggplot(aes(x=t, y=value, group=`Cigarettes Per Day`)) +
  geom_line(aes(col=`Cigarettes Per Day`)) +
  geom_point(aes(col=`Cigarettes Per Day`)) +
  xlab('Age') +
  ylab('Cumulative Incidence') +
  theme_bw() +
  ggtitle(label='Cumulative Incidence Plot',
          subtitle='Pamaretric, using `stcompet, cif`') +
  scale_color_manual(values=c('#608341', '#3d3d3d',  'pink3')) +
  theme_bw()


ggsave(plot=last_plot(), filename='../plots/cif-stcompet.png', dpi=300, height=7, width=10)



stcompet_overall <- readr::read_csv('V:/Health and Retirement Study/edie/projects/smoking-cessation/2022-06-03 data/cif-stcompet-overall-4.csv')
stcompet_overall <- stcompet_overall %>%
  select(`_t`, cif_overall) %>%
  rename(t=`_t`) %>%
  filter(t<=90) %>%
  rename(Overall=cif_overall) %>%
  mutate(t=t+55)

stcompet_full <- full_join(stcompet_curve_v2, stcompet_overall) %>%
  reshape2::melt(id.vars=c('t')) %>%
  filter(!is.na(value))

stcompet_full %>%
  rename(`Cigarettes Per Day`=variable) %>%
  ggplot(aes(x=t, y=value, group=`Cigarettes Per Day`)) +
  geom_line(aes(col=`Cigarettes Per Day`)) +
  geom_point(aes(col=`Cigarettes Per Day`)) +
  xlab('Age') +
  ylab('Cumulative Incidence') +
  theme_bw() +
  ggtitle(label='Cumulative Incidence Plot',
          subtitle='Non-Pamaretric, using `stcompet, cif`') +
  scale_color_manual(values=c('#608341', '#3d3d3d', 'cornflowerblue', 'pink3')) +
  theme_bw()


ggsave(plot=last_plot(), filename='../plots/cif-stcompet-overall.png', dpi=300, height=7, width=10)
