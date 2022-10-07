lib_loc <- 'E:\\Edie\\R\\Libraries'
assign('.lib.loc', lib_loc, envir=environment(.libPaths))

library(readxl, lib.loc=lib_loc)
library(haven, lib.loc=lib_loc)
library(stringr, lib.loc=lib_loc)
library(reshape2, lib.loc=lib_loc)
library(tidyr, lib.loc=lib_loc)
library(dplyr, lib.loc=lib_loc)
library(ggplot2, lib.loc=lib_loc)
library(readr, lib.loc=lib_loc)

# Unadjusted / Bivariate
surv_est_b_v0 <- read_excel('../tables/survival-estimates.xlsx', sheet='unadjusted')

surv_est_b_v1 <- surv_est_b_v0 %>%
  mutate(Coefficient=round(Coefficient, 2)) %>%
  mutate(`95% lower bound`=round(`95% lower bound`, 2)) %>%
  mutate(`95% upper bound`=round(`95% upper bound`, 2)) %>%
  mutate(Bivariate=ifelse(!is.na(`95% lower bound`),
                          paste0(Coefficient, ' (', `95% lower bound`, ', ', `95% upper bound`, ')'),
                          Coefficient))  %>%
  
  mutate(`p-value`=ifelse(is.na(`p-value`), 1, `p-value`)) %>%
  mutate(Bivariate=ifelse(`p-value`<0.001, paste0(Bivariate, '***'),
                     ifelse(`p-value`<0.01, paste0(Bivariate, '**'),
                            ifelse(`p-value`<0.05, paste0(Bivariate, '*'), Bivariate))))
names(surv_est_b_v1)[1:2] <- c('Factor', 'Level')
surv_est_b_v1 <- surv_est_b_v1 %>% select(Factor, Level, Bivariate)


# Saturated
surv_est_s_v0 <- read_excel('../tables/survival-estimates.xlsx', sheet='saturated')

surv_est_s_v1 <- surv_est_s_v0 %>%
  mutate(Coefficient=round(Coefficient, 2)) %>%
  mutate(`95% lower bound`=round(`95% lower bound`, 2)) %>%
  mutate(`95% upper bound`=round(`95% upper bound`, 2)) %>%
  mutate(Saturated=ifelse(!is.na(`95% lower bound`),
                          paste0(Coefficient, ' (', `95% lower bound`, ', ', `95% upper bound`, ')'),
                          Coefficient))  %>%
  
  mutate(`p-value`=ifelse(is.na(`p-value`), 1, `p-value`)) %>%
  mutate(Saturated=ifelse(`p-value`<0.001, paste0(Saturated, '***'),
                     ifelse(`p-value`<0.01, paste0(Saturated, '**'),
                            ifelse(`p-value`<0.05, paste0(Saturated, '*'), Saturated))))
names(surv_est_s_v1)[1:2] <- c('Factor', 'Level')
surv_est_s_v1 <- surv_est_s_v1 %>% select(Factor, Level, Saturated)



# MICE
surv_est_m_v0 <- read_excel('../tables/survival-estimates.xlsx', sheet='mice')

surv_est_m_v1 <- surv_est_m_v0 %>%
  mutate(Coefficient=round(Coefficient, 2)) %>%
  mutate(`95% lower bound`=round(`95% lower bound`, 2)) %>%
  mutate(`95% upper bound`=round(`95% upper bound`, 2)) %>%
  mutate(MICE=ifelse(!is.na(`95% lower bound`),
                          paste0(Coefficient, ' (', `95% lower bound`, ', ', `95% upper bound`, ')'),
                          Coefficient)) %>%
  mutate(`p-value`=ifelse(is.na(`p-value`), 1, `p-value`)) %>%
  mutate(MICE=ifelse(`p-value`<0.001, paste0(MICE, '***'),
                     ifelse(`p-value`<0.01, paste0(MICE, '**'),
                            ifelse(`p-value`<0.05, paste0(MICE, '*'), MICE))))
names(surv_est_m_v1)[1:2] <- c('Factor', 'Level')
surv_est_m_v1 <- surv_est_m_v1 %>% select(Factor, Level, MICE)


# Full table
full_table <- left_join(surv_est_b_v1, surv_est_s_v1) %>%
  left_join(surv_est_m_v1)

write_csv(full_table, '../tables/survival-estimates-combined.csv')
