lib_loc <- 'E:\\Edie\\R\\Libraries'
assign('.lib.loc', lib_loc, envir=environment(.libPaths))


library(haven, lib.loc=lib_loc)
library(stringr, lib.loc=lib_loc)
library(reshape2, lib.loc=lib_loc)
library(tidyr, lib.loc=lib_loc)
library(dplyr, lib.loc=lib_loc)
library(ggplot2, lib.loc=lib_loc)
library(readr, lib.loc=lib_loc)


this_log  <- '../tables/catMarginsAge.log'

a_stata_log    <- scan(this_log, sep='\n', what='character')
breaks_in_text <- which(grepl('------', a_stata_log))
headers     <- breaks_in_text[3]-1
table_start <- breaks_in_text[3]+2
table_end   <- breaks_in_text[4]-1

margins_table <- a_stata_log[table_start:table_end]
margins_table <- trimws(margins_table)
margins_table <- lapply(strsplit(margins_table, '\\|'), trimws)

age_year   <- sapply(margins_table, function(k) strsplit(k[[1]], ' '))
age_year_2 <- lapply(age_year, function(k) as.numeric(k))

values   <- sapply(margins_table, function(k) strsplit(k[[2]], '( )+'))
values_2 <- lapply(values, function(k) round(as.numeric(k),4))

age_year_3 <- do.call(rbind, age_year_2)
values_3   <- do.call(rbind, values_2)

table_1 <- cbind(age_year_3, values_3) %>% data.frame()
names(table_1) <- c('age', 'year', 'margin', 'se', 't', 'pvalue', 'lb', 'ub')


table_2 <- table_1 %>%
  rename(Age=age) %>%
  mutate(Age=ifelse(Age==1, '55-64',
                    ifelse(Age==2, '65-74',
                           ifelse(Age==3, '75-84',
                                  ifelse(Age==4, '85+', NA)))))


makeItPretty <- list(theme_classic(),
                     xlab('Year'),
                     ylab('Smoking Prevalence (%)'),
                     scale_x_continuous(breaks=seq(1998, 2018, 2)),
                     theme(axis.text.x=element_text(size=15),
                           axis.text.y=element_text(size=15),
                           legend.text=element_text(size=15),
                           axis.title.x=element_text(face='bold', size=15),
                           axis.title.y=element_text(face='bold', size=15),
                           legend.title=element_text(face='bold', size=15),
                           title=element_text(face='bold', size=18)),
                     scale_color_manual(values=c('#00688B', "#72587F", '#759B84', '#8B8878', 'black')))

table_2 %>%
  rename(`Age Group`=Age) %>%
  mutate(margin=margin*100) %>%
  ggplot(aes(x=year, y=margin, col=`Age Group`, group=`Age Group`)) +
  geom_point(size=2) +
  geom_line(alpha=0.5, lwd=2) +
  ggtitle('') +
  makeItPretty

ggsave(plot=last_plot(), filename='../plots/smoking-prevalence.png', dpi=300, height=7, width=10)


table_2 %>%
  rename(`Age Group`=Age) %>%
  mutate(margin=margin*100) %>%
  mutate(lb=lb*100) %>%
  mutate(ub=ub*100) %>%
  ggplot(aes(col=`Age Group`, group=`Age Group`)) +
  geom_segment(aes(x=year, xend=year, y=lb, yend=ub), lwd=1, alpha=0.5) +
  geom_point(size=2, aes(x=year, y=margin)) +
  geom_line(lwd=2, aes(x=year, y=margin), alpha=0.5) +
  ggtitle('') +
  makeItPretty

ggsave(plot=last_plot(), filename='../plots/smoking-prevalence-2.png', dpi=300, height=7, width=10)








# All ages
# 15.9 (15.2, 16.7)
# 15 (14, 16)
# 14.3 (13.3, 15.4)
# 14.7 (13.7, 15.6)
# 14.1 (13.1, 15.1)
# 13.6 (12.8, 14.5)
# 13.3 (12.5, 14.1)
# 13.2 (12.5, 14)
# 12.3 (11.6, 13)
# 12.3 (11.5, 13)
# 11.2 (10.4, 12.1)




overall <- do.call(rbind,list(
                      c(Age='Overall', year=1998, margin=0.159, lb=0.152, ub=0.167),
                      c(Age='Overall', year=2000, margin=0.15, lb=0.14, ub=0.16),
                      c(Age='Overall', year=2002, margin=0.143, lb=0.133, ub=0.154),
                      c(Age='Overall', year=2004, margin=0.147, lb=0.137, ub=0.156),
                      c(Age='Overall', year=2006, margin=0.141, lb=0.131, ub=0.151),
                      c(Age='Overall', year=2008, margin=0.136, lb=0.128, ub=0.145),
                      c(Age='Overall', year=2010, margin=0.133, lb=0.125, ub=0.141),
                      c(Age='Overall', year=2012, margin=0.132, lb=0.125, ub=0.14),
                      c(Age='Overall', year=2014, margin=0.123, lb=0.116, ub=0.13),
                      c(Age='Overall', year=2016, margin=0.123, lb=0.115, ub=0.13),
                      c(Age='Overall', year=2018, margin=0.112, lb=0.104, ub=0.121))) %>%
  data.frame() %>%
  mutate(year=as.numeric(year)) %>%
  mutate(margin=as.numeric(margin)) %>%
  mutate(lb=as.numeric(lb)) %>%
  mutate(ub=as.numeric(ub))
  

table_3 <- bind_rows(table_2, overall)




table_3 %>%
  rename(`Age Group`=Age) %>%
  mutate(margin=margin*100) %>%
  mutate(lb=lb*100) %>%
  mutate(ub=ub*100) %>%
  ggplot(aes(col=`Age Group`, group=`Age Group`)) +
  geom_segment(aes(x=year, xend=year, y=lb, yend=ub), lwd=1, alpha=0.5) +
  geom_point(size=2, aes(x=year, y=margin)) +
  geom_line(lwd=2, aes(x=year, y=margin), alpha=0.5) +
  ggtitle('') +
  makeItPretty

ggsave(plot=last_plot(), filename='../plots/smoking-prevalence-3.png', dpi=300, height=7, width=10)






save_yearly <- table_2 %>%
  rename(`Age Group`=Age) %>%
  mutate(margin=margin*100) %>%
  mutate(se=se*100) %>%
  mutate(lb=lb*100) %>%
  mutate(ub=ub*100) %>%
  mutate(Prevalence=paste0(margin, ' (', lb, ', ', ub, ')')) %>%
  rename(Year=year) %>%
  reshape2::dcast(Year ~ `Age Group`, value.var='Prevalence')

write_csv(save_yearly, '../tables/prevalence-estimates-age.csv')

