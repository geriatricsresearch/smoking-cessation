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
  mutate(Age=ifelse(Age==1, '<65',
                    ifelse(Age==2, '65-74',
                           ifelse(Age==3, '75-84',
                                  ifelse(Age==4, '85+', NA)))))


makeItPretty <- list(theme_classic(),
                     xlab('Year'),
                     ylab('Smoking Prevalence (%)'),
                     scale_x_continuous(breaks=seq(1998, 2018, 2)),
                     theme(axis.text.x=element_text(face='bold', size=6)),
                     scale_color_manual(values=c('#00688B', "#72587F", '#759B84', '#8B8878')))

table_2 %>%
  rename(`Age Group`=Age) %>%
  mutate(margin=margin*100) %>%
  ggplot(aes(x=year, y=margin, col=`Age Group`, group=`Age Group`)) +
  geom_point() +
  geom_line(alpha=0.5) +
  ggtitle('Smoking Prevalence') +
  makeItPretty

ggsave(plot=last_plot(), filename='../plots/smoking-prevalence.png', dpi=300, height=7, width=10)


table_2 %>%
  rename(`Age Group`=Age) %>%
  mutate(margin=margin*100) %>%
  mutate(lb=lb*100) %>%
  mutate(ub=ub*100) %>%
  ggplot(aes(col=`Age Group`, group=`Age Group`)) +
  geom_segment(aes(x=year, xend=year, y=lb, yend=ub), lwd=1.5, alpha=0.5) +
  geom_point(aes(x=year, y=margin)) +
  geom_line(aes(x=year, y=margin), alpha=0.5) +
  ggtitle('Smoking Prevalence') +
  makeItPretty

ggsave(plot=last_plot(), filename='../plots/smoking-prevalence-2.png', dpi=300, height=7, width=10)


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
