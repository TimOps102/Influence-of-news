library(tidyverse)
stringCorona <- "corona|covid.19|sars.cov.2"
stringQuerdenken <- "qanon|querdenken|ideologie|lügenpresse|afd|maulkorb|rechte.szene|rechtsextrem|verschwörung|leugnen|leugner"


corona <-  articles %>% filter(grepl(stringCorona, title, ignore.case=TRUE) | grepl(stringCorona, text, ignore.case = TRUE) |grepl(stringCorona, tags, ignore.case = TRUE))
corona <- corona %>%  group_by(source,date) %>% summarise(percentage = mean(grepl(stringQuerdenken, title, ignore.case=TRUE) | grepl(stringQuerdenken, text, ignore.case=TRUE))) 

corona <- corona %>% filter(percentage >0 & percentage <1)


ggplot(corona,aes(x=date,y=percentage))+
  geom_point()+
  geom_smooth(se=FALSE)+
  facet_wrap(~source)+
  labs(title="Appearence of words in context of denying the Corona pandemic ")+
  theme(plot.title = element_text(size=11))
