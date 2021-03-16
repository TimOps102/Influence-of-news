library(tidyverse)
stringCorona <- "corona|covid.19|sars.cov.2"
stringRules <- "regeln|beschränkungen|beschränkung|restriktion|einschränkung|richtlinien|verordnung|kontaktbeschränkungen|maßnahmen"

corona <-  articles %>% filter(grepl(stringCorona, title, ignore.case=TRUE) | grepl(stringCorona, text, ignore.case = TRUE) |grepl(stringCorona, tags, ignore.case = TRUE))
corona <- corona %>%  group_by(source,date) %>% summarise(percentage = mean(grepl(stringRules, title, ignore.case=TRUE) | grepl(stringRules, text, ignore.case=TRUE)))

corona <- corona %>% filter(percentage>0 & percentage <1)

ggplot(corona,aes(x=date,y=percentage))+
  geom_point()+
  geom_smooth(se=FALSE)+
  facet_wrap(~source)+
  labs(title="Appearence of words and phrases which are about Restrictions")+
  theme(plot.title = element_text(size=11))
