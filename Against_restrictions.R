library(tidyverse)
stringCorona <- "corona|covid.19|sars.cov.2"
stringRules <- "regeln|beschränkungen|beschränkung|restriktion|einschränkung|richtlinien|verordnung|kontaktbeschränkungen|maßnahmen"
stringFollow <- "verstoß|nicht.eingehalten|party|unzufrieden|strafe|bußgeld"


corona <-  articles %>% filter(grepl(stringCorona, title, ignore.case=TRUE) | grepl(stringCorona, text, ignore.case = TRUE) |grepl(stringCorona, tags, ignore.case = TRUE))

corona <- corona %>%  filter(grepl(stringRules, title, ignore.case=TRUE) | grepl(stringRules, text, ignore.case=TRUE))

corona <- corona %>% group_by(date) %>% summarise(percentage=mean(grepl(stringFollow, title, ignore.case=TRUE) | grepl(stringFollow, text, ignore.case=TRUE)))

corona <- corona %>% filter(percentage>0 & percentage <1)


ggplot(corona,aes(x=date,y=percentage))+
  geom_point()+
  geom_smooth(se=FALSE)+
  labs(title="Appearance of words and phrases which are against Corona restrictions ")+
  theme(plot.title = element_text(size=11))+
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies")
