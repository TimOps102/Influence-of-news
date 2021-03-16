library(tidyverse)
stringCorona <- "corona|covid.19|sars.cov.2"


corona <-  articles %>% group_by(source,date) %>% summarise(percentage= mean(grepl(stringCorona, title, ignore.case=TRUE) | grepl(stringCorona, text, ignore.case = TRUE) |grepl(stringCorona, tags, ignore.case = TRUE)))

head(articles)


ggplot(corona,aes(x=date,y=percentage))+
  geom_point()+
  geom_smooth(se=FALSE)+
  facet_wrap(~source)+
  labs(title="Appearence of words in context of 'Corona' ") +
  theme(plot.title=element_text(size=11))
  
