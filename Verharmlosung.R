library(tidyverse)

stringCorona <- "corona|covid.19|sars.cov.2"
stringTrigger <- "harmlos|sinken|gesunken|sicher|unbedenklich|ungefährlich|nicht.ansteckend|gefahrlos|risikolos|nicht.riskant"

corona <-  articles %>% filter(grepl(stringCorona, title, ignore.case=TRUE) | grepl(stringCorona, text, ignore.case = TRUE))

corona <- corona %>%  group_by(source,date) %>% summarise(percentage = mean(grepl(stringTrigger, title, ignore.case=TRUE) | grepl(stringTrigger, text, ignore.case=TRUE))) 

head(corona)

View(corona)


corona <- corona %>% filter(((percentage>0) & (percentage<=0.95)))


ggplot(corona, aes(x=date, y=percentage))+
  geom_point()+
  geom_smooth(se=FALSE)+
  facet_wrap(~ source)+
  labs(title="The appearence of synonyms for the word 'harmless' ")+
  theme(plot.title = element_text(size=11))
