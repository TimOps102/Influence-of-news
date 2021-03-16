library(tidyverse)

stringCorona <- "corona|covid.19|sars.cov.2"
stringTrigger <- "harmlos|sinken|gesunken|sicher|unbedenklich|ungefährlich|nicht.ansteckend|gefahrlos|risikolos|nicht.riskant"
stringTrigger <- "ansteckend|gefährlich|gefahr|kritisch|infiziert|muta|tödlich|tod|ansteigen|anstieg|risiko|riskant|bedenklich|schwer|krise"

corona <-  articles %>% filter(grepl(stringCorona, title, ignore.case=TRUE) | grepl(stringCorona, text, ignore.case = TRUE))

corona <- corona %>%  group_by(source,date) %>% summarise(percentage = mean(grepl(stringTrigger, title, ignore.case=TRUE) | grepl(stringTrigger, text, ignore.case=TRUE))) 

head(corona)

View(corona)


corona <- corona %>% filter(((percentage>0) & (percentage<=0.95)))


ggplot(corona, aes(x=date, y=percentage))+
  geom_point()+
  geom_smooth(se=FALSE)+
  labs(title="Appearance of uncomfortable words in context of news coverage")+
  theme(plot.title = element_text(size=11))+scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies")

