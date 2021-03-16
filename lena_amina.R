library(tidyverse)


stringCorona <- "corona|covid.19|sars.cov.2"
stringTrigger <- "tödlich|gefährlich|krise|kritisch|fatal|sterben|mutation"


search <- function(string, object){
  return(grepl(string,object, ignore.case=TRUE))
  
}


#  (search(stringCorona, title)|search(stringCorona, text)) & (search(stringTrigger, title) | search(stringTrigger, text))


corona <- articles %>% group_by(source, date) %>% summarise(percentage= mean( (grepl("corona|covid.19|sars.cov.2", title, ignore.case = TRUE)| grepl("corona|covid.19|sars.cov.2", text, ignore.case = TRUE)) & (grepl("tödlich|gefährlich|krise|kritisch|fatal|sterben|mutation", title, ignore.case = TRUE) | grepl("tödlich|gefährlich|krise|kritisch|fatal|sterben|mutation", text, ignore.case = TRUE))))





corona <-  articles %>% filter(grepl("corona|covid.19|sars.cov.2", title, ignore.case=TRUE) | grepl("corona|covid.19|sars.cov.2", text, ignore.case = TRUE))

corona <- corona %>%  group_by(source,date) %>% summarise(percentage = mean(grepl("tödlich|gefährlich|krise|kritisch|fatal|sterben|mutation", title, ignore.case=TRUE) | grepl("tödlich|gefährlich|krise|kritisch|fatal|sterben|mutation", text, ignore.case=TRUE))) 

head(corona)

View(corona)


corona <- corona %>% filter(((percentage>0) & (percentage<=0.95)))
corona <- corona %>% filter(n()>20)


ggplot(corona, aes(x=date, y=percentage))+
  geom_point()+
  geom_smooth(se=FALSE)+
  facet_wrap(~ source)+
  labs(title="The appearence of certain trigger words in an article")
