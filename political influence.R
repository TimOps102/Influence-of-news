library(tidyverse)
articles <- data %>% mutate(date =as.POSIXct(as.numeric(as.character(published)), origin="1970-01-01", tz="GMT"))



articles <- articles %>% mutate(year = format(date, "%Y"), month=format(date,"%m"), day = format(date,"%d"))
articles <- articles %>% filter(year >=2020 & year<2021)
articles <- articles %>% group_by(source) %>% filter(n()>1000) %>% ungroup()
articles <- articles %>% filter(as.numeric(month) >= 7 & month< 12)
articles <- mutate(articles, date = as.Date(paste(year,month, day), "%Y %m %d"))
articles <- articles %>% group_by(source,date) %>% filter(n()>10) %>% ungroup()
articles <- articles %>% group_by(source) %>% filter(n()>2500) %>% ungroup()

View(artic)

head(articles)

articles %>% filter(year>2020)



write.csv(articles,'~/Desktop/Studium Uni/3.Fachsemester/EInführung in die DH/DH projekt/articles.csv' )

ggplot(articles, aes(month))+
  geom_bar( aes(fill = source) )+
  labs(title= "Newspaper articles per month in 2020")




ggplot(querdenken, aes(month))+
  geom_bar(aes(fill=source))


querdenken <- articles 
querdenken <-  articles %>% group_by(source,date) %>% summarise(percentage = mean(grepl("corona|covid.19|sars.cov.2", title, ignore.case=TRUE) | grepl("corona|covid.19|sars.cov.2", text, ignore.case = TRUE)))
querdenken <- querdenken %>%  group_by(source) %>% filter(n()>150)

ggplot(querdenken, aes(x=source))+
  geom_bar()


querdenken <- querdenken %>%  group_by(source,date) %>% summarise(percentage = mean(grepl("schaden|schäden|folgen|risiko|risiken|nebenwirkungen", title, ignore.case = TRUE)|grepl("schaden|schäden|folgen|risiko|risiken|nebenwirkungen", text, ignore.case = TRUE))) 
querdenken <- querdenken %>% filter((percentage>0) & (percentage<=0.95))
querdenken <- querdenken %>% filter(n()>50)

head(querdenken)


ggplot(querdenken, aes(x=date, y=percentage))+
  geom_point()+
  geom_smooth(se=FALSE)+
  facet_wrap(~ source)+
  labs(title="The appearence of titles which inlude the phrase 'impf' or 'Impf'")




