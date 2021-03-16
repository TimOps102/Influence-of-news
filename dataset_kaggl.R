library(tidyverse)
articles <- rename(articles, title = X1,
                   text = X2,
                   author = X3,
                   published = X4,
                   tags = X5,
                   source = X6)


sources <- articles %>% group_by(source) %>% summarise(
  word_count = sum(grepl("Rechtsextremismus", title) +sum(grepl("Rechtsextremismus", text)))
) %>% 
  arrange(desc(word_count))

view(sources)




corona <- articles %>% mutate(date =as.POSIXct(as.numeric(as.character(published)), origin="1970-01-01", tz="GMT"))
head(corona)

corona <- corona %>% mutate(year = format(date, "%Y"), month=format(date,"%m"), day = format(date,"%d"))
corona <- corona %>% filter(year >=2020)
View(corona)


corona_research <- corona %>%  group_by(month) %>% summarise(percentage=mean(grepl(".[Cc]orona", title )))


test <- "Willkommen zur Corona Schutzimpgun"
grepl(".[Cc]orona", test)


ggplot(corona_research, aes(x=month, y=percentage))+
  geom_bar( stat="identity")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

bild_research <- corona %>% filter(source== "bild", as.numeric(month)>=9)
View(bild_research)

bild_research <- bild_research %>%  group_by(month,day) %>% summarise(percentage = mean(grepl(".[Cc]orona.", title))) 
bild_research <- mutate(bild_research, date = as.Date(paste(day,month), format="%d %m"))
bild_research <- filter(bild_research, percentage<0.4)

ggplot(bild_research, aes(x=date, y=percentage))+
  geom_point()+
  geom_smooth()


articles_per_month <- corona %>% group_by(month) %>% summarise( n=n())


corona <- corona %>% group_by(source) %>% filter(n() >=1000)

ggplot(corona, aes(month))+
  geom_bar( aes(fill = source) )

head(corona)
View(articles_per_month)
