schwurbel <- articles %>% filter(grepl("corona|covid.19|sars.cov.2", title, ignore.case=TRUE) | grepl("corona|covid.19|sars.cov.2", text, ignore.case = TRUE))
schwurbel <- schwurbel %>% group_by(source,date) %>% summarise(percentage = mean(grepl("verschwörung|querdenken|lügen|maulkorb|ideologie", title, ignore.case=TRUE)|grepl("verschwörung|querdenken|lügen|maulkorb|ideologie", text, ignore.case=TRUE)))


schwurbel <- schwurbel %>% filter(percentage>0 & percentage <=0.95)
head(schwurbel)
schwurbel <- schwurbel %>% filter(n()>30)


ggplot(schwurbel, aes(x=date, y=percentage))+
  geom_point()+
  geom_smooth(se=FALSE)+
  facet_wrap(~ source)+
  labs(title="test")


