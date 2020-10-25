path_check<-getwd()

library(tidyverse)
# adata <- read_csv("Bucharest.csv")
# bdata <- read_csv("lipscani.csv")
# data<-data.frame(rbind(adata,bdata))
data<- read_csv("data.csv")
data<-data %>% 
  select (Date, Text, Likes)


#1. Identify captions that contain the hashtag or word bucharest
bucharest_vector <- stringr::str_detect(data$Text, "bucharest")
summary(bucharest_vector)
lipscani_vector <- stringr::str_detect(data$Text, "lipscani")
summary(lipscani_vector)


#add this new columns to the data frame
data$bucharest = bucharest_vector
data$lipscani = lipscani_vector
data$tag = ifelse(data$bucharest=='TRUE'&data$lipscani=='TRUE',"Bucharest & lipscani",
                  ifelse(data$bucharest=='TRUE'&data$lipscani=='FALSE',"Bucharest",
                         ifelse(data$bucharest=='FALSE'&data$lipscani=='TRUE',"lipscani",0)))
data<-data %>% 
  mutate(year=lubridate::year(Date), 
         hour=lubridate::hour(Date),
         wday=lubridate::wday(Date, week_start = 1))


#group by date and count the number of instagram post per day
count_data1<- data %>% 
  # filter(lipscani=='TRUE') %>% 
  mutate(Date = as.Date(Date)) %>% 
  count(Data = as.Date(Date)) %>%
  group_by(Data) %>% 
  complete(Data, fill = list(n = 0))

#visualize
library(ggplot2)
graph_ev_cor <- ggplot(count_data1, aes(Data,n))+ 
  geom_line() + 
  theme_light()+
    labs(title='Lipscani tag on Instagram', subtitle = 'perioada 2014-2020')

graph_ev_cor 

#Year View
count_datay<- data %>% 
  group_by(year,tag) %>% 
  summarise(Cnt=n())

gg_datay<-ggplot(count_datay,aes(year, Cnt, group=tag, colour=tag)) +
  labs(title = "Bucharest & Lipscani tags", x = "Year", y = "Frecventa") +
  geom_line(size = 0.7) + 
  theme_light()
gg_datay


#Day of week view
count_dataw<- data %>% 
  group_by(wday,tag) %>% 
  summarise(Cnt=n(), Like=sum(Likes))

gg_dataw<-ggplot(count_dataw,aes(wday, Like, group=tag, colour=tag)) +
  labs(title = "Bucharest & Lipscani tags", x = "Ziua a saptamanii", y = "Frecventa") +
  geom_line(size = 0.7) + 
  theme_light()
gg_dataw


#Hour of day view
count_datah<- data %>% 
  group_by(hour,tag) %>% 
  summarise(Cnt=n(), Like=sum(Likes))

gg_datah<-ggplot(count_datah,aes(hour, Like, group=tag, colour=tag)) +
  labs(title = "Bucharest & Lipscani tags", x = "Ora", y = "Frecventa") +
  geom_line(size = 0.7) + 
  theme_light()
gg_datah


#Hour vs week day view
count_datahw<- data %>% 
#  filter(lipscani='TRUE') %>% 
  group_by(hour, wday) %>% 
  summarise(Cnt=n(), Like=mean(Likes))

gg_datahw<-ggplot(count_datahw,aes(hour, Like, group=wday, colour=wday)) +
  labs(title = "Bucharest & Lipscani tags", x = "Ora", y = "Frecventa") +
  geom_line(size = 0.7) + 
  theme_light()
gg_datahw


# Export current status
time <- Sys.time()
filename <- str_glue("data.csv")
write.csv(data, filename, fileEncoding = "UTF-8")

#Part 2. Next: Tokenization
#split text
# t<-stringr::str_to_lower(data$Text, locale = "ro") %>%
#   #stringr::str_split(" ",simplify=TRUE)
#   stringr::str_split_fixed(" ", 2)

text <- 
  data %>% 
  dplyr::select(Date,Text) %>%
  as_tibble() %>%
  tidytext::unnest_tokens(word,Text)

words <- 
  text %>% 
  count(word,sort=TRUE) 

#viz
# library("RColorBrewer")
# set.seed(1234)
# wordcloud::wordcloud(words = words$word, freq = words$n, min.freq = 1,
#           max.words=200, random.order=FALSE, rot.per=0.35, 
#           colors=brewer.pal(8, "Dark2"))
