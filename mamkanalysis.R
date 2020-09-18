##Accompanying code for the graphics and insights provided in the 
#"Experiences of Racism in MUFSD deck" 

library(tidytext)
library(textstem)
library(magrittr)
library(tidyverse)
library(readxl)

getwd()
setwd("/Users/simonetaylor/Documents/mufsdeda")

###load data ###
shedding.dat <- read_excel("docs/sheddinglight.xlsx") #insta posts from shedding light
demo.dat <- read_excel("docs/demographics.xlsx") #demographic data from nysed
civilrights.dat <- read_excel("docs/civilrightsdat.xlsx") #benchmark data from orcdata

#some of the stories are duplicated, bc they contain multiple stories within a story. Create Id to trace back to one post
ids <- shedding.dat %>% filter(!duplicated(text)) %>% 
  mutate(id = row_number()) %>% select(text,id)
shedding.dat %<>% inner_join(ids)

length(unique(shedding.dat$id))

###graph of demos over the years (slide 2)###
#sanity check
demo.dat %>% 
  group_by(year) %>% 
  summarise(sum(num_total))

civilrights.dat %>% 
  group_by(year,condition) %>% 
  summarise(sum(prop))

#demographics of mhs over the past 20 years 
unique(demo.dat$race)
demo.dat$race %<>%  factor(levels = c("Multiracial", "Black (not hispanic)",
                                      "American Indian, Alaskan, Asian, Pacific Islander","Hispanic","White (not hispanic)"))

demo.dat %>% 
  ggplot() + 
  geom_area(aes(x = year,y = num_total, group = race, fill = race))+ 
  theme_minimal() + 
  labs(title = "BIPOC are a small but growing minority at MUFSD") + 
  ylab(label = "number of students enrolled")


###graph of school demographics relative to markers of success (slide 3)####
#avg demographics
avg.demo.dat <- demo.dat %>% filter(year >= 2009) %>% group_by(race) %>% summarise(totalmean = mean(prop_total))

avg.demo.dat$condition <- "MUFSD breakdown"
avg.demo.dat$race %<>%as.character()

civilrights.dat %<>% filter(!(condition == "school-suspension" & year == 2013))
civilrights.dat %<>% rename(race = demo)
unique(civilrights.dat$race)
civilrights.dat %<>% mutate(race = ifelse(race == "white","White (not hispanic)",
                                         ifelse(race == "black","Black (not hispanic)",
                                                ifelse(race == "hispanic", "Hispanic",
                                                       ifelse(race == "multiracial", "Multiracial", "American Indian, Alaskan, Asian, Pacific Islander")))))


avg.civilrights.dat <- civilrights.dat %>% select(-n_total) %>% 
  group_by(race,condition) %>% 
  summarise(totalmean = mean(prop)) %>% 
  ungroup()

##rename level
breakdowndat <- rbind(avg.demo.dat,avg.civilrights.dat)
breakdowndat %<>% mutate(condition = ifelse(condition == "Algebra", "Middle School Algebra",
                                            ifelse(condition == "SAT","SAT Enrollment",
                                                   ifelse(condition == "MUFSD breakdown", "MUFSD demographics",condition))))
breakdowndat$condition %<>% factor(levels = c("MUFSD demographics","Middle School Algebra","1+ AP","SAT Enrollment","school-suspension"))

breakdowndat %>% 
  ggplot() +
  geom_col(aes(x = condition, y = totalmean, fill = race),position = "stack") + 
  theme_minimal() +
  labs(title = "BIPOC are often underrepresented in markers of sucess at MUFSD", subtitle = "but overrepresented in school suspensions") + 
  ylab(label = "proportion of MUFSD student body") + 
  xlab(label = NULL )


#####so far, over 50 current and previous students have come foward to share their stories of being a minority at MUSFD#
#cleaning text
shedding.dat$text %<>% tolower
shedding.dat$text

shedding.dat$text <- str_replace(shedding.dat$text, "[^A-Za-z0-9]", "")
shedding.dat %<>% mutate(race = ifelse(race == 'asain','asian',race))
shedding.dat %<>% mutate(race = ifelse(str_detect(race,","),"multiracial",race))
shedding.dat %<>% mutate(race = ifelse(race == 'muslim','not specified',race))

shedding.dat$age_range %<>% factor(levels = c("ongoing","elementary school","middle school","high school"))

###counts of stories by education level (slide 6)###
shedding.dat %>% 
  count(age_range) %>% 
  mutate(prop = n/sum(n)) 

#graph of it 
shedding.dat %>% 
  count(age_range) %>% 
  mutate(total = sum(n)) %>% 
  mutate(prop = n/total*100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop) %>% 
  ggplot() + 
  geom_bar(aes(x="", y=n, fill=age_range),stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() +
  labs(title = "MUFSD students recount experiences of racism at all education levels", subtitle =  "and ongoing throughout their time at MUFSD") + 
  xlab(label = "level of education at MUSFD from the story")+
  ylab(label = "number of stories")

###word spider (slide 11 and 12)###
#tokenize and lemmatize words
tokens <- shedding.dat %>%
  group_by(id) %>%
  unnest_tokens(word, text) %>% 
  ungroup()

tokens %<>% 
  anti_join(stop_words)

tokens %<>% mutate(word = case_when(
  word == "ed" ~ "special ed",
  word == "guidance" ~ "guidance counselor",
  word == "untolerable" ~ "untolerable bigotry",
  word == "monkey" ~ "monkey girl",
  word == "natural" ~ "natural hair",
  word == "passive" ~"passive racism",
  word == "emotional" ~ "emotional abuse",
  word == "physical" ~ "physical harassment",
  TRUE ~ as.character(word)
))

tokens$word %<>% lemmatize_words()

#most common words
tokens %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  view()


#plot the axes of the word spider
vecdist <- function(x, y){
  return(rowSums((x-y)**2))
}

angles <- seq(from = 0, to = 360, length.out = 6)
angles <- angles*pi/180
angles <- angles[1:5]
#get the x and y pos for those angles
angx <- cos(angles)
angy <- sin(angles)

points <- data.frame(x=angx, y=angy, angle=angles,race=(unique(tokens$race)))
ggplot(points) + geom_text(data=points, aes(x=x*.5, y=y*.5, 
                                            label=race, 
                                            color=race), size=6, fontface='bold')+
  geom_segment(data=points, aes(x=0,y=0,xend=x, yend=y,  color=race))+
  theme_classic()+
  scale_size_continuous(guide=F,range=c(3,8))+
  labs(x=NULL, y=NULL, title=NULL)+
  coord_cartesian(xlim=c(-.5,.5), ylim=c(-.5,.5))+theme(axis.text = element_blank(),
                                                        axis.ticks = element_blank(), 
                                                        axis.line = element_blank())

###Finding word positions
word.pos <- tokens %>% group_by(race, word) %>%
  count(word)

#prop of finding that word in the cluster
wordvalues <- word.pos %>% group_by(race) %>%
  mutate(sumwtincluster=sum(n)) %>%
  group_by(word, race) %>% summarise(sumwt=sum((n)), 
                                     sumwtincluster=min(sumwtincluster)) %>%
  mutate(p=sumwt/sumwtincluster)

#find x y positions
wordpos <- wordvalues %>% left_join(points, by='race') %>% group_by(word) %>%
  summarise(size=sum(sumwt),
            xword=weighted.mean(x, sumwt/sumwtincluster), 
            yword=weighted.mean(y, sumwt/sumwtincluster)) %>% ungroup() %>%
  mutate(distance=sqrt(xword**2+yword**2)) %>%
  mutate(value=distance*size) 

#include the top winners for each group on value
message('Finding the winning segment for each word')
x <- wordpos[,c('xword','yword')]
y <- points[,c('x','y')]
y <- as.matrix(y)
dists <- apply(y, 1, function(pointrow){
  pointrow <- matrix(rep(pointrow, nrow(x)), nrow=nrow(x), byrow = T)
  ds <- vecdist(x, pointrow)
  return(ds)
})
#
dists <- data.frame(dists)
names(dists) <- paste0('dist',1:ncol(dists))
wordpos <- cbind(wordpos, dists)

#figure out which cluster each word  belongs to
winner<- apply(dists, 1, function(x){which(x==min(x))})
table(winner)
wordpos$winner <- factor(winner, labels = points$race)
wordpos$value <- (wordpos$size)*sqrt(wordpos$xword**2+wordpos$yword**2)

wordpos2 <- wordpos %>% filter(winner == 'asian' & word == 'yellow')
wordpos1 <- wordpos %>% group_by(winner) %>%  top_n(13,value) %>% ungroup()

wordpos <- rbind(wordpos2,wordpos1)

wordpos %>% 
  filter(!word %in%c("special","didn","fun","love","surround","lot","play","call","roller")) %>% 
  ggplot() +
  geom_text(data=points, aes(x=x*.8, y=y*.8, 
                             label=race, 
                             color=race), size=6, fontface='bold')+
  geom_segment(data=points, aes(x=0,y=0,xend=x, yend=y,  color=race))+
  ggrepel::geom_text_repel(aes(xword, yword, size=size, label=word),segment.alpha = 0)+
  theme_classic()+
  scale_size_continuous(guide=F,range=c(4,8))+
  labs(x=NULL, y=NULL, title=NULL)+
  scale_color_discrete(guide=F)+
  coord_cartesian(xlim=c(-.7,.8), ylim=c(-.8,.7))+ theme(axis.text = element_blank(),axis.ticks = element_blank(), axis.line = element_blank())


####Numbers of school officials being involved (slide 13)###
shedding.dat %>% filter(!duplicated(id)) %>% 
  count(school_officials_involved) %>% 
  arrange(desc(n)) %>% 
  mutate(prop = n/sum(n))

#DASA mentions (slide 14)
table(shedding.dat$DASA_mention)

#report mentions (slide 14)
table(shedding.dat$written_doc)

#mediation mentions (slide 14)
table(shedding.dat$mediation)

#types of involvement
shedding.dat %>% filter(!duplicated(id) & school_officials_involved == 'yes') %>% mutate(school_role = ifelse(grepl('overlooked_by_school',complaint_type) & !grepl('school_denied_opportunity', complaint_type),
                                                                                                              "overlooked_by_school",
                                                                                                              ifelse(grepl('school_denied_opportunity',complaint_type) & !grepl('overlooked_by_school', complaint_type),
                                                                                                                     "school_instigated","both"))) %>% 
  count(school_role) %>% 
  arrange(desc(n)) %>% 
  mutate(prop = n/sum(n))

####extra information 
unique(shedding.dat$year_complaint)

#year of complaints
shedding.dat %<>% 
  mutate(year_complaint = ifelse(year_complaint %in% c("2009","2008","2007"), "pre shaps",
                                 ifelse(year_complaint %in% c("ongoing","not specified"), year_complaint, "shaps as superintendent"))) 

shedding.dat %>%
  count(year_complaint) %>% 
  arrange(desc(n)) %>% 
  mutate(prop = n/sum(n))

#alumni year 
shedding.dat %>% 
  count(alum_year) %>% 
  arrange(desc(n))

#alumni status
shedding.dat %>% 
  count(status) %>% 
  arrange(desc(n))




