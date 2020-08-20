##sheddinglight word cloud##
library(stringr)
library(ggplot2)
library(dplyr)
library(tidytext)
library(textstem)
#install.packages("tm")
library(tm)

#install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)
#install.packages("wordcloud2")
library(wordcloud2)
library(readxl)
library(magrittr)
library(tidyverse)
#install.packages("textdata")
library(textdata)
#install.packages("tsne")
library(tsne)
#install.packages('treemap')
library(treemap)


getwd()
setwd("/Users/simone/Desktop/mamk")

##load data 
shedding.dat <- read_excel("sheddinglight.xlsx")
demo.dat <- read_excel("demographics.xlsx")
civilrights.dat <- read_excel("civilrightsdat.xlsx")


##graph of demos over the years##
##shaps started in 2010
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

unique(civilrights.dat$condition)
civilrights.dat %>%
  filter(condition != "school-suspension") %>%
  group_by(race,year,condition) %>% 
  summarise(n = n_total * prop) %>% 
  filter(condition == "SAT") %>% 
  ggplot() + 
  geom_area(aes(x = year,y = n, group = race, fill = race))+ 
  theme_minimal() 
  labs(title = "BIPOC are a small but growing minority at MUFSD") + 
  ylab(label = "number of students enrolled")


avg.demo.dat <- demo.dat %>% filter(year >= 2009) %>% group_by(race) %>% summarise(totalmean = mean(prop_total))
unique(avg.demo.dat$race)

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
  

##change factor levels

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
  
#####so far, 50 current and previous students have come foward to share their stories of being a minority at MUSFD#
shedding.dat$text %<>% tolower
shedding.dat$text

shedding.dat$text <- str_replace(shedding.dat$text, "[^A-Za-z0-9]", "")
shedding.dat %<>% rowid_to_column("id")
shedding.dat %<>% mutate(race = ifelse(race == 'asain','asian',race))

shedding.dat$age_range %<>% factor(levels = c("ongoing","elementary school","middle school","high school"))
shedding.dat %>% 
  count(age_range) %>% 
  mutate(prop = n/sum(n))
  ggplot() + 
  theme_minimal() +
  geom_col(aes(x = age_range, y = n, fill = age_range))+ 
  labs(title = "MUFSD students experience racism at every level of education", subtitle =  "and most frequently throughout their whole time in the MUFSD system") + 
  xlab(label = "level of education in MUSFD from the story")+
  ylab(label = "number of stories")


treemap.shedding <- shedding.dat %>% 
  count(age_range) %>% 
  mutate(prop = n/sum(n))


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


##was trying to turn this into an area chart but it was a fail
#test <- shedding.dat %>% mutate(length_experience = 13)
#test %<>% 
  #group_by(age_range,length_experience) %>% 
  #summarise(num_stories = length(unique(id)))

#test %<>% uncount(length_experience)
#test %<>% group_by(age_range) %>% mutate(years = rep(1:13)) %>% ungroup()

#test.ongoing<- test %>% filter(age_range == "ongoing") 
#test.elementary <- test %>% filter(age_range == "elementary school")  %>% mutate(num_stories=ifelse(years %in% 1:6,num_stories,0))
#test.middle <- test %>% filter(age_range == "middle school") %>% mutate(num_stories=ifelse(years %in% 7:9,num_stories,0)) 
#test.high <- test %>% filter(age_range == "high school") %>%mutate(num_stories=ifelse(years %in% 10:13,num_stories,0))

#test <- rbind(test.ongoing,test.elementary,test.middle,test.high)
#test %>% 
  #ggplot() + 
  #geom_area(aes(x = years, y = num_stories, fill = age_range))

shedding.dat %<>% mutate(race = ifelse(str_detect(race,","),"multiracial",race))
shedding.dat %<>% mutate(race = ifelse(race == 'muslim','not specified',race))


demo.race <- demo.dat %>% 
  group_by(race) %>% 
  summarise(mean_num = mean(num_total)) %>% 
  mutate(prop = mean_num/sum(mean_num))

demo.race$race <- c("multiracial","black","asian","latinx","white")
demo.race %<>% rename(n = mean_num)
demo.race$group = "mufsd demographics"

shedding.race <- shedding.dat %>% 
  count(race) %>% 
  arrange(desc(n)) %>% 
  mutate(prop = n/sum(n)) 

shedding.race$group <- "shedding light demographics"

race <- rbind(demo.race,shedding.race)
race %>% 
  ggplot() +
  geom_col(aes(x = group, y = prop, fill = race),position = "stack") + 
  theme_minimal() + 
  labs(title = "Black, Latinx, and Multiracial students disproportionally experience racism", subtitle = "relative to their presence at MUFSD") + 
  ylab(label = "proportion of MUFSD student body")

treemap(shedding.race,
        index="race",
        vSize="n",
        type="index")


unique(shedding.dat$year_complaint)
shedding.dat %<>% 
  mutate(year_complaint = ifelse(year_complaint %in% c("2009","2008","2007"), "pre shaps",
                                 ifelse(year_complaint %in% c("ongoing","not specified"), year_complaint, "shaps as superintendent"))) 

unique(shedding.dat$year_complaint)
shedding.dat %>%
  count(year_complaint) %>% 
  arrange(desc(n)) %>% 
  mutate(prop = n/sum(n))
  ggplot()+ 
  theme_minimal()+
  geom_col(aes(x = reorder(year_complaint,n), y = n, fill = year_complaint), position = "identity") + 
  labs(title = "many of these stories occured while shaps was superintendent ") + 
  ylab(label = 'number of stories')+
  xlab(label = "time frame when the story occured")
  

shedding.dat %>% 
  count(alum_year) %>% 
  arrange(desc(n))

shedding.dat %>% 
  count(status) %>% 
  arrange(desc(n))

shedding.dat %>% 
  count(age_range) %>% 
  arrange(desc(n))

summary(shedding.dat$likes)

tokens <- shedding.dat %>%
  group_by(id) %>%
  unnest_tokens(word, text) %>% 
  ungroup()

tokens %<>% 
  anti_join(stop_words)

tokens$word %<>% lemmatize_words()


tokens %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  view()


#wordcloud2(tokens, size = 1.2,color = 'random-light')
#wordcloud(tokens$word,tokens$n,scale = c(4,.5), colors=brewer.pal(5, "Set2"), random.order =  FALSE)

##bigrams 
shedding.complaint %>% 
  unnest_tokens(word, text, token = "ngrams", n = 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(word,word1, word2, sep = " ") %>% 
  count(word, complaint_type) %>% 
  arrange(desc(n)) %>% view()


#####prominence and dominance? 
#create dominance and prominance measures

##trying to wordspider 
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


message('Estimating total word value')



###Finding word positions
word.pos <- tokens %>% group_by(race, word) %>%
  count(word)

wordvalues <- word.pos %>% group_by(race) %>%
  mutate(sumwtincluster=sum(n)) %>%
  group_by(word, race) %>% summarise(sumwt=sum((n)), 
                                        sumwtincluster=min(sumwtincluster)) %>%
  mutate(p=sumwt/sumwtincluster)

wordpos <- wordvalues %>% left_join(points, by='race') %>% group_by(word) %>%
  summarise(size=sum(sumwt),
            xword=weighted.mean(x, sumwt/sumwtincluster), 
            yword=weighted.mean(y, sumwt/sumwtincluster)) %>% ungroup() %>%
  mutate(distance=sqrt(xword**2+yword**2)) %>%
  mutate(value=distance*size) 

#so I'm doing a thing where we include the top winners for each group on value
#that helps me filter for visualization
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

winner<- apply(wordpos[,7:11], 1, function(x){which(x==min(x))})
table(winner)
wordpos$winner <- factor(winner, labels = points$race)
wordpos$value <- (wordpos$size)*sqrt(wordpos$xword**2+wordpos$yword**2)

wordpos2 <- wordpos %>% filter(winner == 'asian' & word == 'yellow')
wordpos1 <- wordpos %>% group_by(winner) %>%  top_n(13,value) %>% ungroup()

wordpos <- rbind(wordpos2,wordpos1)

wordpos %>% 
  filter(!word %in%c("didn","fun","love","surround","lot","play","call","roller")) %>% 
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

table(shedding.dat$complaint_type)

shedding.complaint <- shedding.dat %>% mutate(complaint_type = strsplit(complaint_type, ",")) %>% 
  unnest(complaint_type)

shedding.complaint$complaint_type %<>% str_remove(" ")
table(shedding.complaint$complaint_type)
shedding.complaint$complaint_type %<>% factor(levels = c("verbal","internalized","physical","school_denied_opportunity",'overlooked_by_school'))

tokens.comp.bi <- shedding.complaint %>% 
  unnest_tokens(word, text, token = "ngrams", n = 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(word,word1, word2, sep = " ") %>% 
  count(word, complaint_type) %>% 
  arrange(desc(n)) %>% ungroup()

tokens.comp <- shedding.complaint %>%
  group_by(id) %>%
  unnest_tokens(word, text) %>% 
  ungroup()

tokens.comp %<>% 
  anti_join(stop_words)

##create some bigrams
tokens.comp %<>% mutate(word = case_when(
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


tokens.comp$word %<>% lemmatize_words()

tokens.comp %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  view()

tokens.comp %<>% filter(!is.na(complaint_type))

####worspider for types of harrassment
vecdist <- function(x, y){
  return(rowSums((x-y)**2))
}

angles <- seq(from = 0, to = 360, length.out = 6)
angles <- angles*pi/180
angles <- angles[1:5]
#get the x and y pos for those angles
angx <- cos(angles)
angy <- sin(angles)

points <- data.frame(x=angx, y=angy, angle=angles,complaint_type=(unique(tokens.comp$complaint_type)))
ggplot(points) + geom_text(data=points, aes(x=x*.5, y=y*.5, 
                                            label=complaint_type, 
                                            color=complaint_type), size=6, fontface='bold')+
  geom_segment(data=points, aes(x=0,y=0,xend=x, yend=y,  color=complaint_type))+
  theme_classic()+
  scale_size_continuous(guide=F,range=c(3,8))+
  labs(x=NULL, y=NULL, title=NULL)+
  coord_cartesian(xlim=c(-.5,.5), ylim=c(-.5,.5))+theme(axis.text = element_blank(),
                                                        axis.ticks = element_blank(), 
                                                        axis.line = element_blank())


message('Estimating total word value')


###Finding word positions
word.pos <- tokens.comp %>% group_by(complaint_type, word) %>%
  count(word)

wordvalues <- word.pos %>% group_by(complaint_type) %>%
  mutate(sumwtincluster=sum(n)) %>%
  group_by(word, complaint_type) %>% summarise(sumwt=sum((n)), 
                                     sumwtincluster=min(sumwtincluster)) %>%
  mutate(p=sumwt/sumwtincluster)

wordpos <- wordvalues %>% left_join(points, by='complaint_type') %>% group_by(word) %>%
  summarise(size=sum(sumwt),
            xword=weighted.mean(x, sumwt/sumwtincluster), 
            yword=weighted.mean(y, sumwt/sumwtincluster)) %>% ungroup() %>%
  mutate(distance=sqrt(xword**2+yword**2)) %>%
  mutate(value=distance*size) 

#so I'm doing a thing where we include the top winners for each group on value
#that helps me filter for visualization
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

winner<- apply(wordpos[,7:11], 1, function(x){which(x==min(x))})
table(winner)
wordpos$winner <- factor(winner, labels = points$complaint_type)
wordpos$value <- (wordpos$size)*sqrt(wordpos$xword**2+wordpos$yword**2)

wordpos %<>% group_by(winner) %>%  top_n(21,value) 

points %<>% mutate(complaint_type = case_when(
  as.character(complaint_type) == "overlooked_by_school" ~ "overlooked by school",
  as.character(complaint_type) == "school_denied_opportunity" ~ "school denied opportunity",
  TRUE ~ as.character(complaint_type)
))

wordpos %>% 
  filter(!word %in% c("special","counselor","roller","fun", "dobetterbebetter","bebetter","population","reason","2017")) %>% 
  ggplot() +
  geom_text(data=points, aes(x=x*.75, y=y*.75, 
                             label=complaint_type, 
                             color=complaint_type), size=6, fontface='bold')+
  geom_segment(data=points, aes(x=0,y=0,xend=x, yend=y,  color=complaint_type))+
  ggrepel::geom_text_repel(aes(xword, yword, size=size, label=word),segment.alpha = 0)+
  theme_classic()+
  scale_size_continuous(guide=F,range=c(4,8))+
  labs(x=NULL, y=NULL, title=NULL)+
  scale_color_discrete(guide=F)+
  coord_cartesian(xlim=c(-.9,.9), ylim=c(-.9,.9))+ theme(axis.text = element_blank(),axis.ticks = element_blank(), axis.line = element_blank())












cluster.words <- tokens%>%  group_by(race, word) %>%
  summarise(n=length(unique(id))) %>% group_by(race) %>%
  mutate(pctappear=n/sum(n), #percent word appears in a cluster
         totalclusterwords=sum(n), 
         dominance=log(n/sum(n))) %>%  ##how much a word "dominates" a cluster
  group_by(word) %>%
  mutate(prominence=log(pctappear)-log(weighted.mean(pctappear, totalclusterwords)), #log ratio of likihood word is in one cluster versus others 
         pctword=n/sum(n), 
         totalword=sum(n)) %>%  
  subset(prominence<Inf)%>% group_by(race) %>%
  mutate(prom.sd = scale(prominence, center = T, scale = F))

summary(cluster.words)
cluster.words %>%
  group_by(race) %>% 
  mutate(promtest = quantile(prom.sd, probs = .91), 
         ntest =quantile(dominance, probs = .86)) %>% 
  filter(n >= ntest & prominence >= promtest) %>%
  filter(!grepl("[0-9]",word)) %>%
  group_by(race) %>% 
  arrange(desc(n)) %>%  
  top_n(15,prom.sd*totalword) %>% 
  ggplot(aes(x=dominance, y=prom.sd, color=factor(race), label=word))+
  ggrepel::geom_text_repel() +
  facet_wrap(~race) +
  theme_void()



###EXTRA SHIT I TRIED::
tokens.racism <- tokens %>% filter(word %in% c("feel","feeling","black","hispanic","skin", "racism","bully"))
tokenstext.racism <- tokens.racism %>% left_join(shedding.dat)
tokenstext.racism %<>% filter(!duplicated(id,word))
tokenstext.racism %>% filter(word == "racism") %>% select(text) %>% view()


sentences <- shedding.dat %>%
  group_by(id) %>%
  unnest_tokens(text, sentences, token = "sentences") %>% 
  ungroup()

sentencestxt <- tokens %>% inner_join(sentences)

tokenstext <- tokens %>% left_join(shedding.dat)
tokenstext %>% filter(word == 'feel' | word == "feeling") %>% filter(!duplicated(id)) %>% view()

#sentiment analysis for shits: 
tokens.afinn <- tokens %>% inner_join(get_sentiments("afinn"))

tokens.bing %>% 
  count(sentiment,word) %>% 
  arrange(desc(n)) %>% view()
tokens.nrc %>% 
  count(sentiment,word) %>% 
  arrange(desc(n)) %>% view()

tokens.afinn %>% summarise(mean(value))

bigrams <- shedding.dat %>% 
  unnest_tokens(word, text, token = "ngrams", n = 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(word,word1, word2, sep = " ") %>% 
  count(word) %>% 
  arrange(desc(n)) 

tokens$word %<>% lemmatize_words()



##tf_idf
tokens.tf <- tokens %<>%
  count(id,word) %>% 
  arrange(desc(n)) 
total_words <- tokens.tf %>% group_by(id) %>% summarize(total = sum(n))

tokens.tf <- left_join(tokens.tf, total_words)

tokens.tf <- tokens.tf %>%
  bind_tf_idf(word, id, n)

dtm <- textmineR::CreateDtm(shedding.dat$text, 
                            doc_names = shedding.dat$id, 
                            ngram_window = c(1, 2))

tokens.tf <- Corpus(VectorSource(tokens.tf))
TextDoc_dtm <- TermDocumentMatrix(tokens.tf)

findAssocs(TextDoc_dtm, terms = c("feel","ashamed","black","hispanic","racism"), corlimit = .25)
TextDoc_dtm <- TermDocumentMatrix(tokens)


tokens.tf %>% 
  arrange(desc(tf_idf)) %>% 
  head(30) %>% view()



##trying word embeddings w tidy_text
#install.packages("widyr")
library(widyr)

tidy.embedding.test <- shedding.dat %>% select(id,text)

#Now we can calculate the Skipgram probabilities– 
#or how often we find each word next to every other word within the context window. 

#create context window with length 8
tidy_skipgrams <- tidy.embedding.test %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 10) %>%
  mutate(ngramID = row_number()) %>% 
  tidyr::unite(skipgramID, id, ngramID) %>%
  unnest_tokens(word, ngram)

#calculate unigram probabilities (used to normalize skipgram probabilities later)
# (or the overall frequency of each word in the corpus)
unigram_probs <- tidy.embedding.test %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

#calculate probabilities
skipgram_probs <- tidy_skipgrams %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

#normalize probabilities
normalized_prob <- skipgram_probs %>%
  filter(n > 10) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

normalized_prob[2005:2010,]

normalized_prob %>% 
  filter(word1 == "white") %>%
  arrange(-p_together)

pmi_matrix <- normalized_prob %>%
  mutate(pmi = log10(p_together)) %>%
  cast_sparse(word1, word2, pmi)

#install.packages("irlba")
library(irlba)
#remove missing data
pmi_matrix@x[is.na(pmi_matrix@x)] <- 0
#run SVD
pmi_svd <- irlba(pmi_matrix, 256, maxit = 500)
#next we output the word vectors:
word_vectors <- pmi_svd$u
rownames(word_vectors) <- rownames(pmi_matrix)

#install.packages("broom")
library(broom)

search_synonyms <- function(word_vectors, selected_vector) {
  
  similarities <- word_vectors %*% selected_vector %>%
    tidy() %>%
    as_tibble() %>%
    rename(token = .rownames,
           similarity = unrowname.x.)
  
  similarities %>%
    arrange(-similarity)    
}

pres_synonym <- search_synonyms(word_vectors,word_vectors["feel",])

pmi_svd <- irlba(pmi_matrix, 2, maxit = 500)

#next we output the word vectors:
word_vectors <- pmi_svd$u
rownames(word_vectors) <- rownames(pmi_matrix)

#grab 100 words
forplot<-as.data.frame(word_vectors[200:300,])
forplot$word<-rownames(forplot)

#now plot
#library(ggplot2)
ggplot(forplot, aes(x=V1, y=V2, label=word))+
  geom_text(aes(label=word),hjust=0, vjust=0, color="blue")+
  theme_minimal()+
  xlab("First Dimension Created by SVD")+
  ylab("Second Dimension Created by SVD")

###EXTRA##



###LDA
install.packages("textmineR")

dtm <- textmineR::CreateDtm(shedding.dat$text, 
                            doc_names = shedding.dat$id, 
                            ngram_window = c(1, 2))
#explore the basic frequency
tf <- textmineR::TermDocFreq(dtm = dtm)
original_tf <- tf %>% select(term, term_freq,doc_freq)
rownames(original_tf) <- 1:nrow(original_tf)

# Eliminate words appearing less than 2 times or in more than half of the
# documents
vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]

k_list <- seq(1, 10, by = 1)
model_dir <- paste0("models_", digest::digest(vocabulary, algo = "sha1"))
if (!dir.exists(model_dir)) dir.create(model_dir)
model_list <- textmineR::TmParallelApply(X = k_list, FUN = function(k){
  filename = file.path(model_dir, paste0(k, "_topics.rda"))
  if (!file.exists(filename)) {
    m <- textmineR::FitLdaModel(dtm = dtm, k = k, iterations = 500)
    m$k <- k
    m$coherence <- textmineR::CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  
  m
}, export=c("dtm", "model_dir")) # export only needed for Windows machines
#model tuning
#choosing the best model
coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)
ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,20,1)) + ylab("Coherence")

model <- model_list[which.max(coherence_mat$coherence)][[ 1 ]]
model$top_terms <- textmineR::GetTopTerms(phi = model$phi, M = 20)
top10_wide <- as.data.frame(model$top_terms)

model$topic_linguistic_dist <- textmineR::CalcHellingerDist(model$phi)
model$hclust <- hclust(as.dist(model$topic_linguistic_dist), "ward.D")
model$hclust$labels <- paste(model$hclust$labels, model$labels[ , 1])
plot(model$hclust)

#visualising topics of words based on the max value of phi
set.seed(1234)
final_summary_words <- data.frame(top_terms = t(model$top_terms))
final_summary_words$topic <- rownames(final_summary_words)
rownames(final_summary_words) <- 1:nrow(final_summary_words)
final_summary_words <- final_summary_words %>% reshape2::melt(id.vars = c("topic"))
final_summary_words <- final_summary_words %>% rename(word = value) %>% select(-variable)
final_summary_words <- left_join(final_summary_words,tokens)
final_summary_words <- final_summary_words %>% group_by(topic,word) %>%
  arrange(desc(value))
final_summary_words <- final_summary_words %>% group_by(topic, word) %>% filter(row_number() == 1) %>% 
  ungroup() %>% tidyr::separate(topic, into =c("t","topic")) %>% select(-t)
word_topic_freq <- left_join(final_summary_words, original_tf, by = c("word" = "term"))
pdf("cluster.pdf")
for(i in 1:length(unique(final_summary_words$topic)))
{  wordcloud(words = subset(final_summary_words ,topic == i)$word, freq = subset(final_summary_words ,topic == i)$value, min.freq = 1,
             max.words=200, random.order=FALSE, rot.per=0.35, 
             colors=brewer.pal(8, "Dark2"))}
dev.off()

