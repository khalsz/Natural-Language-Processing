#loading Libraries
library(tidyverse)
library(magrittr)
library(dplyr)
library(jsonlite)
library(httr)
library(sf)
library(tmap)
library(tidytext)
library(wordcloud)
library(stm)
library(tidytext)
library(reshape2)
library(quanteda)
library(tidylo)
library(stringr)
library(quanteda)
library(ggplot2)
library(gridExtra)
library(wordcloud2)
library(maptools)
library(spatstat)
library(ggraph)
library(igraph)
library(textdata)

setwd('C:/Assignment_2-datapack')

# Part 1
## loading and filtering excel data
excel_data <- read.csv('wikipedia_geotags_in_UK.csv')

filter_excel <- excel_data %>% filter(LAD21NM == 'Ryedale') %>% 
  filter(gt_primary == 1)

##Creating empty dataframe for the text
dfpage_n_W <- data_frame()

## Scraping Wikipedia Web data
for (page_name in filter_excel$page_title) {
  # Set a title
  # Retrieve the summary
  a_page_summary <-
    httr::GET(
      # Base API URL
      url = "https://en.wikipedia.org/w/api.php",
      # API query definition
      query = list(
        # Use JSON data format
        format = "json",
        action = "query",
        # Only retrieve the intro
        prop = "extracts",
        exintro = 1,
        explaintext = 1,
        redirects = 1,
        # Set the title
        titles = page_name
      )
    ) %>%
    # Get the content
    httr::content(
      as = "text",
      encoding = "UTF-8"
    ) %>%
    # Trasnform JSON content to R list
    jsonlite::fromJSON() %>%
    # Extract the summary from the list
    magrittr::extract2("query") %>%
    magrittr::extract2("pages") %>%
    magrittr::extract2(1) %>%
    magrittr::extract2("extract")
  
  summ <- data_frame(a_page_summary) %>% 
    mutate(page_name = page_name)
  
  dfpage_n_W <- dfpage_n_W %>% 
    bind_rows(summ)
}

#The excel file filtered to contain only Ryedale district contains 289
#variables and the extracted data also contains 289 

#loading in boundary shapefile of the study area
boundary1 <- read_sf('Census 2022-06-17-03-56/census.shp')
boundary <- read_sf('Census 2022-06-17-03-56/ryedaledissolved.shp')
boundary <- boundary[1]
plot(boundary)
boundary_m <- tm_shape(boundary)+
  tm_borders()

#removing '~' from the data
dfpage_n_W <- data.frame(lapply(dfpage_n_W, function(x){sub('~', '', x)}))

filter_excel2 <- filter_excel %>% select('gt_lat', 'gt_lon', 'page_title')
#merging the data with the excel file to have coordinate   
sentcs_w_pgs <- dfpage_n_W %>% 
  left_join(filter_excel2, c('page_name' = 'page_title')) %>% 
  st_as_sf(coords = c("gt_lon", "gt_lat"), 
           crs = 4326, na.fail = TRUE) %>%  
  st_transform(27700)

#Tokenizing word and removing stopword
T_W_n_Pg <- sentcs_w_pgs %>% 
  unnest_tokens(word, a_page_summary) %>% 
  anti_join(get_stopwords(),  c('word' = 'word'))

#Tokenizing Sentences and removing stopword
Snt_n_pg <- sentcs_w_pgs %>% 
  unnest_tokens(sentence, a_page_summary, token = "sentences") 

# Part 2: Spatial Frequency Analysis

#Word Frequency

word_freq <- T_W_n_Pg %>% 
  count(word, sort = TRUE)
#There are 4172 unique word in Ryedale  Wikipedia page

#Sentence Frequency 
Setnc_freq <- Snt_n_pg %>% 
  count(sentence, sort = TRUE)
#There are 1504 unique sentences in Ryedale Wikipedia pages

top10_wds <- word_freq %>% 
  slice_max(n, n = 10)

top10_wds %>% 
  knitr::kable()

top10_wds %>% 
  ggplot(aes(fct_reorder(word,n), n, fill = word)) +
  geom_col() + 
  geom_text(aes(label = n), size = 3, fontface = "bold", vjust = -0.7) +
  labs(title = "Ryedale Pages Top 10 Words", 
       x = "Top 10 WOrds", y = "Frequency") +
  theme_bw()

boundary_m + 
  tm_shape(top10_wds)+ 
  tm_bubbles( col = 'n', title.size="Word Freq")+
  tm_layout(main.title = 'Ryedale Pages Top 10 Words Map', title.size = 0.7, 
            legend.outside=TRUE)

boundary_m + 
  tm_shape(top10_wds)+ 
  tm_bubbles( col = 'word', size = 'n', title.size="Word Freq")+
  tm_layout(main.title = 'Ryedale Pages Top 10 Words Map', title.size = 0.7, 
            legend.outside=TRUE)

#Word Cloud Map
word_freq %>%
  with(wordcloud(word, n, max.word = 100))

#Top 2 Sentences in Ryedale
tp_2_sent <- Setnc_freq %>%  
  slice_max(n, n =2)

tp_2_sent %>% 
  knitr::kable()


#Chart for top two Ryedale Sentences
tp_2_sent %>% 
  ggplot(aes(sentence, n, fill = sentence)) +
  geom_col() +
  geom_text(aes(label = n), size = 3, fontface = "bold", vjust = -0.7) +
  labs(title = "Top 10 most used word in Hackney's Pages", 
       x = "Top 10 most used word", y = "word frequency") +
  theme(axis.ticks.x = element_blank(),
           axis.text.x = element_blank())


#Plotting top 2 most frequent sentence in all Ryedale pages
boundary_m + 
  tm_shape(tp_2_sent)+ 
  tm_bubbles( col = 'n', title.size="Word Freq")+
  tm_layout(main.title = 'Top 10 Basettlaw Most Used word', title.size = 0.7, 
            legend.outside=TRUE)
#Places where these sentences are used are concentrated on the southern path of Ryedale. 
#The sentences are literately used to decribe Settrington which is located in the southern
#part if Ryedale. 

# - Point Pattern Analysis of Top 10 word in Ryedale
RyedaleOwin <- as.owin(boundary)
class(RyedaleOwin)

coord <- st_coordinates(top10_wds)[,c(1,2)]
RyedalePPP <- unique(ppp(coord[,1], coord[,2], window = RyedaleOwin))
summary(RyedalePPP)

#Top 10 Words Point Map
plot(RyedalePPP)
# density plot
den <- density(RyedalePPP)
plot(den, main='Top 10 word Density Map')
#This shows uneven distribution  of the top 10 words with concentration 
#at the particular point. 

#Quadrant point pattern analysis 
quadrnt <-  quadratcount(RyedalePPP, nx = 4, ny = 4)
plot(RyedalePPP)
plot(quadrnt, add = TRUE, cex = 1.5)
#The quadrant also confirms concentration of point at a particular area

Ke <- Kest(RyedalePPP)
plot(Ke)
#While the K theoretical value for each radius under the assumption of 
#complete randomness (Poisson) is represented by Kpois line and 
#Kiso line represents the observed K values, we can say that the graph confirms 
#clustering of points considering that the Kiso line is above the Kpois line. 

#Total word on each pages 
wrds_on_pgs <- T_W_n_Pg %>% 
  count(page_name, sort = TRUE)

Snt_on_pgs <- Snt_n_pg %>% 
  count(page_name, sort = TRUE)

#Pages Word frequency maps
boundary_m +
  tm_shape(wrds_on_pgs) + 
  tm_bubbles(col = "n", size = 'n')+
  tm_layout(main.title = 'Pages Word Frequency Map', title.size = 0.7, 
            legend.outside=TRUE)
#the pattern of pages words frequency is almost completely random

boundary_m +
  tm_shape(wrds_on_pgs %>% slice_max(n, n=10)) + 
  tm_bubbles(col = "n", shape = 'n')+
  tm_layout(main.title = 'Top 10 Pages Word Frequency Map', title.size = 0.7, 
            legend.outside=TRUE)

wrds_on_pgs %>% slice_max(n, n=10) %>% 
  knitr::kable()

wrds_on_pgs  %>%
  slice_max(n, n=10) %>% 
  ggplot(aes(fct_reorder(page_name, n), n, fill = page_name)) +
  geom_col(show.legend = TRUE) +
  geom_text(aes(label = n), size = 3, fontface = "bold", vjust = -0.5) +
  labs(title = "Top 10 pages with the highest word in Hackney", 
       y = "word frequency") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Pages sentence frequency map
boundary_m +
  tm_shape(Snt_on_pgs) + 
  tm_bubbles(col = "n", size = 'n')+
  tm_layout(main.title = 'Pages Sentence Frequency Map', title.size = 0.7, 
            legend.outside=TRUE)
#Also almost a random distribution of sentences frequency across the area. 


boundary_m +
  tm_shape(Snt_on_pgs %>% slice_max(n, n=10)) + 
  tm_bubbles(col = "n", shape = 'n')+
  tm_layout(main.title = 'Pages with top 10 Sentence Frequency Map', title.size = 0.7, 
            legend.outside=TRUE)
#The top 10 map shows cluster of pages with 10 to 15 sentences. 

Snt_on_pgs %>% slice_max(n, n=10) %>% 
  knitr::kable()

#The pages with the highest sentence frequency are different from that of word frequency. 



#Per page word frequency
word_per_pg <- T_W_n_Pg %>%
  count(page_name, word,  sort = TRUE)

Snt_per_pg <- Snt_n_pg %>% 
  count(page_name, sentence, sort = TRUE)

print(Snt_per_pg %>% filter(n > 1))
#This signifies that there is not two pages sharing the same sentence. 


boundary_m +
  tm_shape(word_per_pg) + 
  tm_bubbles(size = 'n')+
  tm_layout(main.title = 'Pages Sentence Frequency Map', title.size = 0.7, 
            legend.outside=TRUE)

word_per_pg %>% 
  slice_max(n, n=10) %>% 
  ggplot(aes(reorder(page_name, n), n, fill = word)) +
  geom_col(show.legend = TRUE) +
  geom_text(aes(label = n), size = 3, fontface = "bold", vjust = -0.5) +
  labs(title = "Top 10 pages with the highest word in Hackney", x = 'Page Name',
       y = "word frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

word_per_pg %>% 
  slice_max(n, n=10) %>% 
  knitr::kable()


#Term Freqency Analysis
#Per word term frequency analysis
term_words <- word_freq %>%
  mutate(term = n/sum(n))

#top ten words with highest term frequency
top10_termwords <- term_words %>%
  slice_max(n, n = 10)

top10_termwords %>%
  knitr::kable()
#Term frequency analysis also reveales that North and Yorkshire remain
#the most important words. 


top10_termwords %>%
  ggplot(aes(fct_reorder(word,term), term, fill = word)) +
  geom_col() +
  geom_text(aes(label = round(term, 3)), size = 3,
            fontface = "bold", vjust = -0.7) +
  labs(title = "Top 10 Words with highest term frequency",
       x = "Top 10 WOrds", y = "Frequency") +
  theme_bw()

boundary_m +
  tm_shape(top10_termwords)+
  tm_bubbles( col = 'term')+
  tm_layout(main.title = 'Top 10 Words with highest term frequency',
            title.size = 0.7,
            legend.outside=TRUE)

#The pattern in the map reveals cluster of lesser term frequency words

#Words per page Term frequency analysis
sum_total_word <- word_per_pg %>% 
  group_by(page_name) %>% 
  summarize(total = sum(n)) %>% 
  arrange(desc(total))

#adding sum column 
word_per_pg1 <- st_join(word_per_pg, sum_total_word, left = TRUE)

#Creating term frequency
word_per_pg1 %>% 
  mutate(term_freq = n/total) %>% 
  slice_max(term_freq, n= 10) 

word_per_pg1 %>% 
  mutate(term_freq = n/total) %>% 
  slice_max(term_freq, n= 10) %>% 
  ggplot( aes(term_freq,  fill = word)) +
  geom_histogram(show.legend = TRUE, bins = 12) +
  facet_wrap(~page_name.x, ncol = 2, scales = "free_y")+ 
  labs(title = "Top 10 Words per Page highest term frequency") 

word_per_pg1 %>% 
  mutate(term_freq = n/total) %>% 
  slice_max(term_freq, n= 10) %>% 
  ggplot( aes(term_freq,  fill = word)) +
  geom_histogram(show.legend = TRUE, bins = 12) +
  labs(title = "Top 10 Words per Page highest term frequency") 
#Term Frequency: Malton is the most important word per page term frequency

term_tb <- word_per_pg1 %>% 
  mutate(term_freq = n/total) %>% 
  slice_max(term_freq, n= 10) 

boundary_m +
  tm_shape(term_tb) + 
  tm_bubbles(size = 'term_freq', col = 'term_freq')+
  tm_layout(main.title = 'Pages Sentence Frequency Map', title.size = 0.7, 
            legend.outside=TRUE)
#This is an indication that most of the high ranked word per term frequency ranking in
# are located in almost the same area. The word are located in the Malton_Museum pages, 
#Malton School and Crambeck. No doubt, the word are related to the pages. 
  

#Tfidf analysis
tfidf <- word_per_pg %>%
  bind_tf_idf(word, page_name, n) %>%
  arrange(desc(tf_idf))

top10_tfidf <- tfidf %>% 
  slice_max(tf_idf, n=10)

top10_tfidf 
#Museum is also among the high rank word in the tf_idf analysis


top10_tfidf %>% 
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = page_name)) +
  geom_col() + 
  labs(title = 'Top 10 tf-idf word Per Page in Hackney', x = 'tf-idf Score', y = 'word')


boundary_m +
  tm_shape(top10_tfidf) + 
  tm_bubbles( col = 'tf_idf')+
  tm_layout(main.title = 'Pages Sentence Frequency Map', title.size = 0.7, 
            legend.outside=TRUE)

tfidf %>% 
  group_by(page_name) %>% 
  slice_max(tf_idf, n=1) %>% 
  ungroup() 

#we can see 'u.s' in the 1949_Ryder_Cup page. Let's investigate. 

dim(sentcs_w_pgs %>% 
  filter(str_detect(a_page_summary, 'u.s')) %>% 
  select(a_page_summary) )

#The word 'u.s' appears in 61 pages. 


Ryedale_bigrams <- sentcs_w_pgs %>% 
  unnest_tokens(bigrams, a_page_summary, token = 'ngrams', n = 2)


Ryedale_bigrams[c('word1', 'word2')] <- str_split_fixed(
  string = Ryedale_bigrams$bigrams, pattern = " ", n=2
)

anti_Ryedale_bigrams <- Ryedale_bigrams %>% 
  anti_join(get_stopwords(), c('word1' = 'word')) %>% 
  anti_join(get_stopwords(), c('word2' = 'word'))

count_bigram <- anti_Ryedale_bigrams %>% 
  count(word1, word2, sort = TRUE)

count_bigram


top10_count_bigram <- count_bigram %>%
  filter(n > 10) 

top10_count_bigram %>% 
  knitr::kable()

#Creating igraph object
#visualizing relationship between word
bigram_graph <- top10_count_bigram%>%
  graph_from_data_frame()


ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

#The word 'north' had most connection with other word and forms most centre nodes, 
#which are connected bt the name of places. 'km' and miles also have high connections
#However, most word that connect with 'km' are also interconnected, hence, would likely 
#form a cycle as part of phrases. 



### 3. 0 Sentiment Analysis 
# - Bing Sentiment Analysis 
bing_sent <- T_W_n_Pg %>% 
  inner_join(get_sentiments('bing'))

bing_sent_count <- bing_sent %>% 
  count(sentiment)

#Bing Sentiment Frequency Chart
bing_sent_count %>% 
  ggplot(aes(sentiment, n, fill = sentiment, label = scales::percent(n)))+ 
  geom_col()+ 
  ggtitle(label = 'Number of +ve and -ve Sentiments in Ryedale')+
  theme_bw()

#Top 100 Bing sentimental word cloud 
bing_sent %>% 
  count(word, sentiment, sort = TRUE) %>% 
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(random.order=TRUE, title.size=9, fixed.asp=TRUE,
                   colors = c("indianred3","lightsteelblue3"),
                   max.word = 100) 


# - Afinn Sentinment Analysis 
afinn_sent <- T_W_n_Pg %>% 
  inner_join(get_sentiments('afinn'))

top_affin <- afinn_sent %>% 
  count(value) %>% 
  slice_max(n, n=10) 

top_affin %>% 
  knitr::kable()

boundary_m +
  tm_shape(top_affin) + 
  tm_bubbles( col = 'value')+
  tm_layout(main.title = 'Top 10 frequency Afin sentiment rank Map', title.size = 0.7, 
            legend.outside=TRUE)

#Top 100 Afinn ranking sentiment word cloud 
afinn_sent %>% 
  count(word, value, sort = TRUE) %>% 
  acast(word ~ value, value.var = "n", fill = 0) %>%
  comparison.cloud(random.order=TRUE, title.size=3, fixed.asp=TRUE,
                  max.word = 100) 

#Afinn Sentiment Ranking Frequency Chart
afinn_sent %>% 
  ggplot(aes(value, fill= factor(value)))+ 
  geom_histogram(bins = 20)+ 
  ggtitle(
    label = 'Frequency of Afinn Words Polarity Score Sentiments in Ryedale')+
  theme_bw()

#Ryedale Sentiment Rank Map
boundary_m +
  tm_shape(afinn_sent) + 
  tm_bubbles(col = 'value')+
  tm_layout(main.title = 'Ryedale Sentiments Rank Frequency Map', 
            title.size = 0.7, 
            legend.outside=TRUE)
#The map shows that there is somewhat no cluster pattern in the distribution
#of sentiment rank


# - NRC Sentinment Analysis 
nrc_sent <- T_W_n_Pg %>% 
  inner_join(get_sentiments('nrc'))

nrc_sent_count <- nrc_sent %>% 
  count(sentiment)

#NRC Emotion Sentiment Frequency Chart
nrc_sent_count %>% 
  ggplot(aes(sentiment, n, fill = sentiment))+ 
  geom_col()+ 
  ggtitle(label = 'Number of NRC Emotion Lexicon Sentiments in Ryedale')+
  theme_bw()

#Ryedale Emotion Sentiment Map 
boundary_m +
  tm_shape(nrc_sent_count) + 
  tm_bubbles(size = 'n', col = 'sentiment')+
  tm_layout(main.title = 'Ryedale Emotion Sentiments Frequency Map', 
            title.size = 0.7, 
            legend.outside=TRUE)
#The map shows that there is somewhat cluster of 'Trust' Sentiment with an average 
#count of around 600. 


#Per page Sentiment Analysis 
# - Bing 
bing_per_page_count <- bing_sent %>% 
  count(page_name, sentiment)

top10_bing_per_pg <- bing_per_page_count %>% 
  group_by(sentiment) %>% 
  slice_max(n, n= 10)

top10_bing_per_pg %>% 
  knitr::kable()

top10_bing_per_pg %>% 
  ggplot(aes(n, page_name, fill = sentiment))+ 
  geom_col()+ 
  ggtitle(label = 'Pages with Top 10 Bing Sentiment')+
  theme_bw()

boundary_m +
  tm_shape(top10_bing_per_pg) + 
  tm_bubbles(shape = 'sentiment', col = 'n')+
  tm_layout(main.title = 'Ryedale Top 10 Bing Sentiments Map', 
            title.size = 0.7, 
            legend.outside=TRUE)
#The Map shows that there is cluster of positive words in Ryedale among top 10 
#Bing Sentiment. 

#- NRC
nrc_per_page_count <- nrc_sent %>% 
  count(page_name, sentiment)

top3_nrc_per_pg <- nrc_per_page_count %>% 
  group_by(sentiment) %>% 
  slice_max(n, n= 3)

top3_nrc_per_pg %>% 
  knitr::kable()

top3_nrc_per_pg %>% 
  ggplot(aes(n, page_name, fill = sentiment))+ 
  geom_col()+ 
  ggtitle(label = 'Pages with Top 3 NRC Emotion Sentiment')+
  theme_bw()

boundary_m +
  tm_shape(top3_nrc_per_pg) + 
  tm_bubbles(col = 'sentiment', size = 'n')+
  tm_layout(main.title = 'Ryedale Top 3 NRC Emotion Sentiment Map', 
            title.size = 0.7, 
            legend.outside=TRUE)
#No form of cluster was notice. However, sentiment 'Trust' 
#seems to show slight cluster. 

# - AFINN
afinn_per_page_count <- afinn_sent %>% 
  count(page_name, value)


top2_afinn_per_pg <- afinn_per_page_count %>% 
  group_by(value) %>% 
  slice_max(n, n= 2)

top2_afinn_per_pg %>% 
  knitr::kable()

top2_afinn_per_pg %>% 
  ggplot(aes(n, page_name, fill = value))+ 
  geom_col()+ 
  ggtitle(label = 'Pages with Top 2 Afinn Sentiment Rank')+
  theme_bw()

boundary_m +
  tm_shape(top2_afinn_per_pg) + 
  tm_bubbles(col = 'value', size = 'n')+
  tm_layout(main.title = 'Ryedale Top 2 Afinn Ranking Sentiment Map', 
            title.size = 0.7, 
            legend.outside=TRUE)

#Only rank value '4 to 6' shows slight cluster in the map. 

page_word_count %>%
  inner_join(get_sentiments(lexicon = 'bing')) %>%
  count(page_title, sentiment) %>% 
  spread(sentiment, n, fill = 0)


## Sentiment Difference Analysis: Positive - Negative
bing_sent_diff <- bing_per_page_count %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sent_diff = positive - negative) 

bing_sent_diff %>% 
  slice_max(sent_diff, n=20)


min_max_diff <- rbind(
bing_sent_diff %>% 
  arrange(desc(sent_diff)) %>%
  head(10), 
bing_sent_diff %>% 
  arrange(desc(sent_diff)) %>%
  tail(10))
  
min_max_diff %>%
  ggplot(aes(sent_diff, page_name, fill = page_name)) +
  geom_col(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        plot.title = element_text(size=9), 
        axis.text.y = element_text(size=7, angle = 20)) +
  ylab("Page Name")+
  xlab('Sentiment Difference')+
  labs(title = 'Top 10 and Bottom 10 Pages Sentiment Difference', 
       caption = '(Based on data from Wikipedia)')+ 
  theme_bw()

boundary_m +
  tm_shape(min_max_diff) + 
  tm_bubbles(col = 'sent_diff')+
  tm_layout(main.title = 'Ryedale Top 2 Afinn Ranking Sentiment Map', 
            title.size = 0.7, 
            legend.outside=TRUE)
#The positive sentiment difference points are slightly clustered while the negative 
#are dispersed. Places with more positive sentiment words tends to be located around 
#the hearth of the study area. While places with more negative values are distributed 
#in a dispersed way. 





#https://www.tidytextmining.com/ngrams.html
#Atenstaedt R. Word cloud analysis of the BJGP. Br J Gen Pract. 2012 Mar;62(596):148. doi: 10.3399/bjgp12X630142. PMID: 22429422; PMCID: PMC3289811.
#https://gistbok.ucgis.org/bok-topics/point-pattern-analysis