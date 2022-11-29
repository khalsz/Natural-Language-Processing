######## Course Work 2 GY7708 #######
#Geographic Information retrieval and Sentiment analysis 

#Loading Libraries
library(sf)
library(tidyverse)
library(cowplot)
library(stringi)
library(tidytext)
library(wordcloud)
library(reshape2)
library(spdep)
library(topicmodels)

#Loading in the CSV file
wiki_geo <- read.csv('data/wikipedia_geotags_in_UK.csv')

#Loading boundary shapefile of the study area
hackneyshp <- st_read('data/hackney/Export_Output.shp')
## The geometry type is Polygon and the CRS is OSGB 1936 / British National Grid


#Ploting the boundary shapefile of the study area
plot(hackneyshp$geometry)


#Filtering the CSV to my allocated area
wiki_geo <- wiki_geo %>% 
  filter(LAD21NM == 'Hackney') %>% #selecting my allocated LAD
  filter(gt_primary == 1) #Removing pages without a geotag

#Selecting only columns that will be merged with the data
wiki_geo_coord <- wiki_geo %>% select(gt_id, gt_lat, gt_lon, page_title)

#Building function for the extraction of Wikipedia pages 
#in my allocated LAD.  
unnest_function <- function(page_title) {
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
      titles = page_title
    )
  ) %>%
  httr::content(
    as = "text",
    encoding = "UTF-8"
  ) %>%
  jsonlite::fromJSON() %>%
  # Extract the summary from the list
  magrittr::extract2("query") %>%
  magrittr::extract2("pages") %>%
  magrittr::extract2(1) %>%
  magrittr::extract2("extract") 

#Converting the text to dataframe
a_page_summary <- as.data.frame(a_page_summary)  


#creating a column to store each page title  
a_page_summary <- a_page_summary %>%
  mutate(page_title)
return(a_page_summary)

}

# Creating an empty dataframe that will be used to house the data
page_word <- data.frame(
  page_title = character(),
  a_page_summary = character()
)


# Created a loop to run the above function for each of the pages in my allocated LAD.  
for (i in 1:nrow(wiki_geo)) {
  #Print out the pages name for confirmation
  print(paste('Page Title:', wiki_geo$page_title[i])) 
  page_title <- wiki_geo$page_title[i]
  page_word <- page_word %>%
    add_row(unnest_function(page_title)) #adding results from new pages
}

#The number of Hackney pages extracted from Wikipedia is one lesser that the number in the 
#excel table (241:242). This is because the Wikipedia page for page_title 
#'The_Centre_of_Attention' is null; hence no data for The_Centre_of_Attention page.


#Adding the coordinate of the pages from the excel file
page_wordwtCd <- page_word %>% left_join(wiki_geo_coord, by = 'page_title')

#Converting the CRS of the data to British National Grid
page_word_Brt <- page_wordwtCd %>% 
  st_as_sf(coords = c("gt_lon", "gt_lat"), crs = 4326) %>%  
  st_transform(27700)



# word_count <- page_word%>%
#   unnest_tokens(word, a_page_summary) %>%
#   anti_join(get_stopwords()) #removing stopwords.



#Tokenizing the text and transforming it into
#tidy data structure
page_word_Brtun <- page_word_Brt %>%
  unnest_tokens(word, a_page_summary) %>% #Creating token word from the sentences
  anti_join(get_stopwords()) #removing stopwords.

#This allows easy vectorization of the text data as vectoring 
#is an important process when analyzing text with machine learning models. 

#Extracting the frequency of words 
total_word_count <- page_word_Brtun %>%  
  count(word,  sort = TRUE)
class(page_word_Brtun)


#Top 10 most used words in all the pages in Hackney
total_word_count %>%
  slice_max(n, n = 10) %>%
  knitr::kable(caption = "Top 10 Most Used Words in all Hackney Pages")

total_word_count %>%
  slice_min(n, n = 10) %>%
  head(10) %>% 
  knitr::kable(caption = "Bottom 10 Least Used Words in all Hackney Pages") 
  

total_word_count %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_col() +
  geom_text(aes(label = n), size = 3, fontface = "bold", vjust = -0.5) +
  labs(title = "Top 10 most used words in Hackney's Pages", 
       x = "Top 10 most used words", y = "Words frequency") +
  theme_bw()
#The most used non-stop words in my allocated area(Hackney) is London
#The most used word in all the Hackney pages are: 
#"london, hackney, borough, road, street, school, east, station, area, shoredicth" 
#This is becuase teh area is in east london. Also, it can be assumed that there are
#lots of schools and stations in the area. Meanwhile, the word "shoreditch" which is the
#10th most used word is an important word in Hackney; it represents an administrative that 
#consists of different important boundaries. 

#Word Cloud representing each word with size based on their frequency
total_word_count %>%
  with(wordcloud(word, n, max.words = 200))

#Top 10 most used words in all Hackney Pages
top_10_hackney_wds <- total_word_count %>%
  slice_max(n, n = 10)


#Map of Top 10 most used words in Hackney in Hackney 
ggplot() +
  geom_sf(data = hackneyshp, color = 'black') + 
  geom_sf(data = top_10_hackney_wds, aes(color = n, shape = factor(word))) + 
  scale_shape(name = "Words")+
  scale_color_steps(name = 'Word Frequency')+
  theme_bw()+
  labs(title = 'Top 10 Most Used Words on Hackney Pages') + 
  scale_shape_manual(values = 0:10)
  
#Top 10 term Frequent words in all Hackney Pages
top_10_hackney_tem_freq_wds <- top_10_hackney_wds %>% 
  mutate( term_freq = n/sum(n)) #term frequency

#Map of Top 10 most important word in Hackney based on Term frequency 
ggplot() +
  geom_sf(data = hackneyshp, color = 'black') + 
  geom_sf(data = top_10_hackney_tem_freq_wds, 
          aes(color = term_freq, shape = factor(word))) + 
  scale_color_steps(name = 'Word Frequency')+
  theme_bw()+
  labs(title = 'Top 10 Most Important Words') + 
  scale_shape_manual(values = 0:10)


#total number of words on pages 
total_wd_on_pages <- page_word_Brtun %>%
  count(page_title,  sort = TRUE)

#Word Frequency map for all Hackney pages
ggplot() +
  geom_sf(data = hackneyshp, color = 'black') + 
  geom_sf(data = total_wd_on_pages, aes(color = n, size = n)) + 
  scale_size(name = "Word Count")+
  scale_color_steps(name = 'Word Frequency')+
  theme_bw()+
  labs(title = 'Hackey Pages Word Frequency Map')


#Top 10 pages with the highest word frequency
top_10page_wt_hig_wds <- total_wd_on_pages  %>%
  slice_max(n, n=10) 

top_10page_wt_hig_wds %>% 
  knitr::kable(caption = 'Top 10 Pages with Higest Word Count')

bottom_10page_wt_hig_wds <- total_wd_on_pages  %>%
  slice_min(n, n=10) 

bottom_10page_wt_hig_wds %>% 
  knitr::kable(caption = 'Bottom 10 Pages with Lowest Word Count')

#Map of top 10 Hackney pages with the highest word frequency 
ggplot() +
  geom_sf(data = hackneyshp, color = 'black') + 
  geom_sf(data = top_10page_wt_hig_wds, 
          aes(size = n, shape = factor(page_title ))) + 
  scale_size(name = "Word Count")+
  theme(legend.title = element_text(size = 8), 
        legend.text  = element_text(size = 6))+
  labs(title = 'Top 10 Hackey Pages with Highest Word Frequency') + 
  scale_shape_manual(values = 0:10)

#Map of Bottom 10 Hackney pages with the Lowest word frequency 
ggplot() +
  geom_sf(data = hackneyshp, color = 'black') + 
  geom_sf(data = bottom_10page_wt_hig_wds, 
          aes(size = n, shape = factor(page_title ))) + 
  scale_size(name = "Word Count")+
  theme(legend.title = element_text(size = 8), 
        legend.text  = element_text(size = 6))+
  labs(title = 'Bottom 10 Hackey Pages with Lowest Word Frequency') + 
  scale_shape_manual(values = 0:10)
  

#Histogram of Top 10 Hackney pages with the highest word frequency 
total_wd_on_pages  %>%
  slice_max(n, n=10) %>% 
  ggplot(aes(reorder(page_title, n), n, fill = page_title)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), size = 3, fontface = "bold", vjust = -0.5) +
  labs(title = "Top 10 pages with the highest words in Hackney", 
       x = "Top 10 Pages", y = "Words frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

bottom_10page_wt_hig_wds  %>%
  slice_max(n, n=10) %>% 
  ggplot(aes(reorder(page_title, n), n, fill = page_title)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), size = 3, fontface = "bold", vjust = -0.5) +
  labs(title = "Hackney's Bottom 10 Pages with the Least words Frequency", 
       x = "Bottom 10 Pages", y = "Words frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

## Spatial Autocorrelation of words frequency in Hackney Pages

#Converting the sf data into sp data to extract coordinates
sp_total_wd_on_pages <- as(total_wd_on_pages, 'Spatial')
coord <- sp::coordinates(sp_total_wd_on_pages)
str(coord)

# Creating matrix of points for 1 nearest neighbors
k1 <- knn2nb(knearneigh(coord, k = 1))


# calculating upper bound euclidean distance for atleast 1 neighbor for all points
Eucl_k1dist<- max(unlist(nbdists(k1,coord)))

#Source: https://github.com/khalsz/My-PGDip-Projects-/blob/main/Spatial%20ANalysis%20of%20Close%20Stores.R

#Building/defining neighbor points (pages) based on the maximum euclidean distance
sp_total_wd_on_pages.dist  <-  dnearneigh(coord, 0, Eucl_k1dist)  

sp_total_wd_on_pages.dist

##the result shows that there are 241 points to be linked. The total number of 
##connections (neighbors) is 2452, and the average number of links (neighbors) is 10.17


#plotting the neighbor points

plot(hackneyshp$geometry, border='black', lwd=2)
plot(sp_total_wd_on_pages.lw, add=TRUE, coord, col='brown', lwd=1)


sp_total_wd_on_pages.lw <- nb2listw(sp_total_wd_on_pages.dist, 
                                    style="W",zero.policy=T) 

#performing Global Moran's I index with 999 simulations
moran  <-  moran.mc(sp_total_wd_on_pages$n, sp_total_wd_on_pages.lw, 
                    nsim=999, zero.policy = T) 

print(moran)
##The p-value is 0.675. This indicates that there is about 67% chances of being wrong in 
## rejecting the null hypothesis that there is a random distribution 
## Hackney pages word frequency. Hence, there is no clustering or specific pattern of 
## Hackney pages word frequency

plot(moran, main="Moran: Autocorrelation of Pages' Word Count", las=1) 


# library(gridExtra)
# 
# plot_grid(vcv, terfs, labels = "AUTO")

# ggplot() +
#   geom_sf(data = sdcounty, aes(fill = C2003), color = "black", size = 0.25) + 
#   scale_fill_distiller(name="Cases", palette = "YlGn", breaks = pretty_breaks())+
#   geom_sf(data = sdcntrd, aes(size = P2003), color = "black", show.legend = "point") +
#   scale_size(name = "Population") + 
#   theme_bw() +
#   labs(title="2003 West Nile Virus Cases in South Dakota")


#Frequency of each word per page
page_word_count <- page_word_Brtun %>%
  count(page_title, word,  sort = TRUE)


#Top 10 words used per page in
page_word_count %>%
  slice_max(n, n=10) %>%
  knitr::kable()

# ggplot() +
#   geom_sf(data = hackneyshp, color = 'black') + 
#   geom_sf(data = page_word_count, aes(color = n, size = n)) + 
#   scale_size(name = "Word Count")+
#   scale_color_steps(name = 'Word Frequency')+
#   geom_sf_text(data = page_word_count, aes(label = page_title), size = 3, 
#                color = 'black', check_overlap = T)+
#   theme_bw()+
#   labs(title = 'Top 10 Word Frequency Per Hackey Pages') 

#Map of Word Frequency Per Hackney Pages
ggplot() +
  geom_sf(data = hackneyshp, color = 'black') + 
  geom_sf(data = page_word_count, aes(color = n, size = n),  
          shape = factor(page_title )) + 
  scale_size(name = "Word Frequency")+
  scale_color_steps(name = 'Word Frequency')+
  theme_bw()+
  labs(title = 'Word Frequency Per Hackey Pages') 


#Histogram of Top 10 Word Frequency Per Hackney Pages
page_word_count %>%
  slice_max(n, n=10) %>%
  mutate(ordr = reorder(word, n)) %>%
  ggplot(aes(ordr, n, fill = page_title)) +
  geom_col() +
  labs(title = 'Top 10 Word Frequency Per Hackey Pages', 
       x = "Top 10 Words", y = "Words frequency") +
  coord_flip()


top10_page_word_count <- page_word_count %>%
  slice_max(n, n=10)

#Map of Top 10 Word Frequency Per Hackney Pages
ggplot() +
  geom_sf(data = hackneyshp, color = 'black') +
  geom_sf(data = top10_page_word_count, aes(color = n, size = n)) +
  scale_size(name = "Word Count")+
  scale_color_steps(name = 'Word Frequency')+
  geom_sf_text(data = top10_page_word_count, aes(label = page_title), size = 3,
               color = 'black', check_overlap = T)+
  theme_bw()+
  labs(title = 'Top 10 Word Frequency per Hackey Pages ')

#Map of Top 10 Word Frequency Per Hackney Pages
# ggplot() +
#   geom_sf(data = hackneyshp, color = 'black') + 
#   geom_sf(data = top10_page_word_count, 
#           aes(size = n, shape = factor(page_title ))) + 
#   scale_size(name = "Word Count")+
#   scale_color_steps(name = 'Word Frequency')+
#   theme_bw()+
#   labs(title = 'Top 10 Word Frequency per Hackey Pages ') + 
#   scale_shape_manual(values = 0:15)

ggplot(top10_page_word_count) + 
  geom_sf(aes(color = n), show.legend = FALSE) + 
  geom_sf(data = hackneyshp, color = 'black') +
  geom_sf_label(aes(label = page_title), size = 2.5)+ 
  theme_bw()+
  labs(title = 'Top 10 Word Frequency per Hackey Pages ')




# ggplot() +
#   geom_sf(data = hackneyshp, color = 'black') + 
#   geom_sf(data = top10_page_word_count, aes(color = n, size = n,  shape = factor(page_title ))) + 
#   scale_size(name = "Word Count")+
#   scale_color_steps(name = 'Word Frequency')+
#   geom_sf_text(data = top10_page_word_count, aes(label = page_title), size = 3, 
#                color = 'black', check_overlap = T)+
#   theme_bw()+
#   labs(title = 'Top 10 Word Frequency Per Hackey (Top 10) Pages') + 
#   scale_shape_manual(values = 0:15) + 
#   scale_color_viridis(discrete=FALSE)


# #Most used words in each of the pages 
# page_word_count %>%
#   group_by(page_title) %>%
#   slice_max(n) %>%
#   knitr::kable()


#Top 10 Normalized (term) frequency of words per page title
page_word_count %>% 
  group_by(page_title) %>%
  mutate( term_freq = n/sum(n)) %>% 
  ungroup() %>% 
  slice_max(term_freq, n=10) %>% 
  knitr::kable()

#Overall top 10 Per page word term frequency Histogram
##This section shows the term frequency of each words per their individual pages word frequency, 
## and the overall top ten of these term frequency score. 
page_word_count %>% 
  group_by(page_title) %>%
  mutate( term_freq = n/sum(n)) %>% #term frequency
  ungroup() %>% 
  slice_max(term_freq, n = 10) %>% 
  ggplot(aes(word, term_freq, fill = page_title)) + 
  geom_col() +
  theme(legend.title = element_text(size = 10), 
        legend.text  = element_text(size = 8), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
  labs(title = "Overall top 10 Per Page Words Term Frequency", 
       y = "Words term frequency", x= 'Top 10 Words')



top10temfreqppage <- page_word_count %>% 
  group_by(page_title) %>%
  mutate( term_freq = n/sum(n)) %>% #term frequency
  ungroup() %>% 
  slice_max(term_freq, n = 10)

#Overall top 10 Per page word term frequency map
ggplot() +
  geom_sf(data = hackneyshp, color = 'black') + 
  geom_sf(data = top10temfreqppage, aes(color = term_freq, size = term_freq)) + 
  scale_size(name = "Term Freqency")+ 
  scale_color_steps(name = 'Word Frequency')+
  geom_sf_text(data = top10temfreqppage, aes(label = page_title), size = 3, 
               color = 'black', check_overlap = T)+
  theme_bw()+
  labs(title = 'Overall top 10 Per Page Words Term Frequency')


ggplot(top10temfreqppage) + 
  geom_sf(aes(color = term_freq), show.legend = FALSE) + 
  geom_sf(data = hackneyshp, color = 'black') +
  geom_sf_label(aes(label = word), size = 2.5)+ 
  theme_bw()+
  labs(title = 'Overall top 10 Per Page Words Term Frequency')


#Top 10 per page work term frequency
#This section shows the per page term frequency of each word based on the 
#overall word count in all Hackney pages 
page_word_count %>%
  mutate( term_freq = n/sum(n)) %>%
  slice_max(term_freq, n=10) %>%
  knitr::kable()

temfreqppage_top10 <- page_word_count %>%
  mutate( term_freq = n/sum(n)) %>%
  slice_max(term_freq, n=10) 

#Histogram of top 10 Per Page Overall Words Term Frequency
temfreqppage_top10 %>% 
  ggplot(aes(word, term_freq, fill = page_title)) + 
  geom_col() +
  theme(legend.title = element_text(size = 10), 
        legend.text  = element_text(size = 8), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
  labs(title = "Top 10 Per Page Overall Words Term Frequency", 
       y = "Words term frequency", x= 'Top 10 Words')


##Top 10 Per Page Overall Words Term Frequency map
ggplot() +
  geom_sf(data = hackneyshp, color = 'black') + 
  geom_sf(data = temfreqppage_top10, aes(color = term_freq, size = term_freq)) + 
  scale_size(name = "Term Freqency")+ 
  scale_color_steps(name = 'Word Frequency')+
  geom_sf_text(data = temfreqppage_top10, aes(label = page_title), size = 3, 
               color = 'black', check_overlap = T)+
  theme_bw()+
  labs(title = 'Top 10 Per Page Overall Words Term Frequency')


ggplot(temfreqppage_top10) + 
  geom_sf(aes(color = term_freq), show.legend = FALSE) + 
  geom_sf(data = hackneyshp, color = 'black') +
  geom_sf_label(aes(label = word), size = 2.5)+ 
  theme_bw()+
  labs(title = 'Top 10 Per Page Overall Words Term Frequency')


#Top 10 Tfidf score of words per page
page_word_count %>%
  bind_tf_idf(word, page_title, n) %>%
  slice_max(tf_idf, n=10) %>%
  knitr::kable()

page_word_count %>%
  bind_tf_idf(word, page_title, n) %>%
  slice_max(tf_idf, n=10) %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = page_title)) +
  geom_col() + 
  labs(title = 'Top 10 tf-idf Words Per Page in Hackney', x = 'tf-idf Score', y = 'Words')


top10_tfidfppage <- page_word_count %>%
  bind_tf_idf(word, page_title, n) %>%
  slice_max(tf_idf, n=10)

ggplot() +
  geom_sf(data = hackneyshp, color = 'black') + 
  geom_sf(data = top10_tfidfppage, aes(color = tf_idf, size = tf_idf)) + 
  scale_size(name = "Tf-idf Score")+ 
  scale_color_steps(name = 'Tf-idf Score')+
  geom_sf_text(data = top10_tfidfppage, aes(label = page_title), size = 3, 
               color = 'black', check_overlap = T)+
  theme_bw()+
  labs(title =  'TF-IDF: Top 10 Hackey Most Important Words')

ggplot(top10_tfidfppage) + 
  geom_sf(aes(color = tf_idf), show.legend = FALSE) + 
  geom_sf(data = hackneyshp, color = 'black') +
  geom_sf_label(aes(label = word), size = 2.5)+ 
  theme_bw()+
  labs(title = 'TF-IDF: Top 10 Hackey Most Important Words')


#Weighted Log 
top10weightlog <- page_word_count %>% 
  st_drop_geometry() %>% 
  bind_log_odds(page_title, word, n) %>%
  # group_by(page_title) %>%
  # slice_max(log_odds_weighted) %>%
  # ungroup() %>% 
  slice_max(log_odds_weighted, n =10)

top10weightlog%>% 
  knitr::kable()


top10weightlog %>% 
  ggplot(aes(log_odds_weighted, fct_reorder(word, log_odds_weighted), 
             fill = page_title)) +
  geom_col() + 
  labs(title = 'Log Weight: Top 10 Most Important Words in Hackney', 
       x = 'Log Weight Score', y = 'Words') +
  theme_bw()


top10weightlogC <- top10weightlog %>% left_join(wiki_geo_coord)%>% 
  st_as_sf(coords = c("gt_lon", "gt_lat"), crs = 4326) %>%  
  st_transform(27700)
  
  
ggplot() +
  geom_sf(data = hackneyshp, color = 'black') + 
  geom_sf(data = top10weightlogC, 
          aes(color = log_odds_weighted, size = log_odds_weighted )) + 
  scale_size(name = "Log Weight Score")+ 
  scale_color_steps(name = 'Log Weight Score')+
  geom_sf_text(data = top10weightlogC, aes(label = page_title), size = 3, 
               color = 'black', check_overlap = T)+
  theme_bw()+
  labs(title = 'Weighted Log Odds: Top Most Important Words in Hackey')

ggplot(top10weightlogC) + 
  geom_sf(aes(color = log_odds_weighted), show.legend = FALSE) + 
  geom_sf(data = hackneyshp, color = 'black') +
  geom_sf_label(aes(label = word), size = 2.5)+ 
  theme_bw()+
  labs(title = 'Weighted Log Odds: Top Most Important Words in Hackey')


### 3. 0 Sentiment Analysis 
#top 10 words used in all pages and their sentiment analysis
sent_pg_count <- page_word_count %>%
  inner_join(get_sentiments(lexicon = 'bing')) %>%
  count(page_title, sentiment) %>% 
  slice_max(n, n=10) 


#Top pages with highest positive and negative words 
top10sent <- page_word_count %>%
  inner_join(get_sentiments(lexicon = 'bing')) %>%
  count(page_title, sentiment) %>%
  slice_max(n, n=10) 

top10sent%>% 
  knitr::kable()

ggplot() +
  geom_sf(data = hackneyshp, color = 'black', fill = "antiquewhite") + 
  geom_sf(data = top10sent, aes(size = n, color = n, shape = sentiment )) + 
  scale_shape(name = "Sentiment Frequency")+ 
  scale_size(name = 'Sentiment')+
  geom_sf_text(data = top10sent, aes(label = page_title), size = 3, 
               color = 'brown', check_overlap = T)+
  theme(legend.title = element_text(size = 8), 
        legend.text  = element_text(size = 6))+
  labs(title = 'Top 10 Pages with Highest Positive and Negative Words')


# 21_July_2005_London_bombings  has the highest number of positive words
# Followed by Clapton_Pond, Petchey_Academy, Haggerston_Park. The aforementioned 
#page titles have unique number of positive and negative words while other don't have. 

#top 10 ranked pages with highest number of positive and negative words 
top10sent%>% 
  ggplot(aes(page_title, n, color = sentiment, shape = sentiment)) +
  geom_point(size = 3)+ 
  labs(title = 'Top 10 Pages with Highest Positive and Negative Words', 
       x = 'Page Title', y = 'Word Frequency')+
  coord_flip()+ 
  theme(axis.text.y = element_text(angle = 30, hjust = 1, size = 6))

#Top 10 positive and negative words on all Hackney pages
sentimnet_wd <- page_word_count %>%
  inner_join(get_sentiments(lexicon = 'bing')) 

sum(sentimnet_wd$n)
#There are a total of 483 sentiment words in all the pages

page_word_count %>%
  inner_join(get_sentiments(lexicon = 'bing')) %>% 
  count(word, sentiment) %>% 
  group_by(sentiment) %>% 
  summarise(sum(n)) %>% 
  knitr::kable()
##There are 159 negative words and 272 positive words in all Hackney Pages

# page_word_count %>%
#   inner_join(get_sentiments(lexicon = 'bing')) %>% 
#   count(word, sentiment) %>% 
#   acast(word ~ sentiment, value.var = "n", fill = 0) %>%
#   comparison.cloud(scale=c(3, 1), max.words = 60)


page_word_count %>%
  inner_join(get_sentiments(lexicon = 'bing')) %>% 
  count(word, sentiment) %>% 
  group_by(sentiment) %>% 
  summarise(sum = sum(n))%>% 
  ggplot(aes(sentiment, sum, fill= sentiment))+
  geom_col()+
  theme_bw()+
  labs(title = 'Word Sentiment Frequency of all Hackney Pages')

#Sentiment word count
top10sent_word <- page_word_count %>%
  inner_join(get_sentiments(lexicon = 'bing')) %>% 
  count(word, sentiment) %>% 
  slice_max(n, n=10)

sent_word <- page_word_count %>%
  inner_join(get_sentiments(lexicon = 'bing')) %>% 
  count(word, sentiment)

top10sent_word %>% 
  knitr::kable()


#Top 60 sentimental words cloud map
sent_word %>% 
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(scale=c(3, 1), max.words = 60)

# The size of the words in the Cloud Map above is based on the frequency of the word per its sentiments. 
# It shows us the most used positive and negative words. However, we cannot compare the size across the sentiments. 

library(ggrepel)

ggplot() +
  geom_sf(data = hackneyshp, color = 'black') + 
  geom_sf(data = top10sent, aes(color = n, size = n, shape = sentiment )) + 
  scale_size(name = "Word Frequency")+ 
  scale_color_steps(name = 'Word Frequency')+
  theme_bw()+
  labs(title = 'Top 10 Word Frequency per Page with Their Sentiment')+
  geom_sf_text(data = top10sent, aes(label = page_title), size = 3, 
               color = 'black', check_overlap = T)+ 
  theme_void()



LDA()


https://www.tidytextmining.com/tfidf.html
https://slcladal.github.io/topicmodels.html
https://towardsdatascience.com/beginners-guide-to-lda-topic-modelling-with-r-e57a5a8e7a25
https://slcladal.github.io/textanalysis.html
https://yalagiants.netlify.app/2019/07/log-odds-ratio-vs-tf-idf-vs-weighted-log-odds/




  
  # Machine learning with natural language is faced with one major hurdle – its algorithms 
  # usually deal with numbers, and natural language is, well, text. So we need to transform 
  # that text into numbers, otherwise known as text vectorization. It’s a fundamental step in 
  # the process of machine learning for analyzing data, and different vectorization algorithms 
  # will drastically affect end results, so you need to choose one that will deliver the results 
  # you’re hoping for.



# For example, as an English reader/speaker, you won’t be surprised that all the 
# authors you are trying to compare use “of the” and “said the” in their respective 
# documents, and we want to know who used much more than others. tf-idf will not be 
# able to detect this, because the idf of words appearing in every document will 
# always be zero.

# # To explore how log odds ratios work, let’s first take a look at bastardy examinations. 
# # The higher the log odds ratio, the more specific the word is to that examination type, 
# # compared to the other types.
# # Note the differences in the log odds ratio scales, remembering that the higher the scores,
# # the more distinctive the words are to that particular sub-corpus.
# The bastardy examinations have the highest scores overall, which suggests to me that they 
# have a middling-sized range of very specific words. One thing worth noting, I think, is 
# the high presence of “is”, “be” and “are”, compared to “was” (and maybe “has”) in settlement 
# exams. 



## Sentiment Difference Analysis: Positive - Negative
diff_sentiment <- page_word_count %>%
  inner_join(get_sentiments(lexicon = 'bing')) %>%
  count(page_title, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(diff_sent = positive - negative) %>% 
  arrange(diff_sent)

max_diff_sentiment <- diff_sentiment %>% 
  slice_max(diff_sent, n=10) 

max_diff_sentiment %>% 
  knitr::kable(caption = "Sentiment Table with no. (+ve) - (-ve)")

min_diff_sentiment <- diff_sentiment %>% 
  slice_min(diff_sent, n=10) 

min_diff_sentiment %>% 
  knitr::kable(caption = "Sentiment Table with no. (+ve) - (-ve)")

binded_sent <- bind_rows(max_diff_sentiment, min_diff_sentiment)

binded_sent %>%
  ggplot(aes(diff_sent, page_title)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        plot.title = element_text(size=9), 
        axis.text.y = element_text(size=7, angle = 20)) +
  ylab("Contribution to sentiment")+
  labs(title = 'Hackney Top 10 and Bottom 10 Pages Sentiment Difference')


sent_pg_count %>% 
  slice_max(n, n=50) %>% 
  subset(n >= 5) %>% 
  mutate(nn = ifelse(sentiment == 'positive',n, -n )) %>% 
  ggplot(aes(reorder(page_title, nn), nn, fill = sentiment))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6), 
        plot.title = element_text(size=17)) +
  ylab("Sentiment Frequency")+
  xlab("Page Title")+
  labs(title = 'Hackney Top 50 Pages with Sentiments')
  

ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  filter(n >= 150) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Contribution to sentiment")
  

#Topic Modelling
page_tm_mt <- page_word_count %>% 
  cast_dtm(page_title, word, n)


#creating 5 topics from the page topic model  
latent <- LDA(page_tm_mt, k = 3, control = list(seed = 1234))

summary(latent)
latent

#Word-Topic Probability
wd_tp_mdl <- tidy(latent, matrix = 'beta')

wd_tp_mdl

summary(wd_tp_mdl)

top_5_wd_tp_mdl <- wd_tp_mdl %>% group_by(topic) %>%
  slice_max(beta, n = 5) %>% ungroup() 

bottom_5_wd_tp_mdl <- wd_tp_mdl %>% group_by(topic) %>%
  slice_min(beta, n = 5) %>% ungroup()

top_5_wd_tp_mdl %>%
  ggplot(aes(reorder_within(term, beta, topic), beta, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()+ 
  theme_bw()+ 
  ylab("Word within Topic")+
  xlab("Word Probability Score")+
  labs(title = 'Top 5 Words Within Topic Probability Modelling')

#Topic 1 is likely related to city centre, Topic 2: area division, Topic 3: Public Facilities

bottom_5_wd_tp_mdl %>%
  ggplot(aes(reorder_within(term, beta, topic), beta, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()+ 
  theme_bw()+ 
  ylab("Word within Topic")+
  xlab("Word Probability Score")+
  labs(title = 'Bottom 5 Words Within Topic Probability Modelling')


#Page_title-Topic Probability 
#Word-Topic Probability
pt_tp_mdl <- tidy(latent, matrix = 'gamma')

pt_tp_mdl
#The result shows that most of the documents were drawn from a mix of the topics. 
#However, documents Clapton_Pond and Church_of_St_John-at-Hackney seems to be 
#completely drawn from topic 1. 

