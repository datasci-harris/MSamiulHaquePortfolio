#Author: M Samiul Haque Prantar


#Libraries -----------------------------------------------------

library(tidyverse)
library(rvest)
library(tidytext)
library(nycflights13)
library(plyr)

#SHINY APP -----------------------------------------------------

#The shiny app I designed can be accessed through the link below

"https://samiul1392.shinyapps.io/Growth_ODA_Health/"


#DATA CLEANING -----------------------------------------------------

who_clean <- who %>%
  pivot_longer(
    cols = new_sp_m014:newrel_f65, 
    names_to = "key", 
    values_to = "cases", 
    values_drop_na = TRUE
  ) %>% 
  mutate(
    key = stringr::str_replace(key, "newrel", "new_rel")
  ) %>%
  separate(key, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)

who_soph <- who_clean %>% 
  filter(year >= 1997) %>%
  pivot_wider(
    names_from = sex, 
    values_from = cases,
  ) %>% 
  drop_na() %>% 
  group_by(year) %>% 
  dplyr::summarise(total_men = sum(m), total_women = sum(f)) %>%
  mutate(prop_of_men = total_men/(total_men + total_women), 
         prop_of_women = total_women/(total_men + total_women)) %>% 
  pivot_longer(
    prop_of_men:prop_of_women,
    names_to = "gender",
    values_to = "Prop"
  )


ggplot(who_soph, aes(x = as.factor(year), y = Prop)) + 
  geom_bar( aes(fill = gender), position = 'stack', stat = "identity") + 
  theme_minimal() + 
  labs(title = "Men vs. Women - TB Cases", x = 'year') + 
  theme( plot.caption = element_text(hjust = 0, face = "italic"), 
         plot.title = element_text(hjust = .5, face = "bold"), 
         axis.text = element_text(size = 7), 
         panel.grid.major = element_blank()) 


who_2 <- who_clean %>% 
  group_by(country, year, sex) %>%
  dplyr::summarise(total_tb = sum(cases)) %>%
  group_by(country, year) %>% 
  pivot_wider(
    names_from = sex, 
    values_from = total_tb) 


who_2 %>% 
  dplyr::mutate(total_tb = sum(f,m)) %>%
  filter(year >= 1997, total_tb > 50000) %>%
  ggplot(aes(x = year)) + 
  geom_line(aes(y = f), color = "red") + 
  geom_line(aes(y = m),color = "blue") +
  facet_wrap(vars(country), nrow = 5) +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        panel.border = element_rect(fill = FALSE)) +
  labs(title = "Cases of over 50,000 in different countries", 
       subtitle = "Male cases in blue, Female cases in red", 
       y = "cases", x =    "year") +
  xlim(c(2004, 2013)) + 
  ylim(c(0, 7e+05))


#REGEX -----------------------------------------------------

phone <- regex("
  \\(?    
  (\\d{5}) 
  [) -]?   
  (\\d{3}) 
  [ -]?    
  (\\d{3}) 
  ", comments = TRUE)

str_match(c("01781-333-546", "01810-244-596", "01678-012-663"), phone)

#SPATIAL PLOTTING -----------------------------------------------------

delay_df_jun <- flights %>% 
  filter(year == 2013, day == 13, month ==6) %>%
  group_by(dest) %>% 
  dplyr::summarise("Delays on Jun 13, 2013" = mean(arr_delay, na.rm = TRUE)) %>%
  left_join(airports, c("dest" = "faa")) %>% 
  filter(!tzone == "Pacific/Honolulu", !tzone == "America/Anchorage") 

delay_df_gen <- flights %>% 
  group_by(dest) %>% 
  dplyr::summarise('Average Delay 2013' = mean(arr_delay, na.rm = TRUE)) %>% 
  inner_join(delay_df_jun, by = "dest") %>% 
  pivot_longer(
    'Average Delay 2013':'Delays on Jun 13, 2013', names_to = "delay_type", values_to = "delay")


ggplot(delay_df_gen, aes(lon, lat)) +
  borders("state") +
  geom_point(size = delay_df_gen$delay/12, color = "maroon", alpha = .5) + 
  coord_quickmap() + 
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = .5, face = "bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())  +  
  facet_wrap(vars(delay_type)) + 
  labs(title = "Storms Cause Major Flight Delays across Eastern US")

#NATURAL LANGUAGE PROCESSING AND WEB SCRAPING -----------------------------------------------------


url_1 <- "https://www.bis.org/publ/qtrpdf/r_qt1912a.htm"
url_2 <- "https://www.bis.org/publ/qtrpdf/r_qt2012a.htm"

sentiment_analysis <- function(url){
  request <- read_html(url)
  
  article <- html_nodes(request, "#cmsContent")
  paragraphs <- html_nodes(article, "p")
  text_list <- html_text(paragraphs)
  text_df <- tibble(text = text_list)
  
  
  word_token_df <- unnest_tokens(text_df, word_token, text, token = "words")
  
  no_sw_df <- anti_join(word_token_df, stop_words, by = c("word_token" = "word"))
  
  for(s in c("nrc", "afinn", "bing")){ 
    no_sw_df <- no_sw_df %>%
      left_join(get_sentiments(s), by = c("word_token" = "word")) %>%
      rename(replace = c(sentiment = s, value = s), warn_missing = FALSE)
  }
  
  
  p1 = ggplot(filter(no_sw_df, !is.na(afinn))) + 
    geom_histogram(aes(afinn), stat = "count")+ 
    theme_minimal() + 
    theme(panel.grid = element_blank()) +
    labs(title = "Sentiment of BIS Article", subtitle = "Using AFINN Library")
  
  p2 = ggplot(filter(no_sw_df, !is.na(nrc))) + 
    geom_histogram(aes(nrc), stat = "count")+ 
    theme_minimal() + 
    theme(panel.grid = element_blank(),
          axis.text =  element_text(angle = 90, size = 8)) +
    labs(title = "Sentiment of BIS Article", subtitle = "Using NRC Library")
  
  p3 = ggplot(filter(no_sw_df, !is.na(bing))) + 
    geom_histogram(aes(bing), stat = "count", binwidth = 5)+ 
    theme_minimal() + 
    theme(panel.grid = element_blank()) +
    labs(title = "Sentiment of BIS Article", subtitle = "Using BING Library")
  
  
  ggpubr::ggarrange(p1, p2, p3)
  
}

sentiment_analysis(url_1)

sentiment_analysis(url_2)


