
only_core <- function(data) {
  # cores employees have names
  subset(data, core_receiver_name != 'NaN' & core_sender_name != 'NaN' & message_TO_count == 1) 
}

n_sent_emails <- function(social_tbl) {
  # nb of times an email is sent
  select(social_tbl, message_id) %>% table() %>% as_tibble()
}


filter_noise <- function(df) {
  df %>% 
    filter(str_detect(message, "^trv notification"))
}

clean_message <- function(df) {
  df$message %>% 
    str_replace_all("-{5}.*"," ") %>%               # fwd ou original message {"-{5}.*?subject:"}
    str_replace_all("(a|p)m\\s{0,10}to:.*"," ")%>%  # am to: ou pm to: {"(a|p)m\\s{0,10}to:.*?subject:"}
    str_replace_all("={1,2}20"," ") %>% #
    str_replace_all( "=","") %>% #
    str_replace_all( "enron north america corp.*"," ") %>% # signatures polluantes
    str_replace_all( "enron wholesale services.*"," ") %>% # signatures polluantes
    str_replace_all( "\\b\\S*@\\S*\\b"," ") %>%            # email
    str_replace_all( "\\d"," ") %>%                        # chiffre
    str_replace_all( "[[:punct:]]"," ") %>%                # ponctuation
    str_replace_all( "\\b(fw|re)\\b"," ") %>%              # fw ou re
    str_squish()                            
}


add_community <- function(text_data) {
  social_communities <- read_csv(here("output","communautes_SBM_sept2jan.csv"))
  left_join(text_data, email_communaute, by=c("from"="nom"))  
}

filter_time_window <- function(data, date1, date2) {
  filter(data, date >= date1 & date <= date2)
}

get_first_names <- function(path_enron_social) {
  enron_social <- read_csv(path_enron_social)
  
  c(enron_social$core_receiver_name, enron_social$core_sender_name) %>% 
    unique() %>% 
    str_split("_",simplify=TRUE) %>%
    str_c() %>% 
    enframe(name=NULL, value="word")
}

preprocessing_post_tidy <- function(tidy_df) {
  first_names <- get_first_names(here("output", "enron_social.csv"))
  
  tidy_df %>% 
    anti_join(first_names) %>%             # rm employees names
    anti_join(get_stopwords()) %>%         # rm stopwords
    filter(str_count(word) >= 2,           # rm letters
           str_count(word) < 18)  %>%      # rm too long words
    add_count(word) %>%
    filter(n > 5) %>%                      # rm unfrequent words
    select(-n)
  
}



get_sem_data <- function(corpus, social_tbl) {
  
  N <- n_sent_emails(social_tbl)
  colnames(N) <- c("message_id", "nbto")
  
  enron_social %>%
    distinct(message_id, .keep_all = TRUE) %>%
    merge(corpus, by = "message_id") %>%
    distinct(message, .keep_all = TRUE) %>%
    merge(N, by = "message_id") %>% 
    as_tibble()
}
