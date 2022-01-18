library(tidyverse)

add_com_sender_receiver <- function(data, group_df) {
  data %>% 
    select(message_id, from, to) %>% 
    left_join(group_df, by=c("from"="name")) %>% 
    left_join(group_df, by=c("to"="name")) %>% 
    rename(sender=communaute.x, receiver=communaute.y) 
}


add_community <- function(enron_semantic) {
  left_join(enron_semantic, communities, by=c("from"="nom"))  
}


get_pp_mat <- function(topic_model, email_communities) {
  tidytext::tidy(topic_model, "gamma", document_names = rownames(corpus_dfm)) %>%
    mutate(document = as.numeric(document)) %>%
    rename(message_id = document) %>%
    left_join(email_communities, by = "message_id") %>%
    mutate(gamma = ifelse(gamma > 0.1, gamma, 0)) %>%
    group_by(topic, sender) %>%
    summarise(g = sum(gamma)) %>%
    rename(com = sender) %>%
    mutate(g = ifelse(g > 10, g, 0)) %>%
    spread(com, g, fill = 0) %>%
    ungroup() %>%
    column_to_rownames("topic")
}

make_enron_mat_binary <- function(enron_mat) {
  mat_pol_binary <- matrix(data = 0, 
                           nrow = nrow(enron_mat), 
                           ncol = length(enron_mat))
  
  for (i in 0:length(enron_mat)) {
    for (j in 1:nrow(enron_mat)) {
      mat_pol_binary[j,i] = ifelse(enron_mat[j,i]==0, 0, 1)
    }
  }
  
  mat_pol_binary <- as_tibble(mat_pol_binary)
  
  colnames(mat_pol_binary) <- glue("com{colnames(mat_pol_binary)}")  
  rownames(mat_pol_binary) <- glue("topic{rownames(mat_pol_binary)}")
  
  return(mat_pol_binary)
}


plot_nodf <- function(Inulls) {
  as_tibble(Inulls) %>% 
    ggplot(aes(value)) +
    geom_histogram(fill="midnightblue", color="black", alpha=0.6, bins=70) + 
    geom_vline(xintercept = Iobs, col="red", linetype = "dashed") +
    theme_bw() + 
    ggtitle("NODF") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab(NULL) + ylab(NULL) + xlim(0,40)
  
}
