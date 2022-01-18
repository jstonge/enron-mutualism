renv::activate("..")

library(tidytext)
library(tidyverse)
library(stringr)
library(here)
library(reticulate)


use_python("~/miniconda3/envs/enron-mutualism/bin/python") # change this 
pd <- import("pandas")

source(here("src", "cleaning-helpers.R"))

if (!dir.exists(here("output"))) { dir.create(here("output")) }

# social network: employees interactions ----------------------------------


enron_social <- pd$read_pickle(here("dat","enron_social_data.pkl")) %>% as_tibble()

enron_social <- enron_social %>% 
  only_core() %>% 
  select(message_id, sender_id, receiver_id, core_sender_name, core_receiver_name, date) %>% 
  mutate(core_sender_name = as.character(core_sender_name),
         core_receiver_name = as.character(core_receiver_name),
         date = lubridate::as_date(date)) %>% 
  rename(from = sender_id, to=receiver_id)


# Semantic data -----------------------------------------------------------


corpus <- pd$read_pickle(here("dat", "enron_semantic_data.pkl")) %>% as_tibble()

# Unique emails sent between employees
enron_semantic <- get_sem_data(corpus, enron_social)

# Keep only original message, e.g. not email fwd or any other boilerplate text. 
enron_semantic$message_original <- clean_message(enron_semantic)


enron_semantic %>% 
  filter_time_window(date1 = "1999-04-01", date2 = "2002-02-01") %>% 
  select(message_id, message_original, date) %>% 
  write_csv(here("output", "enron_semantic.csv"))


# Write social data =------------------------------------------------------

## 
enron_social <- enron_social %>% 
  filter_time_window(date1 = "1999-04-01", date2 = "2002-02-01") %>% 
  filter(message_id %in% enron_semantic$message_id)
  
write_csv(enron_social, here("output", "enron_social.csv"))

## Write email social network
g <- enron_social %>% 
  count(from, to) %>% 
  tidygraph::as_tbl_graph(directed=FALSE) %>% 
  igraph::simplify(edge.attr.comb = list(n="sum")) %>% 
  tidygraph::as_tbl_graph() %>% 
  tidygraph::mutate(idx = row_number()) 

g %>% 
  as_tibble() %>%  
  write_csv(here("output", "nodes_df.csv"))

g %>% 
  tidygraph::activate("edges") %>% 
  as_tibble() %>%  
  write_csv(here("output", "edges_df.csv"))


