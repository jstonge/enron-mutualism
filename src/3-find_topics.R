renv::activate("..")

library(tidyverse)
library(tidytext)
library(stm)
library(here)
library(glue)

set.seed(02138)

source(here("src", "modeling-helpers.R"))
source(here("src", "cleaning-helpers.R"))


enron_semantic <- read_csv(here("output", "enron_semantic.csv"))

corpus_dfm <- enron_semantic %>% 
  unnest_tokens(word, message_original)  %>% 
  preprocessing_post_tidy() %>%  # 438,312 tokens
  get_dfm()                      # vocabulary 7,234 x 15,813 documents

write_rds(corpus_dfm, here("output","corpus_dfm.rds"))


# Fit many models ---------------------------------------------------------


library(doParallel)
Ks <- c(30,40,60)
registerDoParallel(length(Ks))

many_models <- foreach(k=Ks) %dopar% {
  stm(corpus_dfm, K = k,max.em.its = 75,
      verbose = TRUE, init.type = "Spectral")
}

many_models <- enframe(name=NULL, value = "topic_model", many_models) %>% 
  mutate(K = Ks)


# Model validation --------------------------------------------------------

# see https://juliasilge.com/blog/evaluating-stm/

k_results <- get_k_results(many_models)

plot_quadrant_diagnostic(k_results)
ggsave(here("figs","diagnostic1.png"))

plot_exclusivity_coherence_tradeoff(k_results, c(30,40,60))
ggsave(here("figs","diagnostic2.png"))


# Run  again selected K with more its -------------------------------------


n_k = 40
topic_model <- stm(corpus_dfm, K = n_k, max.em.its = 500, init.type = "Spectral")
write_rds(topic_model, here("output", glue("topic_model_{n_k}.rds")))


# Glimpse topics ----------------------------------------------------------


# topic_model <- read_rds(here("output", "topic_model_40.rds"))
# 
# td_gamma <- tidy(topic_model, "gamma")
# td_beta <- tidy(topic_model)
# 
# top_terms <- get_top_terms(td_beta)
# gamma_terms <- get_gamma_terms(td_gamma, top_terms)
# 
# plot_prevalence_w_top_words(gamma_terms)

