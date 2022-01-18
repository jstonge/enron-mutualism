
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidytext)
library(ggthemes)

library(dplyr)
library(magrittr)
library(purrr)
library(stm)
library(quanteda)



get_dfm <- function(tidy_df) {
  tidy_df %>% 
    count(message_id,word,sort=TRUE) %>% 
    cast_dfm(message_id,word,n)
}


#
# get_top_terms:
# input: tbl_beta as prod by tidytext::tidy(fit_stm, "beta")
# return: a tbl containing top 20 terms for a given topic
#
get_top_terms <- function(tbl_beta) {
  top_terms <- tbl_beta %>%
    arrange(beta) %>%
    group_by(topic) %>%
    top_n(5, beta) %>%
    arrange(-beta) %>%
    select(topic, term) %>%
    summarise(terms = list(term)) %>%
    mutate(terms = map(terms, paste, collapse = ", ")) %>%
    unnest(cols = c(terms))
  
  return(top_terms)
}

#
# get gamma terms
# input: tbl_gamma as prod by tidytext::tidy(fit_stm, "gamma")
#        and top_terms as prod by the fun `get_top_terms`
# return: a tbl containing top gamma terms and associated topics
#
get_gamma_terms <- function(tbl_gamma, top_terms) {
  prep_gamma <- tbl_gamma %>%
    group_by(topic) %>%
    summarise(gamma = mean(gamma)) %>%
    arrange(desc(gamma)) %>%
    left_join(top_terms, by = "topic") %>%
    mutate(topic = paste0("Topic ", topic),
           topic = reorder(topic, gamma))
  
  return(prep_gamma)
}

#
# get_list_topics: helper fun to get a list of topics to plot
# input: modelCorr object as prod by STM::topicCorr fun and a threshold
# return: list of relevant topics to provide to `plot_graph`
#
get_list_topics <- function(modelCorr, threshold) {
  
  relevant_edges <- as_tbl_graph(modelCorr$cor) %>%
    activate(edges) %>%
    filter(weight > threshold) %>%
    as_tibble() %>%
    filter(from != to)
  
  relevant_from <- relevant_edges %>%
    distinct(from)
  
  relevant_to <- relevant_edges %>%
    distinct(to) %>% rename(from=to)
  
  list_topics <- relevant_from %>%
    bind_rows(relevant_to) %>%
    distinct()
  
  return(list_topics)
}

#
# get_k_result: borrowed from https://juliasilge.com/blog/evaluating-stm/
# input: data_sparse as prod by tidytext::cast_sparse()
# return: a nested tibble with information on various model diagnostic scores
#
get_k_results <- function(many_models) {
  
  heldout <- make.heldout(corpus_dfm)
  many_models %>%
    mutate(exclusivity = map(topic_model, exclusivity),
           semantic_coherence = map(topic_model, semanticCoherence, corpus_dfm),
           eval_heldout = map(topic_model, eval.heldout, heldout$missing),
           residual = map(topic_model, checkResiduals, corpus_dfm),
           bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
           lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
           lbound = bound + lfact,
           iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))
  
}

#
# get_tbl_modelRelated_topic
# input: fit STM object
# return: a tibble containing all topics containing models related terms
#         that include both FREX and most frequent terms
#
get_tbl_modelRelated_topic <- function(fit) {
  
  label_t <- labelTopics(fit, n = 20)
  tbl_modelRelated_topic <- tibble()
  
  for (i in 1:dim(label_t$prob)[1]) {
    list_terms <- unique(append(label_t$prob[i,], label_t$frex[i,]))
    tmp <- tibble(term = list_terms) %>%
      summarise(terms = list(term)) %>%
      mutate(terms = map(terms, paste, collapse = ", "),
             topic = i)
    if (sum(str_detect(tmp$terms, "models?|bayes|prior|posterior")) >= 1 ) {
      tbl_modelRelated_topic <- tbl_modelRelated_topic %>%
        bind_rows(tmp)
    }
  }
  return (tbl_modelRelated_topic)
}

#
# get_tbl_topic_all_terms
# inputs: stm fit object
# return: a tibble that contains the different scores from STM
#
get_tbl_topic_all_terms <- function(fit) {
  
  label_t <- labelTopics(fit, n = 20)
  tbl_topic_all_terms <- tibble()
  
  for (i in 1:dim(label_t$prob)[1]) {
    
    list_terms_prob <- label_t$prob[i,]
    list_terms_frex <- label_t$frex[i,]
    list_terms_lift <- label_t$lift[i,]
    list_terms_score <- label_t$score[i,]
    
    tmp <- tibble(prob_term = list_terms_prob,
                  frex_term = list_terms_frex,
                  lift_term = list_terms_lift,
                  score_term = list_terms_score) %>%
      summarise(
        Prob_terms = list(prob_term),
        Frex_terms = list(frex_term),
        Lift_terms = list(lift_term),
        Score_terms = list(score_term)
      ) %>%
      mutate(
        topic = i,
        Prob_terms = map(Prob_terms, paste, collapse = ", "),
        Frex_terms = map(Frex_terms, paste, collapse = ", "),
        Lift_terms = map(Lift_terms, paste, collapse = ", "),
        Score_terms = map(Score_terms, paste, collapse = ", ")
      )
    
    tbl_topic_all_terms <- tbl_topic_all_terms %>%
      bind_rows(tmp)
  }
  return (tbl_topic_all_terms)
}

#
# get_top_terms_by_topics
# input: tibble gamma as output by `tidy` from tidytext and topic K
# return: tibble with top 50 terms in topic K
#
get_top_terms_by_topic <- function(tbl_gamma, k) {
  top_terms <- tbl_gamma %>%
    mutate(topic = str_remove_all(topic, "Topic ")) %>%
    filter(topic == k) %>%
    arrange(-gamma) %>%
    top_n(50, gamma)
  return(top_terms)
}



################################ PLOTTING FUNS #################################

plot_quadrant_diagnostic <- function(k_results) {
  k_results %>%
    transmute(K,
              `Lower bound` = lbound,
              Residuals = map_dbl(residual, "dispersion"),
              `Semantic coherence` = map_dbl(semantic_coherence, mean),
              `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
    gather(Metric, Value, -K) %>%
    ggplot(aes(K, Value, color = Metric)) +
    geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
    facet_wrap(~Metric, scales = "free_y") +
    labs(x = "K (number of topics)",
         y = NULL,
         title = "Model diagnostics by number of topics",
         subtitle = "Enron (by individual)") +
    theme_bw()
}


plot_exclusivity_coherence_tradeoff <- function(k_results, ks) {
  k_results %>%
    select(K, exclusivity, semantic_coherence) %>%
    filter(K %in% ks) %>%
    unnest(cols = c(exclusivity, semantic_coherence)) %>%
    mutate(K = as.factor(K)) %>%
    ggplot(aes(semantic_coherence, exclusivity, color = K)) +
    geom_point(size = 2, alpha = 0.7) +
    labs(x = "Semantic coherence",
         y = "Exclusivity",
         title = "Enron semantic coherence-exclusivity tradeoff") + 
    theme_bw()
  
}


#
# plot_prevalence_w_top_words
#
plot_prevalence_w_top_words <- function(tbl_gamma) {
  tbl_gamma %>%
    top_n(30, gamma) %>%
    ggplot(aes(topic, gamma, label = terms, fill = as.factor(topic))) +
    geom_col(alpha = .8, color = "black",
             fill = "midnightblue", show.legend = FALSE) +
    geom_text(hjust = 0, nudge_y = 0.0005, size = 3,
              family = "IBMPlexSans") +
    coord_flip() +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, 0.14),
                       labels = scales::percent_format()) +
    theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
    theme(plot.title = element_text(size = 16,
                                    family = "IBMPlexSans-Bold"),
          plot.subtitle = element_text(size = 13)) +
    scale_fill_grey() +
    labs(x = NULL, y = expression(gamma),
         title = "Topic prevalence in the Enron corpus",
         subtitle = "With the top words that contribute to each topic")
}

#
# plot_top_words_by_mixtures
# input: tidy beta as output from tidytext, how the scales should behave
#        for facet_wrap and the list of topics of interests
#
plot_top_words_by_mixtures <- function(tbl_beta, scales, list_topics) {
  tbl_beta %>%
    filter(topic %in% list_topics,
           !term %in% c("species", "time", "models?", "size")) %>%
    group_by(topic) %>%
    top_n(10) %>%
    ungroup %>%
    mutate(topic = as.factor(topic),
           term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = topic)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~topic, scales = scales) +
    coord_flip() +
    scale_x_reordered() +
    scale_y_continuous(expand = c(0,0))
}

#
# plot_prevalence_tidy
# input: prep_stm as produced by stm::estimateEffect, stm_fit, scales to determine
#        if facet_wrap has free axis, and tbl with topic labels
#
plot_prevalence_tidy <- function(prep_stm, stm_fit,
                                 scales, label_tbl = FALSE) {
  neworder = prep_stm$topics
  if (label_tbl == FALSE) {
    # tidy topic prevalence
    tmp <- extract.estimateEffect(prep_stm, "pub_year", model = stm_fit,
                                  method = "continuous") %>%
      mutate(label = factor(topic, levels=neworder))
  } else {
    tmp <- extract.estimateEffect(prep_stm, "pub_year", model = stm_fit,
                                  method = "continuous") %>%
      select(-label) %>%
      left_join(label_tbl, by = "topic")
  }
  
  tmp %>%
    ggplot(aes(x = covariate.value, y = estimate,
               ymin = ci.lower, ymax = ci.upper,
               fill = "mignightblue")) +
    facet_wrap(~label, scales = scales) +
    geom_ribbon(alpha = .5, show.legend = FALSE) +
    geom_line(show.legend = FALSE) +
    labs(x = NULL, y = "Expected Topic Proportion") +
    theme_bw() +
    scale_fill_grey()
}

#
# plot_graph
#
# input: corr_df, list_topics(how many topics), which_topics(output get_list_topics fun),
#        threshold, title
# returns: plot where edges represents how likely two topics appear together in a doc
#          and only filtered nodes, given threshold, appear
#
plot_graph <- function(stm_corr_fit, list_topics, which_topic, threshold, title) {
  as_tbl_graph(stm_corr_fit$cor) %>%
    mutate(topic = which_topic,
           is_relevant = ifelse(topic %in% list_topics$from, TRUE, FALSE)) %>%
    activate(edges) %>%
    filter(weight > threshold) %>%
    ggraph("kk") +
    geom_edge_link(aes(edge_colour = weight)) +
    ggtitle(title) +
    scale_edge_colour_viridis() +
    geom_node_point(aes(filter = is_relevant == TRUE)) +
    geom_node_label(aes(label = topic, filter = is_relevant == TRUE),
                    repel = T) +
    theme_graph(title_size = 11)
}

#
# calc_p1: subsidiary fun for plot_summary
#
calc_p1 <- function(stm_fit, covar) {
  td_beta <- tidy(stm_fit, "beta")
  td_gamma <- tidy(stm_fit, "gamma", document_names = covar$section_id)
  top_terms <- get_top_terms(td_beta)
  gamma_terms <- get_gamma_terms(td_gamma, top_terms)
  p1 <- plot_prevalence_w_top_words(gamma_terms)
  return(p1)
}

#
# plot_regroupement_mixture
#
plot_regroupement_mixture <- function(g, i, stm_fit) {
  eg <- g %>% as_tibble() %>%
    filter(edge_group == i) %>%
    distinct(from)
  p <- plot_top_words_by_mixtures(tidy(stm_fit),
                                  scales = "free_y",
                                  eg$from) +
    ggtitle(paste0("Regroupement ", i))
  
  return(p)
}

#
# plot_node_rank
# input: g as prod by get_graph_node
#
plot_node_rank <- function(g) {
  
  g %>%
    ggraph('matrix', sort.by = node_rank_hclust()) +
    geom_edge_point(aes(colour = edge_group),
                    mirror = TRUE) +
    coord_fixed() + theme_graph() +
    labs(subtitle=(paste0("algorithm = ",
                          igraph::V(g)$algo[[1]])))
}

#
# plot_graph_node_rank
# input: g as prod by get_graph_node
#
plot_graph_node_rank <- function(g, layout) {
  g %>%
    ggraph(layout) +
    geom_node_point(aes(size = cent_bet,
                        colour = group),
                    show.legend = FALSE) +
    geom_node_text(aes(label = name)) +
    geom_edge_link(aes(alpha = weight,
                       colour = edge_group)) +
    theme_graph() +
    labs(subtitle=paste0("algorithm = ",
                         igraph::V(g)$algo[[1]]))
}



