
get_top_words <- function() {
  summary_stm <- summary(topic_model)
  
  tibble(topic_nb = 1:n_k) %>% 
    bind_cols(as_tibble(summary_stm$prob) %>% 
                unite("prob", c(V1:V6), sep=", "),
              as_tibble(summary_stm$frex) %>% 
                unite("frex", c(V1:V6), sep=", "))
}


plot_prevalence_w_top_words <- function() {
  gamma_terms %>%
    ggplot(aes(topic, gamma)) +
    geom_col(alpha = .8, color = "black", show.legend = FALSE) +
    ggtext::geom_richtext(aes(label=glue("<span style='color:#540b0e'>**PROB**:{prob}</span> / <span style='color:#335c67'>**FREX**:{frex}</span>")), fill = NA,
                          label.color = NA, hjust = 0, nudge_y = 0.0005, size = 7,
                          family = "IBMPlexSans", show.legend = FALSE) +
    coord_flip() +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, 0.30),
                       labels = scales::percent_format()) +
    theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
    theme(plot.title = element_text(size = 18, hjust = 0.5,
                                    family = "IBMPlexSans-Bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          axis.text.y = element_text(size=15),
          axis.text.x = element_text(size=15),
    ) +
    scale_fill_grey(start=0.6, end=0.4) +
    labs(x = NULL, y = expression(gamma),
         title = "Topic prevalence in the Enron corpus",
         subtitle = "With the top words that contribute to each topic")
}


plot_graph <- function(topic_model, thresh = 0.05) {
  n_k <- topic_model$settings$dim$K
  topic_cor <- stm::topicCorr(topic_model)
  g <- as_tbl_graph(topic_cor$cor, directed=FALSE) %>%
    activate("edges") %>% 
    filter(weight > 0.05) %>% 
    activate("nodes") %>% 
    mutate(topic = as.character(1:n_k),
           gr = group_infomap())
  
  nb_group <- g %>% pull(gr) %>% unique() %>% length()  
  p <- ggraph(g, "fr") +
    geom_edge_link(aes(alpha = weight)) +
    # geom_node_point() +
    # scale_edge_colour_viridis() +
    geom_node_text(aes(label = topic, color = factor(gr)), size = 12, 
                   show.legend = FALSE) 
  
  
  if (nb_group == 7) {
    p <- p + scale_fill_manual(values = c("#6587cd", "#64ac48","#ab62c0","#a39440",
                                          "#c95676","#4aac8d","#ce6b3e"))
  }
  p + theme_graph(title_size = 11) + 
    theme(plot.title = element_text(size = 18, hjust = 0.5,
                                    family = "IBMPlexSans-Bold"), 
          legend.title =  element_text(size=15) , legend.text = element_text(size=15))
}


calc_ratio <- function(df) {
  round(nrow(filter(df, animales>plantes)) / nrow(df) * 100,2)
}

get_marginals <- function(x) {
  tmp <- read_csv(here(DIR_web_of_life, x))
  tibble(plantes=nrow(tmp), animales=ncol(tmp)) 
}

plot_sim_web <- function(tbl, col, vline) {
  ggplot(tbl, aes({{ col }})) +
    geom_histogram(aes(fill = type)) +
    geom_vline(xintercept = vline, colour = "black") +
    annotate(
      geom = "text", 
      x = vline - 0.04,  y = 10, angle = 90,
      label = "Observed value Enron"
    ) +
    scale_fill_brewer(palette = "Dark2") +
    theme_bw()
}

run_conn <- function(x) {
  tmp <- read_csv(here(DIR_web_of_life, x))
  tibble(connectance=networklevel(tmp, index=c("connectance"))) 
}


get_dd <- function(x) {
  tmp <- read_csv(here(DIR_web_of_life,x))
  tmp_dd <- networklevel(t(tmp), index=c("degree distribution"))$`degree distribution.HL` %>% as_tibble()
  tibble(estimate = tmp_dd$Estimate[3],
         stderror = tmp_dd$`Std. Error`[3],
         pr = tmp_dd$`Pr(>|t|)`[3],
         r2 = tmp_dd$R2[3],
         aic = tmp_dd$AIC[3])
}


get_nest <- function(x) {
  tmp <- read_csv(here(DIR_web_of_life,x))
  tibble(
    binmatnest2 = nested(tmp,method = "binmatnest2"),
    binmatnest = nested(tmp,method = "binmatnest"),
    discrepancy2 = nested(tmp,method = "discrepancy2"),
    discrepancy = nested(tmp,method = "discrepancy"),
    checker = nested(tmp,method = "checker"),
    nodf2 = nested(tmp,method = "NODF2"),
    nodf = nested(tmp,method = "NODF"),
    wnodf = nested(tmp,method = "weighted NODF"),
    wine = wine(as.data.frame(tmp))$wine,
    nos = NOS(tmp)$Nbar,
    win = wine(as.data.frame(tmp))$win
  )
}

