renv::activate("..")

library(patchwork)
library(tidytext)
library(tidyverse)
library(forcats)
library(here)
library(glue)

source(here("src", "mutualist-helpers.R"))

n_k <-  40

topic_model <- read_rds(here("output", glue("topic_model_{n_k}.rds")))
corpus_dfm <- read_rds(here("output", "corpus_dfm.rds"))
enron_semantic <- read_csv(here("output", "enron_semantic.csv"))
communities <- read_csv(here("output","communautes_geoSBM.csv"))
enron_social <- read_csv(here("output", "enron_social.csv"))


email_communities <- add_com_sender_receiver(enron_social, 
                                             communities[c("communaute", "name")])


plot_pp_mat <- function(pp_mat) {
  
  marginal_topic = tibble(topic=rownames(pp_mat), value=rowSums(pp_mat)) %>%
    arrange(-value)
  marginal_com = tibble(community=colnames(pp_mat), value=colSums(pp_mat)) %>%
    arrange(-value)
  
  order_mat <- bipartite::sortweb(pp_mat)
  order_community <- colnames(order_mat)
  order_topic <- rownames(order_mat)
  
  p <- as_tibble(order_mat, rownames = "topic") %>%
    pivot_longer(-topic, names_to = "community", values_to = "# interactions") %>%
    mutate(
      community = fct_rev(
        factor(community, levels = order_community)),
      topic = factor(topic, levels = order_topic)) %>%  
    ggplot(aes(x = topic, y = community, fill = `# interactions`)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "midnightblue") +
    theme_bw() +
    theme(axis.text.x = element_text(size=15, angle = 90), 
          axis.text.y = element_text(size=15), 
          legend.position = "left") +
    labs(x="Topic", y="Community")
  
  
  p_marginal_topic <- marginal_topic %>%
    ggplot(aes(x=reorder(topic, -value), y=value)) +
    geom_col(color = "black", fill = "#f4a261") +
    theme_minimal() +
    theme(axis.text.y = element_blank(), axis.text.x = element_blank()) +
    xlab(NULL) + ylab(NULL) + 
    theme_void()
  
  p_marginal_com <- marginal_com %>%
    ggplot(aes(x=reorder(community, value), y=value)) +
    geom_col(color = "black", fill = "#2a9d8f") +
    theme_minimal() +
    ylab(NULL) + xlab(NULL) +
    theme(axis.text.y = element_blank(), axis.text.x = element_blank()) +
    coord_flip() + 
    theme_void() 
  
  wrap_plots(p, p_marginal_topic, p_marginal_com) + 
    patchwork::plot_layout(ncol = 2, nrow = 2, design = "B#
                                                         AC",
                           widths = c(3,1), heights = c(1,1.5))
}


mat_pol <- get_pp_mat(topic_model, email_communities)

plot_pp_mat(mat_pol)

ggsave(here("figs", "fig2.png"), width = 13, heigh = 7)

