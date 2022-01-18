renv::activate("..")

library(tidyverse)
library(here)
library(glue)
library(bipartite)

source(here("src", "mutualist-helpers.R"))

n_k <-  40

topic_model <- read_rds(here("output", glue("topic_model_{n_k}.rds")))
communities <- read_csv(here("output","communautes_geoSBM.csv"))
enron_social <- read_csv(here("output", "enron_social.csv"))
corpus_dfm <- read_rds(here("output","corpus_dfm.rds"))


email_communities <- add_com_sender_receiver(enron_social, communities[c("communaute", "name")])


# make mat pol ------------------------------------------------------------


mat_pol <- get_pp_mat(topic_model, email_communities)


# fig 3A ------------------------------------------------------------------


mat_pol_binary <- make_enron_mat_binary(mat_pol)

Iobs <- nested(t(mat_pol_binary), method = "NODF")
nulls <- nullmodel(web=t(mat_pol_binary), N=1000, method = "r2d") # takes a while!
Inulls <- sapply(nulls, function(x) nested(x, method = "NODF"))

plot_nodf(Inulls)
ggsave(here("figs", "fig3a.png"), width = 5)


# fig 3B ------------------------------------------------------------------


# Dark, median and light grey lines refer to 
# exponential, power and truncated power law, respectively
colnames(mat_pol) <- glue("com. {colnames(mat_pol)}")
rownames(mat_pol) <- glue("topic {rownames(mat_pol)}")
png(here("figs", "fig3b.png"), width = 800, height = 400)
p_deg_dist <- plotModuleWeb(computeModules(t(mat_pol)), labsize = 1)
title(main = "Module Enron", line = -1.5)
dev.off()

# fig 3C ------------------------------------------------------------------

png(here("figs", "fig3c.png"), width = 500, height = 400)
table_degree_dist <-  degreedistr(t(mat_pol),level = "higher")
dev.off()

# fig 4C ------------------------------------------------------------------


table_gt <- as_tibble(table_degree_dist) %>%
  round(., digits = 5) %>% 
  as_tibble(rownames = "Cum. Dist") %>%
  arrange(AIC) %>% gt::gt() 

gt::gtsave(table_gt, here("figs", "fig3d.png"))

