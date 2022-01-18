library(tidyverse)
library(patchwork)
library(tidytext)
library(here)
library(bipartite)
library(tidygraph)
library(glue)

source(here("src", "mutualist-helpers.R"))

n_k <-  40

corpus_dfm <- read_rds(here("output", "corpus_dfm.rds"))
enron_semantic <- read_csv(here("output", "enron_semantic.csv"))
enron_social <- read_csv(here("output", "enron_social.csv"))
communities <- read_csv(here("output","communautes_geoSBM.csv"))


email_communities <- add_com_sender_receiver(
  enron_social, communities[c("communaute", "name")]
)


# enron plant-pol matrix --------------------------------------------------


mat_pol <- get_pp_mat(topic_model, email_communities)

# Statistiques de email_communities mat_pol
max(max(mat_pol)) #= 1218
min(min(mat_pol)) #= 0.
sum(sum(mat_pol)) #= 13387
mean(colMeans(mat_pol)) #= 28
median(as.matrix(mat_pol)) #= 8.16


# Mutualist measures ------------------------------------------------------


calc_summary_table <- function(mat) {
    tmp_dd <- networklevel(t(mat), index=c("degree distribution"))$`degree distribution.HL` %>% as_tibble()
    
    tibble(nb_plants   = nrow(mat), 
           nb_animals  = ncol(mat), 
           ratio_ap    = nb_animals/nb_plants,
           connectance = networklevel(mat, index = c("connectance")),
           estimate_dd = tmp_dd$Estimate[3],   # dd -> degree dist
           stderror_dd = tmp_dd$`Std. Error`[3],
           pr_dd       = tmp_dd$`Pr(>|t|)`[3],
           r2_dd       = tmp_dd$R2[3],
           aic_dd      = tmp_dd$AIC[3],
           nodf = nested(mat,method = "NODF"),                 # nestedness
           lh = computeModules(mat)@likelihood, # mod1
           nos = NOS(mat)$mod)                                 # mod2
}

# Summary table
enron_mat_summary <- calc_summary_table(mat_pol)

# Calculate degree distribution
degreedistr(t(mat_pol), level = "higher") %>% round(5)

# Calculate nestedness
## First make enron matrix binary
enron_binary <- make_enron_mat_binary(mat_pol)

## Then run NODF
Iobs <- nested(t(enron_binary), method = "NODF")
nulls <- nullmodel(web=t(enron_binary), N=2000, method = "r2d") # takes a while!
Inulls <- sapply(nulls, function(x) nested(x, method = "NODF"))

## Glance plot
plot_nodf(Inulls)


