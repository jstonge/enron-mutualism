
library(tidyverse)
library(tidytext)
library(stm)
library(here)
library(patchwork)
library(ggtext)
library(glue)
library(ggraph)
library(bipartite)
library(tidygraph)
library(foreach)

source(here("src", "modeling-helpers.R"))
source(here("src", "mutualist-helpers.R"))
source(here("src", "sup_mat-helpers.R"))


topic_model_40K <- read_rds(here("output", glue("topic_model_40.rds")))
communities <- read_csv(here("output", "communautes_geoSBM.csv"))
enron_social <- read_csv(here("output", "enron_social.csv"))
corpus_dfm <- read_rds(here("output", "corpus_dfm.rds"))

email_communities <- add_com_sender_receiver(
  enron_social, communities[c("communaute", "name")]
)

mat_pol <- get_pp_mat(topic_model_40K, email_communities)

# fig S1 ------------------------------------------------------------------


enron_social <- read_csv(here("output", "enron_social.csv"))

# plot time series
enron_social %>% 
  count(date) %>% 
  ggplot(aes(x=date,y=n)) +
  geom_line() + 
  scale_x_date(date_labels = "%Y %b %d") +
  labs(y="email exchanges", x=NULL,title= "Enron timeline") +
  theme_bw() + 
  theme(axis.text.x = element_text(size=12))
  
ggsave(here("figs", "figS1.png"), width=12, height=5)


# fig S2 ------------------------------------------------------------------



td_gamma <- tidy(topic_model_40K, "gamma")
td_beta <- tidy(topic_model_40K)

top_terms <- get_top_terms(td_beta)
gamma_terms <- get_gamma_terms(td_gamma, top_terms)

gamma_terms <- gamma_terms %>% 
  separate("topic", c(NA, "topic_nb"), sep=" ", remove=FALSE) %>% 
  mutate(topic_nb = as.integer(topic_nb)) %>% 
  left_join(get_top_words(), by = "topic_nb")

p1 <- plot_prevalence_w_top_words()

# ggsave(here("figs", "figS2.png"), height = 20, width = 24, dpi=400)


# figS3 -------------------------------------------------------------------

set.seed(30)


p2 <- plot_graph(topic_model_40K) + ggtitle("Topic Network")


p1 / p2 + plot_layout(heights = c(2, 1))
ggsave(here("figs", "figS2.png"), height = 24, width = 22, dpi=400)


# Web-of-Life comparison --------------------------------------------------


DIR_web_of_life <- here("dat", "mutualist_studies")

p_p <- list.files(DIR_web_of_life, "M_PL*")
n_p <- length(p_p)

p_sd <- list.files(DIR_web_of_life, "M_SD*")
n_sd <- length(p_sd)


##############################################
# 1) Are there always more plants than insects?
##############################################

nrow(mat_pol)
ncol(mat_pol)


doParallel::registerDoParallel(8)
esp_p <- foreach(i=1:n_p, .combine = rbind) %dopar% {  get_marginals(p_p[i]) }


doParallel::registerDoParallel(8)
esp_sd <- foreach(i=1:n_sd, .combine = rbind) %dopar% { get_marginals(p_sd[i]) }


glue("{nrow(subset(esp_p, animales>plantes))} PP sites where animals > plantes")
glue("{nrow(subset(esp_sd, animales>plantes))} SD sites where animals > plantes")


#######################################

#######################################
# 2) Connectance
#######################################

obs_conn <- networklevel(mat_pol, index=c("connectance"))


doParallel::registerDoParallel(8)
c_p <- foreach(i=1:n_p, .combine = rbind) %dopar% { run_conn(p_p[i]) }

doParallel::registerDoParallel(4)
c_sd <- foreach(i=1:n_sd, .combine = rbind) %dopar% { run_conn(p_sd[i]) }

c_p <- c_p %>% filter(c_p[,1]>0)
c_sd <- c_sd %>% filter(c_sd[,1]>0)

c_p$type <- "polinator"
c_sd$type <- "seed dispersal"
conn_pp_sd <- rbind(c_p, c_sd)

p_conn <- plot_sim_web(conn_pp_sd, connectance, obs_conn) + ggtitle("Connectance")


#######################################

#######################################
# 3) Degree Distribution
#######################################

# Dark, median and light grey lines refer to 
# exponential, power and truncated power law, respectively
p_deg_dist <- degreedistr(t(mat_pol),level = "higher")
dd_enron <- p_deg_dist %>% as_tibble() %>%  arrange(AIC) %>% slice(1) %>% pull(Estimate)


doParallel::registerDoParallel(8)
dd_p <- foreach(i=1:n_p, .combine = rbind) %dopar% { get_dd(p_p[i]) }

doParallel::registerDoParallel(8)
dd_sd <- foreach(i=1:n_sd, .combine = rbind) %dopar% { get_dd(p_sd[i]) }

dd_p$type <- "pollinator"

p_dist <- plot_sim_web(dd_p, estimate, dd_enron) + ggtitle("Cumulative degree dist.")


#######################################

#######################################
# 4) Nestedness
#######################################

enron_binary <- make_enron_mat_binary(mat_pol)
enron_nodf <- nested(enron_binary,method = "NODF")

nn <- 1:n_p
nn <- nn[-c(116,132)]
doParallel::registerDoParallel(8) # bit more data intensive
nest_p <- foreach(i=nn, .combine = rbind) %dopar% { get_nest(p_p[i]) }

doParallel::registerDoParallel(8)
nest_sd <- foreach(i=n_sd, .combine = rbind) %dopar% { get_nest(p_sd[i]) }

nest_p$type <- "pollinator"
nest_sd$type <- "seed dispersal"
nest <- rbind(nest_p, nest_sd)

# write_rds(nest, here(DIR_web_of_life,"nestedness.rds"))
# nest <- read_rds(here(DIR_web_of_life,"nestedness.rds"))

p_nodf <- plot_sim_web(nest, nodf, enron_nodf) + ggtitle("Nestedness (NODF)")


#######################################

#######################################
# 5) Modularity
#######################################

colnames(mat_pol) <- paste0("com", colnames(mat_pol))  
rownames(mat_pol) <- paste0("topic", rownames(mat_pol))

enron_mod <- computeModules(t(mat_pol))@likelihood

get_mod <- function(x) {
  tmp <- read_csv(here(DIR_web_of_life, x), col_types = cols()) 
  tibble(lh = computeModules(tmp)@likelihood,
         nos = NOS(tmp)$mod)
}

nn = 1:n_p
nn = nn[-c(116,132)]
doParallel::registerDoParallel(8)
foreach(i=nn, .combine = rbind) %dopar% { get_mod(p_p[i]) }

write_rds(mod_p, here(output, "mod_pol.rds"))

# mod_sd <- data.frame(lh = integer(n_sd),
#                      nos = integer(n_sd))
# nn = 1:n_sd
# nn = nn[-c(26,29)]
# for (i in nn) {
#   print(i)
#   print(Sys.time())
#   tmp <- read_csv(paste0(dir_sd,p_sd[i]), col_types = cols())
#   mod_sd$lh[i] <- computeModules(tmp)@likelihood
#   mod_sd$nos[i] <- NOS(tmp)$mod
# }
# saveRDS(mod_sd,paste0(dir_data,"mod_sd.rds"))

# mod_p <- readRDS(paste0(dir_data,"mod_pol.rds"))
# mod_p <- mod_p %>% filter(mod_p$lh>0)
# mod_sd <- readRDS(paste0(dir_data,"mod_sd.rds"))
# mod_sd <- mod_sd %>% filter(mod_sd$lh>0)
# 
# mod_p$type <- "pol"
# mod_sd$type <- "sd"
# modularite <- rbind(mod_p, mod_sd)
# saveRDS(modularite, paste0(dir_data,"modularite.rds"))

modularite <- readRDS(paste0(dir_data,"modularite.rds"))

ggplot(modularite,aes(lh, fill = type)) +
  geom_histogram(binwidth=0.1) +
  geom_point(aes(x=computeModules(mat_pol)@likelihood, y=57), colour="black") + 
  theme(legend.position = "none") +
  ggsave("mod_lh.png")

ggplot(modularite,aes(nos, fill = type)) +
  geom_histogram(binwidth=0.1) +
  geom_point(aes(x=NOS(mat_pol)$mod, y=15), colour="black") + 
  theme(legend.position = "none") +
  ggsave("mod_nos.png")


