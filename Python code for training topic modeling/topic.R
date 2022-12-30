# ------ paper 2 ------
library(FactoMineR)
data(tea)

res.mca = MCA(tea, ncp=20, quanti.sup=19, quali.sup=c(20:36), graph=FALSE)
res.hcpc = HCPC(res.mca)


# ------ paper 1 ------
library(tidyverse)
library(topicmodels)
# library(tidytext)
library(FactoMineR)
library(R.matlab)
library(umap)

# load simulated data
datapath <- "../dataset/COVID_positive_any_new_PASC_Feature_DX.mat"
data <- R.matlab::readMat(datapath)
simdata <- data$Feature.DX

# --- train a topic model, use LDA ---
# 1st argument of `LDA` can be a 0-1 indicator matrix, 
#   number of row = number of subject
#   number of column = number of attributes/words
lda_res <- LDA(simdata, 
               k = 10, 
               method = "VEM", 
               control = list(seed = 1234))

lda_res_post <- posterior(lda_res)

lda_res_post$terms    # probability of each term being from each topic
lda_res_post$topics    # probability of each subject being from each topic

# --- clustering based on the topics ---
cluster_res <- HCPC(as.data.frame(lda_res_post$topics), 
                    nb.clust = 4    # number of cluster
                    )
# HCPC will generate tree plot and factor map
# you can also draw the figures later
plot(cluster_res, choice = "tree")
plot(cluster_res, choice = "bar")
plot(cluster_res, choice = "map", draw.tree = FALSE)
plot(cluster_res, choice = "3D.map")

# --- umap visualization ---
lda_umap <- umap(lda_res_post$topics)

tibble(
    x = lda_umap$layout[, 1], 
    y = lda_umap$layout[, 2], 
    cluster = cluster_res$data.clust$clust
) %>%
    ggplot(aes(x = x, y = y)) + 
    geom_point(aes(color = cluster))
