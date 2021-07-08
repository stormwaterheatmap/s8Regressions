library(tidyverse)
library(factoextra)
library(cluster)
library(DataExplorer)
library(tidymodels)
library(caret)
library(broom)
load("~/GitHub/s8Regressions/data/s8_data.rda")

View(s8_data %>% head())

data.df <- s8_data %>% filter(paramgroup %in% c("Conventional", "Metal","Nutrient")) %>%
  pivot_wider(names_from = parameter,
             values_fn = max,
  values_from = new_result_value,
  id_cols = c(field_collection_start_date, study_name, location_name,nondetect_flag)) %>%
  as.data.frame()


data.df2 <- data.df %>% select(location_name, contains(c('zinc','cadmium','chloride'))) %>% select(!contains(c('solid')))


copper_cadmium <- data.df %>% select(location_name, contains(c('copper','cadmium'))) %>% select(!contains(c('solid')))
copper_zinc <- data.df %>% select(location_name, contains(c('copper','zinc')))%>% select(!contains(c('solid')))
cadmium_zinc <- data.df %>% select(location_name, contains(c('cadmium','zinc')))%>% select(!contains(c('solid')))
cadmium_zinc <- data.df %>% select(location_name, contains(c('cadmium','zinc')))%>% select(!contains(c('solid')))



cluster_data <- function(data, num_clusters = 3){


  fit1 <- kmeans(data[,-1],num_clusters)

  return(fit1)

}

cluster_and_plot <- function(data.df,num_clusters = 3){
cluster.1 <- cluster_data(data.df,num_clusters)
plot(cluster.1$data, col = cluster.1$fit$cluster,pch=1)
}

prep_data <- function(data.df){
  df_transformed<-  data.df %>%
  select(!contains(c("solid")))  %>% drop_na() %>%
      mutate(across(where(is.numeric), ~ log(.x)))

    preObj <- preProcess(df_transformed, method=c("center", "scale"))
    newData <- predict(preObj, df_transformed)
    return(newData)


}

copper_cadmium <- data.df %>%
  select(location_name, contains(c('copper','cadmium'))) %>%
  prep_data()


parameters <- c("surfactant", "chloride", #"nitrogen",
                "oxygen", "nitrate")#,"Total Suspended Solid")

parameters <- c("copper","lead")

study_names <- s8_data$study_name %>% unique() %>% sort()




clus1 <- copper_cadmium[,-1] %>% kmeans(3)

clus1 %>%
  mutate(cluster = clus1$cluster,
         location = location_name) %>%
  ggplot(aes(`Copper - Water - Total`, `Lead - Water - Total`, color = factor(cluster), label = location_name))+
  geom_point()
  #geom_text()


fviz_cluster(clus1,data = copper_cadmium[,-1])
# K-Means Cluster Analysis
 # 5 cluster solution
# get cluster means
ag_df <- aggregate(copper_cadmium,by=list(clus1$cluster),FUN=mean)



points <- copper_cadmium[,-1]
kclust <- kmeans(copper_cadmium[,-1], centers = 4)

kclusts <-
  tibble(k = 1:4) %>%
  mutate(
    kclust = map(k, ~kmeans(points, .x)),
    tidied = map(kclust, broom::tidy),
    glanced = map(kclust, broom::glance),
    augmented = map(kclust,broom::augment, copper_cadmium)
  )
clusters <-
  kclusts %>%
  unnest(cols = c(tidied))

assignments <-
  kclusts %>%
  unnest(cols = c(augmented))

clusterings <-
  kclusts %>%
  unnest(cols = c(glanced))

p1 <-
  ggplot(assignments, aes(x = `location_name`, y = `Cadmium - Water - Dissolved`)) +
  geom_point(aes(color = .cluster), alpha = 0.5)

p1 +  facet_wrap(~ location_name)
p1

pca_fit <- data.df2 %>%
  select(where(is.numeric)) %>%
  drop_na() %>% # retain only numeric columns
  prcomp(scale = TRUE)


dd <- pca_fit %>%
  augment(data.df2 %>% drop_na()) %>% # add original dataset back in
  ggplot(aes(.fittedPC1, .fittedPC2, color = location_name)) +
  geom_point(size = 1.5)

# define arrow style for plotting
arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
)


pca_fit %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text(
    aes(label = column),
    hjust = 1, nudge_x = -0.02,
    color = "#904C2F"
  ) +
  xlim(-1.25, .5) + ylim(-.5, 1) +
  coord_fixed()  # fix aspect ratio to 1:1



biopsy <- read_csv("https://wilkelab.org/classes/SDS348/data_sets/biopsy.csv")

data.df <- s8_data %>% filter(paramgroup %in% "Metal") %>% #c("Conventional", "Metal","Nutrient")) %>%
  pivot_wider(names_from = parameter,
              values_fn = max,
              values_from = new_result_value,
              id_cols = c(field_collection_start_date, study_name, location_name,nondetect_flag)) %>%
  as.data.frame()


data.df2 <- data.df %>% select(location_name, contains(c('zinc','cadmium', 'copper', 'lead'))) %>%
  select(!contains(c('solid'))) %>%
  drop_na() %>%
  mutate(across(where(is.numeric), ~ log(.x)))

rownames(data.df2) <- (data.df2$location_name %>% as.factor() %>% as.numeric() %>% as.character() %>% make.unique())



library("factoextra")
# K-means clustering
km.res <- eclust(data.df2 %>% select(where(is.numeric)), "kmeans", k = 4,
                 nstart = 25, graph = FALSE)

res.hc <- eclust(data.df2, "hclust", k = 4,
                 method = "ward.D2", graph = FALSE)

fviz_cluster(km.res,  frame.type = "norm", frame.level = 0.68)
fviz_dend(res.hc, rect = TRUE, show_labels = TRUE, cex = 0.5)

res.hk <-hkmeans(data.df2 %>% select(where(is.numeric)), 4)

fviz_cluster(res.hk, frame.type = "norm", frame.level = 0.68)

fviz_dend(res.hk, cex = 0.6, rect = TRUE)


pca_fit <- data.df2 %>%
  select(where(is.numeric)) %>% # retain only numeric columns
  prcomp(scale = TRUE) # do PCA on scaled data

pca_fit %>%
  augment(data.df2) %>% # add original dataset back in
  ggplot(aes(.fittedPC1, .fittedPC2, color = location_name)) +
  geom_point(size = 1.5)

pca_fit %>%
  tidy(matrix = "rotation")

arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
)

# plot rotation matrix
pca_fit %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text(
    aes(label = column),
    hjust = 1, nudge_x = -0.02,
    color = "#904C2F"
  ) +
  xlim(-1.25, .5) + ylim(-.5, 1) +
  coord_fixed()
