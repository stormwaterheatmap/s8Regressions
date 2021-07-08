library(tidyverse)
library(factoextra)
library(cluster)
library(DataExplorer)
library(tidymodels)
library(caret)
library(broom)
library(janitor)
load("~/GitHub/s8Regressions/data/s8_data.rda")

View(s8_data %>% head())

data.df <- s8_data %>% filter(paramgroup %in% c("Nutrient")) %>% #,"Conventional","Metal")) %>%
 #filter(study_id %in% c("WAR044503_S8D"),"WAR044003_S8D")) %>%
  filter(!parameter %in% c('Biochemical Oxygen Demand - Water - Total', 'Magnesium - Water - Dissolved', 'Chloride - Water - Total', 'Total Suspended Solids - Water - Total', 'Ammonia - Water - Total',
                           'Mercury - Water - Total', 'Mercury - Water - Dissolved','Arsenic - Water - Dissolved',
                           'Calcium - Water - Dissolved','Calcium - Water - Total','Hardness as CaCO3 - Water - Total','Magnesium - Water - Total') ) %>%
  filter(sample_matrix%in% c("water","Water","WATER")) %>%
  mutate(loc_date =paste0(location_id)) %>% #,field_collection_start_date)) %>%
  pivot_wider(names_from = loc_date,id_cols = parameter,
              values_from = new_result_value,
              values_fn = mean) %>%
  t() %>% as.data.frame() %>%
drop_na() %>%
  #t() %>%
  as.data.frame() %>%
  mutate(across(where(is.numeric), ~ log(.x))) %>%
  row_to_names(row_number = 1)


data.df2 <- data.df  %>% remove_rownames() %>%
  #column_to_rownames("parameter") %>%
  mutate_all(function(x) as.numeric(as.character(x))) %>%
  scale(center = TRUE,scale = TRUE)


library(factoextra)
set.seed(123)
# K-means on faithful dataset




library(factoextra)
set.seed(123)
km.res1 <- kmeans(data.df2, 3)
fviz_cluster(km.res1, data = data.df2,repel =TRUE,ellipse.type = "convex")
fviz_cluster(km.res1, data = data.df2,repel =TRUE,ellipse.type = "convex")

km.res1 <- kmeans(data.df2, 3)
fviz_cluster(km.res1, data = data.df2,repel =TRUE,ellipse.type = "convex")




