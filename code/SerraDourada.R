#####
# Script of processing data for the Expanded Abstract: "Unsupervised segmentation of 
# the Serra Dourada Granite, Goiás Tin Province, based on Airborne Geophysics", presented 
# at the 17th International Congress of the Brazilian Geophysical Society.
# https://sbgf.org.br/congresso/
#
# version: 0.1 (2021/03/01)
#
# Last modifications: Publication
#
# -----
# Process flow: Import, Min-Max Feature Scaling, CLR and Min-Max Feature Scaling, 
#               centered and scaled PCA, MBC: First Round, Data Analysis, MBC: Second
#               Round, Data Analysis, Export Figures
# -----
# Guilherme Ferreira, (guilherme.ferreira@cprm.gov.br)
# March, 2021
#####

# Setting up the enviroment

setwd('~/GitHub/GoiasTinProvince') # Working direction
set.seed(0) # Random State

# Import Packages

library(tidyverse) # ggplot, dplyr, readr, tibble, readr
library(mclust) # Model-Based Clustering
library(factoextra) # Cluster Vis and PCA
library(geoquimica) # Data transformation
library(ggpubr) # ggarrange

#####
# Data wrangling
#####

# Importing ----
df_raw <- readRDS('data/gamma_GSD.RDS') %>%
  filter(!is.na(X)) %>%
  select(c('X','Y','KPERC','eTH','eU','CT', 'ASA')) %>%
  rename(Longitude = X,
         Latitude = Y,
         TGA = ASA,
         TC = CT,
         K = KPERC)

# Processing ----

# Min-max feature scaling
df_norm <- df_raw %>%
  elem_norm(method = 'minmax', keep = c('Longitude', 'Latitude'))

# CLR transformation
df_clr <- df_raw %>%
  elem_norm(method = 'clr', keep = c('Longitude', 'Latitude')) %>%
  elem_norm(method = 'minmax', keep = c('Longitude', 'Latitude'))

# PCA
pca <- prcomp(x = df_clr[,3:7],center = TRUE,scale. = TRUE)

summary(pca)

## Appending Components
df <- bind_cols(df_clr, as_tibble(pca$x))

# Plot Feature Maps ----

## Min-Max 
(raw.plot <- ggplot(df_norm %>%
                      gather(key = 'Variables',value = 'Value', 3:7),
                    aes(x = Longitude, y = Latitude, fill = Value)) +
    geom_raster() +
    coord_equal() +
    scale_fill_gradient2(low = 'blue',
                         mid = 'green',
                         high = 'red',
                         midpoint = .5) +
    facet_wrap(. ~ Variables,nrow = 1) +
    theme_classic() +
    theme(axis.text.x = element_text(
      angle = 90,
      vjust = 0.5),
      legend.direction = 'horizontal',
      legend.position = 'bottom')
  +
    labs(fill = 'Scaled Values')
)

## CLR
(clr.plot <- ggplot(df_clr %>%
                      gather(key = 'Variables',value = 'Value', 3:7),
                    aes(x = Longitude, y = Latitude, fill = Value)) +
    geom_raster() +
    coord_equal() +
    scale_fill_gradient2(low = 'blue',
                         mid = 'green',
                         high = 'red',
                         midpoint = .5) +
    facet_wrap(. ~ Variables,nrow = 1) +
    theme_classic() +
    theme(axis.text.x = element_text(
      angle = 90,
      vjust = 0.5),
      legend.direction = 'horizontal',
      legend.position = 'bottom')
  +
    labs(fill = 'CLR + Scaled Values')
)

## Principal Components maps
(pca.plot <- ggplot(df %>%
         gather(key = 'Components',value = 'PCA', 8:12),
       aes(x = Longitude, y = Latitude, fill = PCA)) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_c() +
  facet_wrap(. ~ Components,nrow = 1) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5),
        legend.direction = 'horizontal',legend.position = 'bottom')
)

### PCA Individuals and Variables importances
fviz_pca_ind(pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = F,      # Avoid text overlapping
             axes = c(1,2), #controla quais eixos devem ser mostrados na figura
             geom = c("point")
)

fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#####
# Airborne Geophysics
#####

# Model Based Clustering: 1st Round ----
set.seed(0) # Fixing a random seed for reproducibility
tic <- Sys.time() # Initial time
mbc <- Mclust(pca$x)
tac <- Sys.time() # Final time
tac - tic # Benchmarking

summary(mbc)

## Appending MBC result
df <- bind_cols(df_raw, as_tibble(pca$x),
                as_tibble(mbc['classification']))
## Adjusting variables
df <- df %>%
  mutate(cluster = case_when(classification == 1 ~ 'Cluster 1',
                             classification == 2 ~ 'Cluster 2',
                             classification == 3 ~ 'Cluster 3',
                             classification == 4 ~ 'Cluster 4',
                             classification == 5 ~ 'Cluster 5',
                             classification == 6 ~ 'Cluster 6',
                             classification == 7 ~ 'Cluster 7',
                             classification == 8 ~ 'Cluster 8',
                             TRUE ~ 'Cluster 9')) %>%
  mutate(cluster = factor(cluster),
         classification = NULL)

## Plot MBC
### Bayiesian Information Criterion 
plot(mbc, what = 'BIC')
### Biplot of Classification by feature combinations
plot(mbc,what = 'classification')
### Map of 1st round clustering
(map1 <- ggplot(df, aes(x = Longitude,
               y = Latitude,
               fill = as.factor(cluster))) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_d(option = 'B') +
  theme_classic() +
  labs(fill = 'Round 1') +
    theme(axis.text.x = element_text(angle = 90,
                                     vjust = .5))
)
## Biplot of features by clusters

### TC x K colored by eTH
df %>%
  ggplot(aes(x = K, y = TC, col = eTH)) +
  geom_point() +
  scale_color_viridis_c()

### TGA vs eTH colored by eU
df %>%
  ggplot(aes(x = eTH, y = TGA, col = eU)) +
  geom_point() +
  scale_color_viridis_c(alpha = .7,) +
  facet_wrap(. ~ cluster)

# Model Based Clustering: 2nd Round ----

## Selecting Clusters 6 and 7
df6 <- df %>%
  select(Longitude, Latitude, K, eTH, eU, TC, TGA, cluster) %>%
  filter(cluster == 'Cluster 6' |
         cluster == 'Cluster 7')

## PCA
pca9 <- prcomp(df6[,c('K','eTH','eU','TC','TGA')],
               center = TRUE,
               scale. = FALSE)
summary(pca9)

## MBC
set.seed(123)
mbc9 <- Mclust(pca9$x)
summary(mbc9)

### Appending MBC result
df6 <- bind_cols(df6,
                 as_tibble(mbc9['classification']))

### Adjusting variables
df6 <- df6 %>%
  mutate(Cluster = case_when(classification == 1 ~ 'Cluster a',
                             classification == 2 ~ 'Cluster b',
                             classification == 3 ~ 'Cluster c',
                             classification == 4 ~ 'Cluster d',
                             classification == 5 ~ 'Cluster e',
                             classification == 6 ~ 'Cluster f',
                             classification == 7 ~ 'Cluster g',
                             classification == 8 ~ 'Cluster h',
                             classification == 9 ~ 'Cluster i')) %>%
  mutate(classification = NULL)

## MBC plot

### eU x K colored by eTH
df6 %>%
  ggplot(aes(x = K, y = eU, col = eTH)) +
  geom_point() +
  scale_color_viridis_c(alpha = .5,) +
  facet_wrap(. ~ Cluster)

### Map of 2nd round clustering
(map2 <- df6 %>%
  ggplot(aes(x = Longitude,
             y = Latitude,
             fill = Cluster)) +
  geom_raster(inherit.aes = FALSE,
              data = df,
              mapping = aes(x = Longitude, y = Latitude),
              fill = 'gray', alpha = .5) +
  geom_raster() +
  coord_equal() +
  theme_classic() +
  labs(fill = 'Round 2') +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = .5))
)

#####
# Lithogeochemistry
#####

# Import data ----

geoq <- readRDS(file = 'data/geochemistry.RDS')

# Data normalization ----
geoq_norm <- geoq %>%
  filter(Longitude > -48.6) %>%
  filter(Latitude < -13) %>%
  elem_norm(method = 'clr',keep = c('Latitude','Longitude','ETR','Y')) %>%
  elem_norm(method = 'minmax',keep = c('Latitude','Longitude','ETR','Y')) 

# Correlation analysis ----
M <- cor(geoq_norm %>%
           filter(Longitude > -48.6) %>%
           filter(Latitude < -13) %>%
           filter(K2O > .5) %>%
           select(6:12,27),
         method = "pearson")

(corrplot <- corrplot(M, method="color",# col=col(200),  
         type="upper", order= 'original', 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         diag=FALSE, 
         tl.cex = 1, # tamanho da fonte dos labels
         tl.offset = 1, # offset do nome das colunas em relação a matriz
         number.font = 1.5,
         number.digits = 1)
)

(pairs.plot <- ggpairs(geoq_norm %>%
          filter(Longitude > -48.6) %>%
          filter(Latitude < -13) %>%
          filter(K2O > .85) %>%
          filter(Th > .45) %>%
          select(6:12,27)) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = .5))
)

# Map of REE concentration

zoom <- data.frame(xmin = -48.579,
                   xmax = -48.435,
                   ymin = -13.575,
                   ymax = -13.4)


(map.full <- geoq %>%
    filter(Longitude > -48.55) %>%
    filter(Latitude < -13) %>%
    ggplot(aes(x = Longitude, y = Latitude, fill = ETR, size = ETR)) +
    geom_raster(inherit.aes = FALSE,
                data = df_raw, fill = 'gray',
                mapping = aes(x = Longitude, y = Latitude)) +
    geom_raster(inherit.aes = FALSE,
                data = df6, fill = 'gray30',
                mapping = aes(x = Longitude, y = Latitude)) +
    # geom_rect(inherit.aes = FALSE, col = 'black',
    #           fill = NA, data = zoom,mapping = aes(xmin = xmin,
    #                                                xmax = xmax,
    #                                                ymin = ymin,
    #                                                ymax = ymax)) +
    geom_jitter(alpha = .6, shape = 21, col = 'black',width = .005,height = .005) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90,hjust = .5,vjust = .5)) +
    coord_equal() +
    scale_fill_gradient(low = 'green',high = 'red') +
    scale_size_binned(range = c(.1,6),nice.breaks = TRUE) +
    labs(fill = expression(paste(Sigma,'REE (ppm)')),
         size = expression(paste(Sigma,'REE (ppm)')))
)

(map.detail <- geoq %>%
    filter(Longitude > -48.6) %>%
    filter(Latitude < -13) %>%
    ggplot(aes(x = Longitude, y = Latitude, fill = ETR, size = ETR)) +
    geom_raster(inherit.aes = FALSE,
                data = df_raw, fill = 'gray',
                mapping = aes(x = Longitude, y = Latitude)) +
    geom_raster(inherit.aes = FALSE,
                data = df6, fill = 'gray30',
                mapping = aes(x = Longitude, y = Latitude)) +
    geom_rect(inherit.aes = FALSE, col = 'black',
              fill = NA,
              data = zoom,
              mapping = aes(xmin = xmin,
                            xmax = xmax,
                            ymin = ymin,
                            ymax = ymax)) +
    geom_point(alpha = .6, shape = 21, col = 'black') +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90,hjust = .5,vjust = .5)) +
    coord_equal(ylim = c(-13.6, -13.4)) +
    scale_fill_gradient(low = 'green',high = 'red') +
    scale_size_binned(range = c(.1,6),nice.breaks = TRUE) +
    labs(fill = expression(paste(Sigma,'REE (ppm)')),
         size = expression(paste(Sigma,'REE (ppm)')))
)

#####
# Exporting Figures
#####

# Figure 02: Map of raw, clr transformed and PCA transformed features
Cairo::CairoPDF(file = 'figure/Figure02.pdf',width = 8,height = 13)
ggarrange(raw.plot, clr.plot, pca.plot,ncol = 1)
dev.off()

# Figure 03: BIC curve for MBC
Cairo::CairoPDF(file = 'figure/Figure03.pdf',width = 9,height = 5)
plot(mbc, what = 'BIC')
dev.off()

# Figure 04: MBC adjustment of model VVV on data
Cairo::CairoPDF(file = 'figure/Figure04.pdf',width = 8,height = 8)
plot(mbc,what = 'classification')
dev.off()

# Figure 05 - Boxplot for Round 1 segmentation
Cairo::CairoPDF(file = 'figure/Figure05.pdf',width = 10,height = 5)
df %>%
  elem_norm(method = 'clr', keep = c('Longitude','Latitude','PC1','PC2','PC3','PC4','PC5','cluster')) %>%
  elem_norm(keep = c('Longitude','Latitude','PC1','PC2','PC3','PC4','PC5','cluster')) %>%
  gather(key = 'Features',
         value = 'Values',
         3:7) %>%
  ggplot(aes(x = Features, y =Values, fill = Features)) +
  geom_boxplot(outlier.shape = NA) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = 'top') +
  facet_wrap(. ~ cluster,nrow = 1) +
  labs(y = 'CLR + Scaled Values')
dev.off()

# Figure 06 - Biplot for Round 1 segmentation
Cairo::CairoPDF(file = 'figure/Figure06',width = 6,height = 4)
df %>%
  ggplot(aes(x = eU, y = eTH, col = TC)) +
  geom_point() +
  scale_color_viridis_c(alpha = .5,
                        option = 'D') +
  facet_wrap(. ~ cluster) +
  labs(col = '  TC \nmR/h')
dev.off()

# Figure 07 - Map of Clusters Rounds 1 and 2
Cairo::CairoPDF(file = 'figure/Figure07.pdf',width = 12,height = 9)
ggarrange(map1, map2, map.full, nrow = 1)
dev.off()