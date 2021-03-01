# GoiasTinProvince

This code is part of the expanded abstract "Unsupervised segmentation of  the Serra Dourada Granite, Goiás Tin Province, based on Airborne Geophysics", presented 
at the 17th International Congress of the [Brazilian Geophysical Society.](https://sbgf.org.br/congresso/)

## Abstract
Manual interpretation of high-dimensional geoscience data can represent a significant challenge since a large volume of data is increasingly overwhelming geoscientists. Dimensional reduction techniques, such as unsupervised segmentation, can reveal high-dimensional patterns within data that can provide meaningful insights to support the geoscientific data interpretation. In this work, we use unsupervised segmentation techniques, such as Principal Component Analysis (PCA)  and Model-Based Clustering on radiometric and magnetic airborne geophysical data of the Serra Dourada Granite (SGD), located at the western portion of the Goiás Tin Province, center Brazil. The SDG has an association with greisen mineralization and a late-magmatic enrichment on REE, with the formation of supergene deposits of REE on its southernmost portion. Due to the high similarity of the rock's aspects on the outcrop scale, the whole batholith lacks a more accurate mapping, and the division on sub-units remains a problem. We propose here an alternative approach to assist the batholith's cartography through a quantitative analysis over the airborne geophysical data: potassium (K; %), Thorium (eTh; ppm), Uranium (eU; ppm), Total Count (CT; µR/h), and the Total Gradient Amplitude (TGA; nT/m). The features are prepared using Centered Log-Ratio, opening the compositional data, and Min-Max Feature Scaling for variance equalization. Then, we applied PCA to decrease redundancy and create the new orthogonal basis, which improves the segmentation's performance. For cluster analysis, we apply a Model-Based Clustering approach, with the Bayesian Criteria Information function, to automatically identify the most appropriate number of clusters accordingly to a probabilistic determination. Our results allow the individualization of 9 significant areas with local coherence and similar multivariate relations by looping through some data segmentation rounds. Additionally, we detail the cluster with the highest values of eTh as it also has relevance for Rare-Earth Elements prospection. Finally, we evaluate the proposed segmentation by comparing it response according to a multivariate analysis of relation values for felsic rocks.

## Depends:
R (≥ 3.6.0)

## Imports
``` r
library(tidyverse) # ggplot, dplyr, readr, tibble, readr
library(mclust) # Model-Based Clustering
library(factoextra) # Cluster Vis and PCA
library(geoquimica) # Data transformation
library(ggpubr) # ggarrange
```
## Authors:
* [Guilherme Ferreira da Silva](https://cutt.ly/RdsGmT5), (Mail to: guilherme.ferreira@cprm.gov.br)
* [Marcos Vinícius Ferreira](http://lattes.cnpq.br/0664633989688055) 
* [Iago Sousa Lima Costa](http://lattes.cnpq.br/9427131869731616) 
* [Lucy Takehara Chemale](http://lattes.cnpq.br/6291856399463658)
* [Luiz Gustavo Rodrigues Pinto](http://lattes.cnpq.br/3875556796137699)
