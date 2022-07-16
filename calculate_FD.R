setwd("C:/Users/kotha020/Dropbox/PostdocProjects/WallisFunctionalTraits/")

trait_matrix<-read.csv("filled_trait_matrix.csv")
IGB_trait_matrix<-trait_matrix[trait_matrix$project=="IGB",]
MB_trait_matrix<-trait_matrix[trait_matrix$project=="MB",]
CGOP_trait_matrix<-trait_matrix[trait_matrix$project=="CGOP",]

sp_matrix<-read.csv("Abundance_openSites_update.csv")
IGB_sp_matrix<-sp_matrix[sp_matrix$project=="Pool-Boucherville",]
MB_sp_matrix<-sp_matrix[sp_matrix$project=="2019-MerBleue",]
CGOP_sp_matrix<-sp_matrix[sp_matrix$project=="2018-Hacker-PhD-UBC",]
