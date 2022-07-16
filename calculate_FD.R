setwd("C:/Users/kotha020/Dropbox/PostdocProjects/WallisFunctionalTraits/")

trait_matrix<-read.csv("filled_trait_matrix.csv")
all_traits_site<-which(!is.na(trait_matrix$) & !is.na(trait_matrix$LMA_site) & !is.na(trait_matrix$LDMC_site))
all_traits_CABO<-which(!is.na(trait_matrix$N_CABO) & !is.na(trait_matrix$LMA_CABO) & !is.na(trait_matrix$LDMC_CABO))
all_traits_sp<-which(!is.na(trait_matrix$N_TRYsp) & !is.na(trait_matrix$LMA_TRYsp) & !is.na(trait_matrix$LDMC_TRYsp))
all_traits_gn<-which(!is.na(trait_matrix$N_TRYgn) & !is.na(trait_matrix$LMA_TRYgn) & !is.na(trait_matrix$LDMC_TRYgn))

IGB_trait_matrix<-trait_matrix[trait_matrix$project=="IGB",]
MB_trait_matrix<-trait_matrix[trait_matrix$project=="MB",]
CGOP_trait_matrix<-trait_matrix[trait_matrix$project=="CGOP",]

sp_matrix<-read.csv("Abundance_openSites_update.csv")
IGB_sp_matrix<-sp_matrix[sp_matrix$project=="Pool-Boucherville",]
MB_sp_matrix<-sp_matrix[sp_matrix$project=="2019-MerBleue",]
CGOP_sp_matrix<-sp_matrix[sp_matrix$project=="2018-Hacker-PhD-UBC",]