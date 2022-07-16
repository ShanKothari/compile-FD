setwd("C:/Users/kotha020/Dropbox/PostdocProjects/WallisFunctionalTraits/")

trait_matrix<-read.csv("filled_trait_matrix.csv")
all_traits_site<-which(!is.na(trait_matrix$trait_n_perc) & 
                         !is.na(trait_matrix$trait_leaf_mass_per_area_g_m2) & 
                         !is.na(trait_matrix$trait_leaf_dry_matter_content_mg_g))
all_traits_CABO<-which(!is.na(trait_matrix$N_CABO) & !is.na(trait_matrix$LMA_CABO) & !is.na(trait_matrix$LDMC_CABO))
all_traits_sp<-which(!is.na(trait_matrix$N_TRYsp) & !is.na(trait_matrix$LMA_TRYsp) & !is.na(trait_matrix$LDMC_TRYsp))
all_traits_gn<-which(!is.na(trait_matrix$N_TRYgn) & !is.na(trait_matrix$LMA_TRYgn) & !is.na(trait_matrix$LDMC_TRYgn))

## remove all rows with NAs in our target traits
trait_matrix<-trait_matrix[all_traits_gn,]
trait_matrix$sp_project<-paste(trait_matrix$binomial,trait_matrix$project,sep="_")

## log-transform LMA and N to reduce skewness
## since differences may be less important at the upper tail
trait_matrix$logN_TRYgn<-log(trait_matrix$N_TRYgn)
trait_matrix$logLMA_TRYgn<-log(trait_matrix$LMA_TRYgn)

## z-standardize all traits to give them equal emphasis
trait_matrix$N_z<-(trait_matrix$logN_TRYgn-mean(trait_matrix$logN_TRYgn))/sd(trait_matrix$logN_TRYgn)
trait_matrix$LMA_z<-(trait_matrix$logLMA_TRYgn-mean(trait_matrix$logLMA_TRYgn))/sd(trait_matrix$logLMA_TRYgn)
trait_matrix$LDMC_z<-(trait_matrix$LDMC_TRYgn-mean(trait_matrix$LDMC_TRYgn))/sd(trait_matrix$LDMC_TRYgn)

## calculate trait distances
trait_dist<-dist(trait_matrix[,c("N_z","LMA_z","LDMC_z")],method="euclidean")
## forcing trait distances to be between 0 and 1
trait_dist<-(trait_dist-min(trait_dist))/(max(trait_dist)-min(trait_dist))

#######################################
## read species matrix

sp_matrix<-read.csv("Abundance_openSites_update.csv")
sp_matrix$proj_code<-NA

## assigning species x project names consistent with the trait matrix
sp_matrix$proj_code[sp_matrix$project=="Pool-Boucherville"]<-"IGB"
sp_matrix$proj_code[sp_matrix$project=="2018-Hacker-PhD-UBC"]<-"CGOP"
sp_matrix$proj_code[sp_matrix$project=="2019-MerBleue"]<-"MB"

## deal with taxon not IDed to genus
sp_matrix$scientific_name<-as.character(sp_matrix$scientific_name)
sp_matrix$scientific_name[sp_matrix$scientific_name=="Abildgaardieae"]<-"Abildgaardieae Ignore"

## get binomial names (sans authority) consistent with trait data
sp_mat_split<-strsplit(sp_matrix$scientific_name,split=" ")
sp_matrix$genus<-unlist(lapply(sp_mat_split,function(el) el[[1]]))
sp_matrix$species<-unlist(lapply(sp_mat_split,function(el) el[[2]]))
sp_matrix$species[which(sp_matrix$species!=tolower(sp_matrix$species))]<-NA
sp_matrix$binomial<-paste(sp_matrix$genus,sp_matrix$species,sep=" ")

## create species x project ID
sp_matrix$sp_project<-paste(sp_matrix$binomial,sp_matrix$project,sep="_")
