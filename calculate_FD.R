setwd("C:/Users/kotha020/Dropbox/PostdocProjects/WallisFunctionalTraits/")

library(reshape2)
library(FD)

trait_matrix<-read.csv("filled_trait_matrix.csv")

## duplicated binomials
## distinguish the two Phragmites varieties since CABO has data on both
## delete the second Sanicula variety because there's only genus-level data for it
trait_matrix$binomial<-as.character(trait_matrix$binomial)
trait_matrix$binomial[trait_matrix$scientific_name=="Phragmites australis (Cavanilles) Trinius ex Steudel subsp. australis"]<-"Phragmites australis var. australis"
trait_matrix<-trait_matrix[-which(trait_matrix$scientific_name=="Sanicula crassicaulis Poeppig ex de Candolle var. crassicaulis"),]

## create sp x project codes
## this allows us to use the best (most specific) possible
## trait data for species in each site
trait_matrix$sp_project<-paste(trait_matrix$binomial,trait_matrix$project,sep="_")

## remove all rows with NAs in our target traits
all_traits_site<-which(!is.na(trait_matrix$trait_n_perc) & 
                         !is.na(trait_matrix$trait_leaf_mass_per_area_g_m2) & 
                         !is.na(trait_matrix$trait_leaf_dry_matter_content_mg_g))
all_traits_CABO<-which(!is.na(trait_matrix$N_CABO) & !is.na(trait_matrix$LMA_CABO) & !is.na(trait_matrix$LDMC_CABO))
all_traits_sp<-which(!is.na(trait_matrix$N_TRYsp) & !is.na(trait_matrix$LMA_TRYsp) & !is.na(trait_matrix$LDMC_TRYsp))
all_traits_gn<-which(!is.na(trait_matrix$N_TRYgn) & !is.na(trait_matrix$LMA_TRYgn) & !is.na(trait_matrix$LDMC_TRYgn))
## allowing up to genus-level TRY traits (most permissive)
trait_matrix<-trait_matrix[all_traits_gn,]

## log-transform LMA and N to reduce skewness
## since differences may be less important at the upper tail
trait_matrix$logN_TRYgn<-log(trait_matrix$N_TRYgn)
trait_matrix$logLMA_TRYgn<-log(trait_matrix$LMA_TRYgn)

## z-standardize all traits to give them equal emphasis
trait_matrix$N_z<-(trait_matrix$logN_TRYgn-mean(trait_matrix$logN_TRYgn))/sd(trait_matrix$logN_TRYgn)
trait_matrix$LMA_z<-(trait_matrix$logLMA_TRYgn-mean(trait_matrix$logLMA_TRYgn))/sd(trait_matrix$logLMA_TRYgn)
trait_matrix$LDMC_z<-(trait_matrix$LDMC_TRYgn-mean(trait_matrix$LDMC_TRYgn))/sd(trait_matrix$LDMC_TRYgn)

#######################################
## read species matrix

sp_matrix<-read.csv("Abundance_openSites_update.csv")

## drop low-priority plots
sp_matrix<-sp_matrix[-which(sp_matrix$Priority==3),]

## assigning species x project names consistent with the trait matrix
sp_matrix$proj_code<-NA
sp_matrix$proj_code[sp_matrix$project=="Pool-Boucherville"]<-"IGB"
sp_matrix$proj_code[sp_matrix$project=="2018-Hacker-PhD-UBC"]<-"CGOP"
sp_matrix$proj_code[sp_matrix$project=="2019-MerBleue"]<-"MB"
## remove 2018 and 2019 (non-pooled) Boucherville
sp_matrix<-sp_matrix[-which(is.na(sp_matrix$proj_code)),]

## deal with taxon not IDed to genus
sp_matrix$scientific_name<-as.character(sp_matrix$scientific_name)
sp_matrix$scientific_name[sp_matrix$scientific_name=="Abildgaardieae"]<-"Abildgaardieae Ignore"

## get binomial names (sans authority) consistent with trait data
sp_mat_split<-strsplit(sp_matrix$scientific_name,split=" ")
sp_matrix$genus<-unlist(lapply(sp_mat_split,function(el) el[[1]]))
sp_matrix$species<-unlist(lapply(sp_mat_split,function(el) el[[2]]))
sp_matrix$species[which(sp_matrix$species!=tolower(sp_matrix$species))]<-NA
sp_matrix$binomial<-paste(sp_matrix$genus,sp_matrix$species,sep=" ")

## separate Phragmites varieties again
sp_matrix$binomial[sp_matrix$scientific_name=="Phragmites australis (Cavanilles) Trinius ex Steudel subsp. australis"]<-"Phragmites australis var. australis"

## aggregate by binomal and plot_field_id within project
## this is really ONLY for Sanicula crassicaulis, of which
## two varieties are found in the same plot
## (so we're just aggregating them within that plot)
sp_matrix_agg<-aggregate(sp_matrix[,"abundance"],
                         by=list(sp_matrix$plot_field_id,
                                 sp_matrix$proj_code,
                                 sp_matrix$binomial),
                         FUN=sum)
colnames(sp_matrix_agg)<-c("plot_field_id","proj_code","binomial","abundance")

## create species x project ID
sp_matrix_agg$sp_project<-paste(sp_matrix_agg$binomial,sp_matrix_agg$proj_code,sep="_")
sp_matrix_agg$plot_project<-paste(sp_matrix_agg$plot_field_id,sp_matrix_agg$proj_code,sep="-")
sp_matrix_agg$binomial<-NULL
sp_matrix_agg$proj_code<-NULL
sp_matrix_agg$plot_field_id<-NULL

## calculate what percent of species cover
## we have trait data for
sp_matrix_split<-split(sp_matrix_agg,f=list(sp_matrix_agg$plot_project))
sp_coverage<-unlist(lapply(sp_matrix_split,function(plot_df){
  coverage<-sum(plot_df$abundance[plot_df$sp_project %in% trait_matrix$sp_project],na.rm=T)/sum(plot_df$abundance,na.rm=T)
  return(coverage)}))

## drop rows with species x project IDs not in trait_matrix
sp_matrix_agg<-sp_matrix_agg[which(sp_matrix_agg$sp_project %in% trait_matrix$sp_project),]

## turn into a community matrix
sp_matrix_melt<-melt(sp_matrix_agg,id.vars=c("plot_project","sp_project"))
sp_matrix_wide<-dcast(sp_matrix_melt,plot_project~sp_project)

## turn sites into rownames
rownames(sp_matrix_wide)<-sp_matrix_wide[,"plot_project"]
sp_matrix_wide$plot_project<-NULL
sp_matrix_wide<-as.matrix(sp_matrix_wide)
sp_matrix_wide[is.na(sp_matrix_wide)]<-0

###################################
## arrange species and trait matrices alike

## get trait_matrix rows into the right order
## including dropping species x project IDs not
## in the species matrix
trait_matrix_order<-trait_matrix[match(colnames(sp_matrix_wide),trait_matrix$sp_project),]
rownames(trait_matrix_order)<-trait_matrix_order$sp_project

## calculate trait distances
trait_dist<-dist(trait_matrix_order[,c("N_z","LMA_z","LDMC_z")],method="euclidean")
## forcing trait distances to be between 0 and 1
trait_dist<-(trait_dist-min(trait_dist))/(max(trait_dist)-min(trait_dist))

####################################
## Scheiner FTD scripts

source("C:/Users/kotha020/Documents/GitHub/DecomposingFD/R/AlphaFD.R")

## apply Sam Scheiner's metrics
FTD_list<-FTD.comm(tdmat=trait_dist,
                   spmat=sp_matrix_wide,
                   abund=T,
                   q=1)

FTD_df<-FTD_list[[1]]

FTD_name_split<-strsplit(rownames(FTD_df),split = "-")
FTD_df$plot_id<-unlist(lapply(FTD_name_split,function(x) x[[1]]))
FTD_df$project<-unlist(lapply(FTD_name_split,function(x) x[[2]]))

###########################
## Etienne's FD

## abundance-weighted metrics
FD_list<-dbFD(x = trait_dist,
              a = sp_matrix_wide,
              w.abun = T)

FD_df<-data.frame(names=names(FD_list$nbsp),
                  nsp=FD_list$nbsp,
                  sing.sp=FD_list$nbsp,
                  FRic=FD_list$FRic,
                  FEve=FD_list$FEve,
                  FDiv=FD_list$FDiv,
                  FDis=FD_list$FDis,
                  RaoQ=FD_list$RaoQ)

################################
## additional processing

## check: are the rows arranged the same way?
sum(!(FD_df$names==rownames(FTD_df)))

## combine data
combined_df<-data.frame(FTD_df,
                        FD_df,
                        coverage=sp_coverage)
combined_df$nsp.1<-NULL

###################################
## write_data

write.csv(combined_df,"functional_diversity.csv")
