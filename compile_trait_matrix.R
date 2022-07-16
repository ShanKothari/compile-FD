library(spectrolab)

setwd("C:/Users/kotha020/Dropbox/PostdocProjects/WallisFunctionalTraits/")

trait_summ<-read.csv("TraitsSpecies/Species_traits_by_projects.csv")
trait_sub<-trait_summ[,c("project",
                         "scientific_name",
                         "trait_leaf_mass_per_area_g_m2",
                         "trait_leaf_dry_matter_content_mg_g",
                         "trait_n_perc")]

trait_sub$scientific_name<-as.character(trait_sub$scientific_name)
trait_sub$scientific_name[trait_sub$scientific_name=="Abildgaardieae"]<-"Abildgaardieae Ignore"

trait_sp_split<-strsplit(trait_sub$scientific_name,split=" ")
trait_sub$genus<-unlist(lapply(trait_sp_split,function(el) el[[1]]))
trait_sub$species<-unlist(lapply(trait_sp_split,function(el) el[[2]]))
trait_sub$species[which(trait_sub$species!=tolower(trait_sub$species))]<-NA
trait_sub$binomial<-paste(trait_sub$genus,trait_sub$species,sep=" ")

trait_sub$N_CABO<-trait_sub$trait_n_perc
trait_sub$N_TRYsp<-trait_sub$trait_n_perc
trait_sub$N_TRYgn<-trait_sub$trait_n_perc
trait_sub$LMA_CABO<-trait_sub$trait_LMA_perc
trait_sub$LMA_TRYsp<-trait_sub$trait_LMA_perc
trait_sub$LMA_TRYgn<-trait_sub$trait_LMA_perc
trait_sub$LDMC_CABO<-trait_sub$trait_LDMC_perc
trait_sub$LDMC_TRYsp<-trait_sub$trait_LDMC_perc
trait_sub$LDMC_TRYgn<-trait_sub$trait_LDMC_perc

###################################
## read CABO data

ref_traits<-readRDS("../FreshLeafModels/ProcessedSpectra/all_ref_and_traits.rds")
ref_meta<-meta(ref_traits)

ref_meta_agg<-aggregate(ref_meta[,c("LMA","LDMC","Nmass")],
                        by=list(ref_meta$species),
                        FUN=mean,na.rm=T)

###################################
## read TRY data

TRY_N<-read.csv("../FreshLeafModels/TraitData/TRY/TRY_N.csv")
TRY_N<-TRY_N[-which(TRY_N$SpeciesName==""),]
TRY_N_sp<-strsplit(as.character(TRY_N$SpeciesName),split=" ")
TRY_N$genus<-unlist(lapply(TRY_N_sp,function(el) el[[1]]))
TRY_N$species<-unlist(lapply(TRY_N_sp,function(el){
  if(length(el)>1){
    return(el[[2]])
  } else{
    return(NA)
  }}))

TRY_SLA<-read.csv("../FreshLeafModels/TraitData/TRY/TRY_SLA.csv")
TRY_SLA_sp<-strsplit(as.character(TRY_SLA$SpeciesName),split=" ")
TRY_SLA$genus<-unlist(lapply(TRY_SLA_sp,function(el) el[[1]]))
TRY_SLA$species<-unlist(lapply(TRY_SLA_sp,function(el){
  if(length(el)>1){
    return(el[[2]])
  } else{
    return(NA)
  }}))

TRY_LDMC<-read.csv("../FreshLeafModels/TraitData/TRY/TRY_LDMC.csv")
TRY_LDMC_sp<-strsplit(as.character(TRY_LDMC$SpeciesName),split=" ")
TRY_LDMC$genus<-unlist(lapply(TRY_LDMC_sp,function(el) el[[1]]))
TRY_LDMC$species<-unlist(lapply(TRY_LDMC_sp,function(el){
  if(length(el)>1){
    return(el[[2]])
  } else{
    return(NA)
  }}))

#########################################
## attach data to matrix

for(i in 1:nrow(trait_sub)){
  
  ## fill in N
  if(is.na(trait_sub$trait_n_perc)){
    
  }
    
  ## fill in LMC
}