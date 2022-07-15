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

###################################
##

ref_traits<-readRDS("../FreshLeafModels/ProcessedSpectra/all_ref_and_traits.rds")
ref_meta<-meta(ref_traits)