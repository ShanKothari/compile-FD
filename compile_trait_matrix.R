library(spectrolab)

setwd("C:/Users/kotha020/Dropbox/PostdocProjects/WallisFunctionalTraits/")

trait_summ<-read.csv("TraitsSpecies/Species_traits_by_projects.csv")
trait_sub<-trait_summ[,c("project",
                         "scientific_name",
                         "trait_leaf_mass_per_area_g_m2",
                         "trait_leaf_dry_matter_content_mg_g",
                         "trait_n_perc")]

trait_sub$scientific_name<-as.character(trait_sub$scientific_name)
trait_sp_split<-strsplit(trait_sub$scientific_name,split=" ")
trait_sub$genus<-unlist(lapply(trait_sp_split))

###################################
##