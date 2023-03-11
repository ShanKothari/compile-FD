library(spectrolab)

setwd("C:/Users/querc/Dropbox/PostdocProjects/WallisFunctionalTraits/")

trait_summ<-read.csv("TraitsSpecies/Species_traits_by_projects_allPriorities.csv")
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
trait_sub$LMA_CABO<-trait_sub$trait_leaf_mass_per_area_g_m2
trait_sub$LMA_TRYsp<-trait_sub$trait_leaf_mass_per_area_g_m2
trait_sub$LMA_TRYgn<-trait_sub$trait_leaf_mass_per_area_g_m2
trait_sub$LDMC_CABO<-trait_sub$trait_leaf_dry_matter_content_mg_g
trait_sub$LDMC_TRYsp<-trait_sub$trait_leaf_dry_matter_content_mg_g
trait_sub$LDMC_TRYgn<-trait_sub$trait_leaf_dry_matter_content_mg_g

###################################
## read CABO data

ref_traits<-readRDS("../FreshLeafModels/ProcessedSpectra/all_ref_and_traits.rds")
ref_meta<-meta(ref_traits)

ref_meta_agg<-aggregate(ref_meta[,c("LMA","LDMC","Nmass")],
                        by=list(ref_meta$species),
                        FUN=mean,na.rm=T)

ref_meta_agg_sp<-strsplit(as.character(ref_meta_agg$Group.1),split=" ")
ref_meta_agg$genus<-unlist(lapply(ref_meta_agg_sp,function(el) el[[1]]))
ref_meta_agg$species<-unlist(lapply(ref_meta_agg_sp,function(el) el[[2]]))
ref_meta_agg$binomial<-paste(ref_meta_agg$genus,ref_meta_agg$species,sep=" ")

###################################
## process additional SLA data from TRY

# ## SLA with petiole, 3116 (TRY request 25397)
# TRY_SLA_petiole<-read.table("TRY/TRYSLApetiole/25397.txt",
#                       sep = "\t",fill=T,header=T,quote="",fileEncoding="latin1")
# 
# TRY_SLAp_sub<-TRY_SLA_petiole[which(TRY_SLA_petiole$TraitID %in% c(3116)),]
# TRY_SLAp_sub$StdValue<-as.numeric(as.character(TRY_SLAp_sub$StdValue))
# TRY_SLAp_sub$ErrorRisk<-as.numeric(as.character(TRY_SLAp_sub$ErrorRisk))
# TRY_SLAp_sub<-TRY_SLAp_sub[!is.na(TRY_SLAp_sub$StdValue),]
# TRY_SLAp_sub<-TRY_SLAp_sub[which(TRY_SLAp_sub$ErrorRisk<3),]
# TRY_SLAp_sub<-TRY_SLAp_sub[,c("DatasetID","SpeciesName","AccSpeciesID","ObservationID",
#                             "ObsDataID","TraitID","TraitName","StdValue","UnitName","ErrorRisk")]
# 
# write.csv(TRY_SLAp_sub,"TRY/TRY_SLAp.csv")
# 
# ## SLA indeterminate about petiole, 3117 (TRY request 25398)
# TRY_SLA_ind<-read.table("TRY/TRYSLAindeterminate/25398.txt",
#                             sep = "\t",fill=T,header=T,quote="",fileEncoding="latin1")
# 
# TRY_SLAi_sub<-TRY_SLA_ind[which(TRY_SLA_ind$TraitID %in% c(3117)),]
# TRY_SLAi_sub$StdValue<-as.numeric(as.character(TRY_SLAi_sub$StdValue))
# TRY_SLAi_sub$ErrorRisk<-as.numeric(as.character(TRY_SLAi_sub$ErrorRisk))
# TRY_SLAi_sub<-TRY_SLAi_sub[!is.na(TRY_SLAi_sub$StdValue),]
# TRY_SLAi_sub<-TRY_SLAi_sub[which(TRY_SLAi_sub$ErrorRisk<3),]
# TRY_SLAi_sub<-TRY_SLAi_sub[,c("DatasetID","SpeciesName","AccSpeciesID","ObservationID",
#                               "ObsDataID","TraitID","TraitName","StdValue","UnitName","ErrorRisk")]
# 
# write.csv(TRY_SLAi_sub,"TRY/TRY_SLAi.csv")

###################################
## read processed TRY data

TRY_N<-read.csv("../FreshLeafModels/TraitData/TRY/TRY_N.csv")
TRY_N<-TRY_N[-which(TRY_N$SpeciesName==""),]
TRY_N_sp<-strsplit(as.character(TRY_N$SpeciesName),split=" ")
TRY_N$genus<-unlist(lapply(TRY_N_sp,function(el) el[[1]]))
TRY_N$species<-unlist(lapply(TRY_N_sp,function(el){
  if(length(el)>1){
    return(el[[2]])
  } else{
    return("")
  }}))
TRY_N$binomial<-paste(TRY_N$genus,TRY_N$species,sep=" ")

TRY_SLA_np<-read.csv("../FreshLeafModels/TraitData/TRY/TRY_SLA.csv")
TRY_SLA_p<-read.csv("TRY/TRY_SLAp.csv")
TRY_SLA_i<-read.csv("TRY/TRY_SLAi.csv")
TRY_SLA<-do.call(rbind,args=list(TRY_SLA_np,TRY_SLA_p,TRY_SLA_i))

TRY_SLA<-TRY_SLA[-which(TRY_SLA$SpeciesName==""),]
TRY_SLA_sp<-strsplit(as.character(TRY_SLA$SpeciesName),split=" ")
TRY_SLA$genus<-unlist(lapply(TRY_SLA_sp,function(el) el[[1]]))
TRY_SLA$species<-unlist(lapply(TRY_SLA_sp,function(el){
  if(length(el)>1){
    return(el[[2]])
  } else{
    return("")
  }}))
TRY_SLA$binomial<-paste(TRY_SLA$genus,TRY_SLA$species,sep=" ")

TRY_LDMC<-read.csv("../FreshLeafModels/TraitData/TRY/TRY_LDMC.csv")
TRY_LDMC_sp<-strsplit(as.character(TRY_LDMC$SpeciesName),split=" ")
TRY_LDMC$genus<-unlist(lapply(TRY_LDMC_sp,function(el) el[[1]]))
TRY_LDMC$species<-unlist(lapply(TRY_LDMC_sp,function(el){
  if(length(el)>1){
    return(el[[2]])
  } else{
    return("")
  }}))
TRY_LDMC$binomial<-paste(TRY_LDMC$genus,TRY_LDMC$species,sep=" ")

#########################################
## attach data to matrix

for(i in 1:nrow(trait_sub)){
  
  CABO_match<-match(trait_sub$scientific_name[i],ref_meta_agg$Group.1)
  if(is.na(CABO_match)){
    CABO_match<-match(trait_sub$binomial[i],ref_meta_agg$binomial)
  }
  
  ## if N is missing, fill from CABO if possible
  if(is.na(trait_sub$trait_n_perc[i]) & !is.na(CABO_match)){
    trait_sub$N_CABO[i]<-ref_meta_agg$Nmass[CABO_match]
    trait_sub$N_TRYsp[i]<-ref_meta_agg$Nmass[CABO_match]
    trait_sub$N_TRYgn[i]<-ref_meta_agg$Nmass[CABO_match]
  }
  
  ## if LMA is missing, fill from CABO if possible
  if(is.na(trait_sub$trait_leaf_mass_per_area_g_m2[i]) & !is.na(CABO_match)){
    trait_sub$LMA_CABO[i]<-ref_meta_agg$LMA[CABO_match]*1000
    trait_sub$LMA_TRYsp[i]<-ref_meta_agg$LMA[CABO_match]*1000
    trait_sub$LMA_TRYgn[i]<-ref_meta_agg$LMA[CABO_match]*1000
  }
  
  ## if LDMC is missing, fill from CABO if possible
  if(is.na(trait_sub$trait_leaf_dry_matter_content_mg_g[i]) & !is.na(CABO_match)){
    trait_sub$LDMC_CABO[i]<-ref_meta_agg$LDMC[CABO_match]
    trait_sub$LDMC_TRYsp[i]<-ref_meta_agg$LDMC[CABO_match]
    trait_sub$LDMC_TRYgn[i]<-ref_meta_agg$LDMC[CABO_match]
  }

  ## now if N_TRYsp is still missing we fill from TRY
  if(is.na(trait_sub$N_TRYsp[i])){
    TRY_N_matchsp<-which(TRY_N$binomial==trait_sub$binomial[i])
    trait_sub$N_TRYsp[i]<-mean(TRY_N$StdValue[TRY_N_matchsp]/10,na.rm=T)
    trait_sub$N_TRYgn[i]<-mean(TRY_N$StdValue[TRY_N_matchsp]/10,na.rm=T)
  }
  
  ## now if LMA_TRYsp is still missing we fill from TRY
  if(is.na(trait_sub$LMA_TRYsp[i])){
    TRY_SLA_matchsp<-which(TRY_SLA$binomial==trait_sub$binomial[i])
    trait_sub$LMA_TRYsp[i]<-mean(1000/TRY_SLA$StdValue[TRY_SLA_matchsp],na.rm=T)
    trait_sub$LMA_TRYgn[i]<-mean(1000/TRY_SLA$StdValue[TRY_SLA_matchsp],na.rm=T)
  }
  
  ## now if LDMC_TRYsp is still missing we fill from TRY
  if(is.na(trait_sub$LDMC_TRYsp[i])){
    TRY_LDMC_matchsp<-which(TRY_LDMC$binomial==trait_sub$binomial[i])
    trait_sub$LDMC_TRYsp[i]<-mean(1000*TRY_LDMC$StdValue[TRY_LDMC_matchsp],na.rm=T)
    trait_sub$LDMC_TRYgn[i]<-mean(1000*TRY_LDMC$StdValue[TRY_LDMC_matchsp],na.rm=T)
  }
  
  ## now if N_TRYgn is still missing we fill from TRY
  if(is.na(trait_sub$N_TRYgn[i])){
    TRY_N_matchgn<-which(TRY_N$genus==trait_sub$genus[i])
    trait_sub$N_TRYgn[i]<-mean(TRY_N$StdValue[TRY_N_matchgn]/10,na.rm=T)
  }
  
  ## now if LMA_TRYgn is still missing we fill from TRY
  if(is.na(trait_sub$LMA_TRYgn[i])){
    TRY_SLA_matchgn<-which(TRY_SLA$genus==trait_sub$genus[i])
    trait_sub$LMA_TRYgn[i]<-mean(1000/TRY_SLA$StdValue[TRY_SLA_matchgn],na.rm=T)
  }
  
  ## now if LDMC_TRYgn is still missing we fill from TRY
  if(is.na(trait_sub$LDMC_TRYgn[i])){
    TRY_LDMC_matchgn<-which(TRY_LDMC$genus==trait_sub$genus[i])
    trait_sub$LDMC_TRYgn[i]<-mean(1000*TRY_LDMC$StdValue[TRY_LDMC_matchgn],na.rm=T)
  }
}

#################################################
## manual fill-in

trait_sub$N_all<-trait_sub$N_TRYgn
trait_sub$LMA_all<-trait_sub$LMA_TRYgn
trait_sub$LDMC_all<-trait_sub$LDMC_TRYgn

## Amy Heim's dissertation
## https://library2.smu.ca/handle/01/29843
trait_sub$LDMC_all[trait_sub$binomial=="Aronia melanocarpa"]<-375.8

## https://academic.oup.com/aob/article/118/6/1139/2418679
## digitized from graph in July; sterile fronds only
trait_sub$N_all[trait_sub$binomial=="Osmundastrum cinnamomeum"]<-2.551

## https://www.sciencedirect.com/science/article/pii/S0048969720321355
## calculated from control and 0 treatment in data
trait_sub$N_all[trait_sub$binomial=="Scheuchzeria palustris"]<-2.879

## https://www.sciencedirect.com/science/article/pii/S0304423816304216
## based on lowest level to minimize effects of fertilization
trait_sub$N_all[trait_sub$binomial=="Valerianella locusta"]<-3.608

## https://link.springer.com/article/10.1007/s00442-006-0619-5
trait_sub$LMA_all[trait_sub$binomial=="Sphagnum capillifolium"]<-10000/463.6
# based on riparium
trait_sub$LMA_all[trait_sub$binomial=="Sphagnum fallax"]<-10000/357.6
# based on an overall average
trait_sub$LMA_all[trait_sub$binomial=="Sphagnum papillosum"]<-10000/356.1
trait_sub$LMA_all[trait_sub$binomial=="Sphagnum magellanicum"]<-10000/356.1

## doi:10.1111/1365-2435.13883
trait_sub$LDMC_all[trait_sub$binomial=="Sphagnum fallax"]<-1/(8.42+1)*1000
trait_sub$LDMC_all[trait_sub$binomial=="Sphagnum magellanicum"]<-1/(16.04+1)*1000
trait_sub$LDMC_all[trait_sub$binomial=="Sphagnum papillosum"]<-1/(10.26+1)*1000
## based on fuscum
trait_sub$LDMC_all[trait_sub$binomial=="Sphagnum capillifolium"]<-1/(8.96+1)*1000
## capillifolium sect. acutifolia?? related to fuscum

## LMA Polytrichum
## https://www.sciencedirect.com/science/article/pii/S0269749109004680

######################################
## write data

write.csv(trait_sub,"filled_trait_matrix.csv",row.names=F)
