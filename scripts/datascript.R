## run the data script here ##
## this script manipulates raw data into derived data objects for analysis ##
options(stringsAsFactors = TRUE)
derivedDir <- "./data/derived_data/"
derivedFiles <- list("j.df.rda","j.stoic_df.rda", "j.summary_stoic.rda","GB_summary.rda","GB_samples.rda")
fullPaths <- unlist(lapply(derivedFiles, function(x) paste0(derivedDir,x)))
################Loading in the BOM files############################
##load in data sheets##
if(all(file.exists(fullPaths))){
  lapply(fullPaths, load, envir = .GlobalEnv)
} else {
stream_meta <- read.csv(file="./data/raw_data/stream_metadata.csv",header = TRUE,stringsAsFactors = FALSE)#stream metadata sheet
nutrients = read.csv(file = "./data/raw_data/jul_nutrients.csv", header = TRUE, stringsAsFactors = FALSE)
##
vols <- read.csv(file = "./data/raw_data/BOM_volumes.csv", header = TRUE, stringsAsFactors = FALSE)

DM <- read.csv(file="./data/raw_data/DM.csv",header = TRUE, stringsAsFactors = FALSE)

cutC<- read.csv(file="./data/raw_data/cutC.csv",header =TRUE, stringsAsFactors = FALSE)

PerP <- read.csv(file="./data/raw_data/PerP.csv", header = TRUE, stringsAsFactors = FALSE)

PerCN <- read.csv(file="./data/raw_data/PerCN.csv", header = TRUE, stringsAsFactors = FALSE)

######### Manipulating the DM data file to fill in NAs will mean data   #########
#Filling in those with just orgmatter_mg or OrgCon to complete the set.
DM %>% select(-sampleID) %>%
  mutate(orgcon_perc = ifelse(is.na(orgcon_perc) & !is.na(orgmatter_mg), orgmatter_mg/drymass_mg, orgcon_perc)) %>%
  mutate(orgmatter_mg = ifelse(!is.na(orgcon_perc) & is.na(orgmatter_mg), drymass_mg * orgcon_perc, orgmatter_mg)) %>%
  group_by(stream, ym, subtype, spp, sample) %>% 
  summarise(drymass_mg = sum(drymass_mg, na.rm = TRUE), orgmatter_mg = sum(orgmatter_mg, na.rm = TRUE), orgcon_perc = mean(orgcon_perc, na.rm = TRUE))-> DM

DM %>% 
  group_by(stream, ym, subtype, spp) %>%
  summarise(meanorgcon_perc = mean(orgcon_perc, na.rm = TRUE)) -> OrgCon_sum1

DM %>%
  left_join(OrgCon_sum1) %>%
  mutate(orgcon_perc = coalesce(orgcon_perc, meanorgcon_perc)) %>%
  select_(~-meanorgcon_perc) -> DM_mod

DM %>% 
  group_by(ym, subtype, spp) %>%
  summarise(meanorgcon_perc = mean(orgcon_perc, na.rm = TRUE)) -> OrgCon_sum2

DM_mod %>%
  left_join(OrgCon_sum2) %>%
  mutate(orgcon_perc = coalesce(orgcon_perc, meanorgcon_perc)) %>%
  select_(~-meanorgcon_perc) -> DM_mod

DM %>%
  group_by(stream, ym, subtype) %>%
  summarise(meanorgcon_perc = mean(orgcon_perc, na.rm = TRUE)) -> OrgCon_sum3

DM_mod %>%
  left_join(OrgCon_sum3) %>%
  mutate(orgcon_perc = coalesce(orgcon_perc, meanorgcon_perc)) %>%
  select_(~-meanorgcon_perc) %>%
  mutate(orgmatter_mg = ifelse(is.na(orgmatter_mg) | orgmatter_mg == 0 & !is.na(orgcon_perc), orgcon_perc*drymass_mg, orgmatter_mg)) %>%
  full_join(vols %>% select_(~-sampleID)) -> DM_full

drop_cols = c('drymass_mg','orgmatter_mg','depth_cm','totvol_mL','area_m','elutvol_mL','subelut_mL','scrubvol_mL','subscrub_mL')
DM_full %>%
  mutate(totdrymass_mg = ifelse(subtype == "cut>1", drymass_mg/area_m, 
                         ifelse(subtype == "elut<1", (drymass_mg * (totvol_mL/subelut_mL))/area_m, 
                                ifelse(subtype == "scrub<1", (drymass_mg *(scrubvol_mL/subscrub_mL))/area_m, 
                                       ifelse(subtype == "elut>1",(drymass_mg *(totvol_mL/elutvol_mL))/area_m,
                                              ifelse(subtype == "scrub>1",drymass_mg/area_m, NA)))))) %>%
  mutate(totorgmatter_mg = ifelse(subtype == "cut>1", orgmatter_mg/area_m, 
                         ifelse(subtype == "elut<1", (orgmatter_mg * (totvol_mL/subelut_mL))/area_m, 
                                ifelse(subtype == "scrub<1", (orgmatter_mg *(scrubvol_mL/subscrub_mL))/area_m, 
                                       ifelse(subtype == "elut>1",(orgmatter_mg *(totvol_mL/elutvol_mL))/area_m,
                                              ifelse(subtype == "scrub>1",orgmatter_mg/area_m, NA)))))) %>%
  select_(~-one_of(drop_cols))-> DM_full
#need to check this if we do any date besides 2012-07. There are a couple missing 
############################################   Merge DM_full with Chemistry to get tot ecosystem stoic   ####################################
##################### Pulling together the Chemistry data for ES data  #################
#####################################    Chemistry/CNP of samples    ################################################
##USE DM.full for ES chemistry data
## USE esC with DM.full merge to calculate the PerX of organic matter
#head(esC)
#head(DM.full)
PerCN %>% select_(~-sampleID) %>% dplyr::rename(CNwt_mg = samplewt_mg) %>%
  group_by(stream, ym, subtype, sample, spp) %>% 
  summarise(perC = mean(perC, na.rm = TRUE), perN = mean(perN, na.rm = TRUE), CNwt_mg = mean(CNwt_mg,na.rm = TRUE)) -> PerCNmod

PerP %>% select_(~ -sampleID) %>%  dplyr::rename(Pwt_mg = samplewt_mg) %>%
  group_by(stream, ym, subtype, sample, spp) %>% 
  summarise(perP = mean(perP, na.rm = TRUE), Pwt_mg = mean(Pwt_mg, na.rm = TRUE)) -> PerPmod

PerCNmod %>%
  full_join(PerPmod %>% filter(subtype != "cut>1")) ->esCmod

esCmod %>%
  left_join(DM_full %>% select_(~-totdrymass_mg, ~-totorgmatter_mg)) -> esCmod

esCmod %>% left_join(DM %>%
                       group_by(stream, ym, subtype, spp) %>%
                       summarise(meanorgcon_perc = mean(orgcon_perc, na.rm = TRUE))) %>%
  mutate(orgcon_perc = coalesce(orgcon_perc, meanorgcon_perc)) %>%
  select_(~-meanorgcon_perc) -> esCmod

# adjust BOM samples for inorganic mass and recalc nutrient content #
esCmod %>% mutate(C_mg = (perC/100)*CNwt_mg, N_mg = (perN/100)*CNwt_mg, P_mg = (perP/100)*Pwt_mg) %>%
  mutate(CN_OM = CNwt_mg*orgcon_perc, P_OM = Pwt_mg * orgcon_perc) %>%
  mutate(perC = (C_mg/CN_OM)*100, perN = (N_mg/CN_OM)*100, perP = (P_mg/P_OM)*100) %>%
  select_(~-C_mg, ~-N_mg, ~-P_mg, ~-CN_OM, ~-P_OM, ~-CNwt_mg, ~-Pwt_mg, ~-orgcon_perc) -> esChem
##### QAQC for the adjusted CNP ######
## remove the top tails ##
c100_fix = which(esChem$perC > 85)
esChem[c100_fix, 'perC'] = NA
n25_fix = which(esChem$perN > 25)
esChem[n25_fix, 'perN'] = NA
p6_fix = which(esChem$perP > 6)
esChem[p6_fix, 'perP'] = NA

##################### Pulling together the Chemistry data for all data  #################
DM_full %>% left_join(esChem) -> DM_full

cutC %>% select_(~ -sampleID) %>% group_by(stream, ym, subtype, sample, spp) %>%
  summarise(meanperC = mean(perC, na.rm = TRUE), meanperN = mean(perN, na.rm = TRUE), meanperP = mean(perP, na.rm = TRUE))-> cutC

DM_full %>% left_join(cutC) %>% 
  mutate(perC = coalesce(perC, meanperC), perN = coalesce(perN, meanperN), perP = coalesce(perP, meanperP)) %>%
  select_(~-meanperC, ~-meanperN, ~-meanperP)-> DM_full

#####################################    Chemistry/CNP of samples ################################################
####Filling in the NAs with mean data #####
# create a big OM chemistry df for all samples #

esChem %>% dplyr::rename(meanperC = perC, meanperN = perN, meanperP = perP) %>%
  bind_rows(cutC) -> chem_full

chem_full %>%
  group_by(ym, stream, sample, subtype, spp) %>%
  summarise(summC = mean(meanperC, na.rm = TRUE), summN = mean(meanperN, na.rm = TRUE), summP = mean(meanperP, na.rm = TRUE)) -> chem_summ1

DM_full %>% left_join(chem_summ1) %>%
  mutate(perC = coalesce(perC, summC), perN = coalesce(perN, summN),perP = coalesce(perP, summP)) %>%
  select_(~-summC, ~-summN, ~-summP) -> DM_full

chem_full %>%  
  group_by(ym, stream, subtype, spp) %>%
  summarise(summC = mean(meanperC, na.rm = TRUE), summN = mean(meanperN, na.rm = TRUE), summP = mean(meanperP, na.rm = TRUE)) -> chem_summ2

DM_full %>% left_join(chem_summ2)  %>%
  mutate(perC = coalesce(perC, summC), perN = coalesce(perN, summN),perP = coalesce(perP, summP)) %>%
  select_(~-summC, ~-summN, ~-summP) -> DM_full

chem_full%>%
  group_by(stream, subtype, spp) %>% 
  summarise(summC = mean(meanperC, na.rm = TRUE), summN = mean(meanperN, na.rm = TRUE), summP = mean(meanperP, na.rm = TRUE)) -> chem_summ3

DM_full %>% left_join(chem_summ3)  %>%
  mutate(perC = coalesce(perC, summC), perN = coalesce(perN, summN),perP = coalesce(perP, summP)) %>%
  select_(~-summC, ~-summN, ~-summP) -> DM_full

chem_full%>%
  group_by(spp)%>%
  summarise(summC = mean(meanperC, na.rm = TRUE), summN = mean(meanperN, na.rm = TRUE), summP = mean(meanperP, na.rm = TRUE)) -> chem_summ4

DM_full %>% left_join(chem_summ4)  %>%
  mutate(perC = coalesce(perC, summC), perN = coalesce(perN, summN), perP = coalesce(perP, summP)) %>%
  select_(~-summC, ~-summN, ~-summP) -> df_full

#set wood to 50%C, 0.036%N, & 0.005%P by smyth et al. 2016 Plant and Soil. DOI: 10.1007/s11104-016-2972-4
# wood.fix = which(df_full$spp == 'Wood')
# df_full[wood.fix, c('PerC','PerN','PerP')] = c(50,0.036,0.005)
df_full <- df_full %>%
  mutate(perC = if_else(spp == "Wood", 50, perC),
         perN = if_else(spp == "Wood", 0.036, perN),
         perP = if_else(spp == "Wood", 0.005, perP))
########################################
#make CNP totals for samples next #remember to multiply by DM for cut and OM for elut/scrub
df_full %>%
  mutate(totC_mg = ifelse(subtype == 'cut>1', totdrymass_mg*(perC/100),totorgmatter_mg*(perC/100)), 
         totN_mg = ifelse(subtype == 'cut>1', totdrymass_mg*(perN/100),totorgmatter_mg*(perN/100)),
         totP_mg = ifelse(subtype == 'cut>1', totdrymass_mg*(perP/100),totorgmatter_mg*(perP/100))) -> df_full


# sum totals for  for samples
# this combines subtypes to full sample mass and stoic

stoic.df <- df_full %>%
  group_by(stream, ym, sample) %>%
  summarise(sumdrymass_mg = sum(totdrymass_mg, na.rm = TRUE), sumorgmatter_mg = sum(totorgmatter_mg, na.rm = TRUE), sumC_mg = sum(totC_mg, na.rm = TRUE), sumN_mg = sum(totN_mg, na.rm = TRUE), sumP_mg = sum(totP_mg, na.rm = TRUE)) %>%
  mutate(CNeco_molar = (sumC_mg/sumN_mg)*(14.007/12.011), CPeco_molar = (sumC_mg/sumP_mg) *(30.9737/12.011), NPeco_molar = (sumN_mg/sumP_mg)*(30.9737/14.007)) ###Final stoic file

###summary_stoicfile summarise mean of ecosystems ###
summary_stoic <- stoic.df %>%
  group_by(stream, ym) %>%
  summarise(meandrymass_mg = mean(sumdrymass_mg, na.rm = TRUE), meanorgmatter_mg = mean(sumorgmatter_mg, na.rm = TRUE), meanC_mg = mean(sumC_mg, na.rm = TRUE),
            meanN_mg = mean(sumN_mg, na.rm = TRUE), meanP_mg = mean(sumP_mg, na.rm = TRUE), meanCNeco_molar = mean(CNeco_molar, na.rm = TRUE), 
            meanCPeco_molar = mean(CPeco_molar, na.rm = TRUE), meanNPeco_molar = mean(NPeco_molar, na.rm = TRUE), sedrymass_mg = se(sumdrymass_mg), 
            seorgmatter_mg = se(sumorgmatter_mg), seC_mg = se(sumC_mg), seN_mg = se(sumN_mg), seP_mg = se(sumP_mg), seCNeco_molar = se(CNeco_molar), 
            seCPeco_molar = se(CPeco_molar), seNPeco_molar = se(NPeco_molar), varCNeco_molar = var(CNeco_molar, na.rm = TRUE), varCPeco_molar = var(CPeco_molar, na.rm = TRUE), 
            varNPeco_molar = var(NPeco_molar, na.rm = TRUE))

options(stringsAsFactors = FALSE, max.print = 100000)

##### subset full year data to just summer ####

j.stoic_df <- stoic.df[which(stoic.df$ym == "2012-07"),]
j.summary_stoic <- summary_stoic[which(summary_stoic$ym == "2012-07"),]
#calculate molar ratios
##this is derived subtype-level mass & stoic
j.df <- df_full[which(df_full$ym == "2012-07"),] %>%
  group_by(stream, ym, spp, sample) %>%
  mutate(CN_molar = (totC_mg/totN_mg)*(14.007/12.011), CP_molar = (totC_mg/totP_mg)*(30.9737/12.011),
         NP_molar = (totN_mg/totP_mg)*(30.9737/14.007))


####### separate out broad OM categories ########
#isolate the stoichiometry of 'green' and 'brown' pools#####
j.df$G_B = "Green"
CBOM_fix = which(j.df$subtype == "elut>1")
FBOM_fix = which(j.df$subtype == "elut<1")
wood_fix = which(j.df$spp == "Wood")
# j.df$spp = as.character(j.df$spp)

j.df[CBOM_fix, "spp"] = "CBOM"
j.df[FBOM_fix, "spp"] = "FBOM"

j.df[CBOM_fix, "G_B"] = "Brown"
j.df[FBOM_fix, "G_B"] = "Brown"
j.df[wood_fix, "G_B"] = 'Brown'

# sum subtypes to sample-level broad characterization #
GB_samples<- j.df %>% group_by(stream, G_B, sample) %>%
  summarise( sumorgmatter_mg = sum(totorgmatter_mg), sumC_mg = sum(totC_mg, na.rm = TRUE), sumN_mg = sum(totN_mg, na.rm = TRUE), sumP_mg = sum(totP_mg, na.rm = TRUE)) 

## summarise ecosystem-level broad characterization #
GB_summary <- GB_samples %>% group_by(stream, G_B) %>%
  summarise(meanorgmatter_mg = mean(sumorgmatter_mg, na.rm = TRUE), seorgmatter_mg = se(sumorgmatter_mg), 
            meanC_mg = mean(sumC_mg, na.rm = TRUE), seC_mg = se(sumC_mg), meanN_mg = mean(sumN_mg, na.rm = TRUE), seN_mg = se(sumN_mg),
            meanP_mg = mean(sumP_mg, na.rm = TRUE), seP_mg = se(sumP_mg))

##widen the summary out for OM total
GB_summary.wide <- GB_summary %>% select_(~stream, ~G_B, ~meanorgmatter_mg) %>%
  spread(key = G_B, value = meanorgmatter_mg, drop = T) 

##### End Green/Brown separation ######

##rename 'scrub' subtypes to characterize as epilithic 
CBOM_fix2 = which(j.df$subtype == "scrub>1" & j.df$stream == "st11U")
j.df[CBOM_fix2, "spp"] = "CBOMe"

CBOM_fix2 = which(j.df$subtype == "scrub>1" & j.df$stream == "st11L")
j.df[CBOM_fix2, "spp"] = "CBOMe"

CBOM_fix2 = which(j.df$subtype == "scrub>1" & j.df$stream == "st14")
j.df[CBOM_fix2, "spp"] = "CBOMe"

CBOM_fix2 = which(j.df$subtype == "scrub>1" & j.df$stream == "hver")
j.df[CBOM_fix2, "spp"] = "CBOMe"

FBOM_fix2 = which(j.df$subtype == "scrub<1" & j.df$stream == "hver")
j.df[FBOM_fix2, "spp"] = "FBOMe"

FBOM_fix2 = which(j.df$subtype == "scrub<1" & j.df$stream == "st11U")
j.df[FBOM_fix2, "spp"] = "FBOMe"

FBOM_fix2 = which(j.df$subtype == "scrub<1" & j.df$stream == "st11L")
j.df[FBOM_fix2, "spp"] = "FBOMe"

FBOM_fix2 = which(j.df$subtype == "scrub<1" & j.df$stream == "st14")
j.df[FBOM_fix2, "spp"] = "FBOMe"

CBOM_fix2 = which(j.df$subtype == 'scrub>1')
j.df[CBOM_fix2,'spp'] = 'CBOMe'

FBOM_fix2 = which(j.df$subtype == 'scrub<1')
j.df[FBOM_fix2,'spp'] = 'FBOMe'

j.df <<- j.df
#j.df = data.frame(unclass(j.df));j.df$spp = factor(j.df$spp)
rm("CBOM_fix","CBOM_fix2","FBOM_fix","FBOM_fix2")

# summarize streams by species attributes #
j.df.comm <- j.df %>%
  group_by(stream, ym, spp) %>%
  summarise(meanorgmatter_mg = mean(totorgmatter_mg, na.rm = TRUE), meanperC = mean(perC, na.rm = TRUE), 
            meanperN = mean(perN, na.rm = TRUE), meanperP = mean(perP, na.rm = TRUE), meanCN_molar = mean(CN_molar, na.rm = TRUE),
            meanCP_molar = mean(CP_molar, na.rm = TRUE), meanNP_molar = mean(NP_molar, na.rm = TRUE), 
            meanC_mg = mean(totC_mg, na.rm = TRUE), meanN_mg = mean(totN_mg, na.rm = TRUE), meanP_mg = mean(totP_mg, na.rm = TRUE))

j.df.comm %>%
  left_join(stream_meta, by = c("stream")) %>%
  left_join(nutrients, by = c("stream","ym")) %>%
  group_by(stream, ym, spp) %>%
  mutate(nflux_mg_m2_s = (medianQ_L_s*(no3.nh4_ug_L/1000))/((width_cm/100)*length_m), 
         pflux_mg_m2_s = (medianQ_L_s*(srp_ug_L/1000))/((width_cm/100)*length_m)) -> j.df.comm

j.summary_stoic %>%
  left_join(stream_meta, by = c("stream")) %>%
  left_join(nutrients, by = c("stream","ym")) %>%
  group_by(stream, ym) %>%
  mutate(h2oN = depth_m*no3.nh4_ug_L, h2oP = depth_m*srp_ug_L, nflux_mg_d = (medianQ_L_s*86400*(no3.nh4_ug_L/1000))/((width_cm/100)*length_m), 
         pflux_mg_d = (medianQ_L_s*86400*(srp_ug_L/1000))/((width_cm/100)*length_m)) %>%
  mutate(h2oNP = (no3.nh4_ug_L/srp_ug_L) * (30.9737/14.007)) %>%
  mutate(NPimb = meanNPeco_molar - h2oNP) -> j.summary_stoic

j.stoic_df %>%
  left_join(stream_meta, by = c("stream")) %>%
  left_join(nutrients, by = c("stream","ym")) %>%
  group_by(stream, ym) %>%
  mutate(h2oNP = (no3.nh4_ug_L/srp_ug_L)*(30.9737/14.007)) ->> j.stoic_df


j.summary_stoic <<- merge(j.summary_stoic, GB_summary.wide, by = "stream")

GB_summary %>% 
  left_join(stream_meta) ->> GB_summary
}
##### Load data objects for plots #####
load(file = "./data/ocecolors.rda", envir = .GlobalEnv)
theme_set(theme_mod)
#################### End data manipulation code ######################
rm(list = ls()[!ls() %in% c("j.df", "j.stoic_df","j.summary_stoic", "GB_samples",
                            "GB_summary", "se","ci","pred_plot","preddat_fun",
                            "blackwhite","colors", "theme_mod","theme_black")])
