###################### Calculating the effect size of interspecific and intraspecific processes on ecosystem stoichiometry ##################
##### Using the Price equation formulation of Teurlincx and Winfree #######
#Using the Teurlincx/Winfree code to run the Price partition
#source the price equation
#Now make pairwise environmental dataframe and then compare with price diff#####
#Create the pairwise where number of OMmass in ref_site > comparison_site
pair_function = function(DATA,...){
  streams = unique(DATA$Stream)
  
  ref_site = c()
  comparison_site = c()
  pairs = c()
  
  co = as.matrix(combn(unique(streams),2))
  #print(co)
  for(elem in 1:ncol(co)){
    x = sum(DATA[which(DATA$Stream == as.character(co[1,elem]) & DATA$C.mean > 0), 'C.mean'])
    y = sum(DATA[which(DATA$Stream == as.character(co[2,elem]) & DATA$C.mean > 0), 'C.mean'])
    
    if(x < y) {
      ref_site = c(ref_site, unique(as.character(co[2,elem])))
      comparison_site = c(comparison_site, unique(as.character(co[1,elem])))
    } else {
      # if(length(x$SPP) > length(y$SPP)){
      #   ref_site = c(ref_site, unique(as.character(x$Stream)))
      #   comparison_site = c(comparison_site, unique(as.character(y$Stream)))
      #   
      # } else{
      ref_site = c(ref_site, unique(as.character(co[1,elem])))#unique(as.character(y$Stream)))
      comparison_site = c(comparison_site, unique(as.character(co[2,elem])))#unique(as.character(x$Stream)))
    }
  }
  pairs = data.frame(ref_site, comparison_site)
  return(pairs)
}
#debugonce(pair_function)

pairs = pair_function(j.summary_stoic)
#pairs

# streams = unique(j.summary_stoic$Stream)
# temp.pair = as.matrix(dist(j.summary_stoic$mean_temp)); rownames(temp.pair) = streams; colnames(temp.pair) = streams
# slope.pair = as.matrix(dist(j.summary_stoic$Slope)); rownames(slope.pair) = streams; colnames(slope.pair) = streams
# CV.pair = as.matrix(dist(j.summary_stoic$CV)); rownames(CV.pair) = streams; colnames(CV.pair) = streams
# PC1.pair = as.matrix(dist(j.summary_stoic$PC1)); rownames(substrate.pair) = streams; colnames(substrate.pair) = streams
# PC2.pair = as.matrix(dist(j.summary_stoic$PC2)); rownames(substrate.pair) = streams; colnames(substrate.pair) = streams
# Q.pair = as.matrix(dist(j.summary_stoic$median_Q)); rownames(Q.pair) = streams; colnames(Q.pair) = streams
# no3.pair = as.matrix(dist(j.summary_stoic$no3.nh4_mg_L)); rownames(no3.pair) = streams; colnames(no3.pair) = streams
# srp.pair = as.matrix(dist(j.summary_stoic$srp_mg_L)); rownames(srp.pair) = streams; colnames(srp.pair) = streams
# 
# temp.pair["st1", "hver"]
# temp.pair["hver", "st1"]

pairwise_df = function(DATA,pairs,...){
  
  streams = unique(DATA$Stream)
  temp.pair = as.matrix(dist(DATA$mean_temp)); rownames(temp.pair) = streams; colnames(temp.pair) = streams
  slope.pair = as.matrix(dist(DATA$Slope)); rownames(slope.pair) = streams; colnames(slope.pair) = streams
  CV.pair = as.matrix(dist(DATA$CV)); rownames(CV.pair) = streams; colnames(CV.pair) = streams
  substrate.pair = as.matrix(dist(DATA$sub.pred)); rownames(substrate.pair) = streams; colnames(substrate.pair) = streams
  Q.pair = as.matrix(dist(DATA$median_Q)); rownames(Q.pair) = streams; colnames(Q.pair) = streams
  depth.pair = as.matrix(dist(DATA$depth_m)); rownames(depth.pair) = streams; colnames(depth.pair) = streams
  no3.pair = as.matrix(dist(DATA$no3.nh4_mg_L)); rownames(no3.pair) = streams; colnames(no3.pair) = streams
  srp.pair = as.matrix(dist(DATA$srp_mg_L)); rownames(srp.pair) = streams; colnames(srp.pair) = streams
  np.pair = as.matrix(dist(DATA$h2oNP)); rownames(np.pair) = streams; colnames(np.pair) = streams
  CN.pair = as.matrix(dist(DATA$CN.mean)); rownames(CN.pair) = streams; colnames(CN.pair) = streams 
  imb.pair = as.matrix(dist(DATA$NPimb)); rownames(imb.pair) = streams; colnames(imb.pair) = streams 
  bom.pair = as.matrix(dist(DATA$C.mean/1000)); rownames(bom.pair) = streams; colnames(bom.pair) = streams
  pc1.pair = as.matrix(dist(DATA$PC1));rownames(pc1.pair) = streams; colnames(pc1.pair) = streams
  pc2.pair = as.matrix(dist(DATA$PC2));rownames(pc2.pair) = streams; colnames(pc2.pair) = streams
  co = pairs
  
  ref_site = c()
  comparison_site = c()
  temp.diff = c()
  slope.diff = c()
  cv.diff = c()
  sub.diff = c()
  Q.diff = c()
  depth.diff = c()
  no3.diff = c()
  srp.diff = c()
  np.diff = c()
  CN.diff = c()
  imb.diff = c()
  bom.diff = c()
  pc1.diff = c()
  pc2.diff = c()
  
  for( elem in 1:nrow(co)){  
    ref_site = c(ref_site, as.character(co[elem, 1]))
    comparison_site = c(comparison_site, as.character(co[elem, 2]))
    temp.diff = c(temp.diff, temp.pair[as.character(co[elem, 1]),as.character(co[elem, 2])])
    slope.diff = c(slope.diff, slope.pair[as.character(co[elem, 1]),  as.character(co[elem, 2])])
    cv.diff = c(cv.diff, CV.pair[as.character(co[elem, 1]), as.character(co[elem, 2])])
    sub.diff = c(sub.diff, substrate.pair[ as.character(co[elem, 1]), as.character(co[elem, 2])])
    Q.diff = c(Q.diff, Q.pair[as.character(co[elem, 1]),  as.character(co[elem, 2])])
    depth.diff = c(depth.diff, depth.pair[as.character(co[elem, 1]), as.character(co[elem, 2])])
    no3.diff = c(no3.diff, no3.pair[as.character(co[elem, 1]),  as.character(co[elem, 2])])
    srp.diff = c(srp.diff, srp.pair[as.character(co[elem, 1]), as.character(co[elem, 2])])
    np.diff = c(np.diff, np.pair[as.character(co[elem, 1]), as.character(co[elem, 2])])
    CN.diff = c(CN.diff, CN.pair[as.character(co[elem,1]), as.character(co[elem,2])])
    imb.diff = c(imb.diff, imb.pair[as.character(co[elem,1]), as.character(co[elem,2])])
    bom.diff = c(bom.diff, bom.pair[as.character(co[elem,1]), as.character(co[elem,2])])
    pc1.diff = c(pc1.diff, pc1.pair[as.character(co[elem,1]), as.character(co[elem,2])])
    pc2.diff = c(pc2.diff, pc2.pair[as.character(co[elem,1]), as.character(co[elem,2])])
  }
  pairwise.df = data.frame(ref_site, comparison_site, temp.diff, slope.diff, cv.diff, sub.diff,
                           Q.diff, depth.diff, no3.diff, srp.diff, np.diff, CN.diff, imb.diff, bom.diff,
                           pc1.diff, pc2.diff) 
  return(pairwise.df)
}
j.summary_pairs = j.summary_stoic
#debugonce(pairwise_df);
j.summary_pair = pairwise_df(j.summary_pairs,pairs) #pairwise data frame
##### End Pairwise datafram code ####
# j.price %>% filter(SPP != "PotPer") -> j.price
positions = c(1:2,4:5,8:14)

j.price_long = j.df %>% dplyr::select(positions) %>%
  dplyr::mutate_at('SPP', funs(dplyr::recode(.,FBOMe = "Biofilm", CBOMe = "Biofilm" ))) %>%
  group_by(Stream, Date, Sample, SPP) %>%
  dplyr::summarize(OM.tot = sum(OM.tot), C.tot = sum(C.tot), N.tot = sum(N.tot), P.tot = sum(P.tot),
            PerC = mean(PerC,na.rm = T), PerN = mean(PerN, na.rm = T), PerP = mean(PerP, na.rm = T))

### make sure dfBIO jives with j.summary_stoic ###
j.price_long %>%
  dplyr::select(Stream:OM.tot) %>%
  group_by(Stream, Date, Sample) %>%
  dplyr::summarise(OM.sum = sum(OM.tot, na.rm = T)) %>%
  ungroup() %>% group_by(Stream,Date) %>%
  dplyr::summarise(OM.sum = mean(OM.sum, na.rm = T))
###

# price_wideC <- j.price_long %>%
#   spread(SPP, C.tot) %>%
#   replace(.,is.na(.), 0) %>%
#   ungroup() %>% gather(SPP, C.tot, CalHam:Wood) %>%
#   group_by(Stream, Date, SPP) %>%
#   summarise(C.tot = mean(C.tot))
#create the site by species matrix of abundance
# use j.df_long

dfBIO <- j.price_long %>%
  dplyr::select((Stream:OM.tot)) %>%
  mutate(OM.tot = OM.tot/1000) %>% #set to g/m2
  spread(SPP, OM.tot) %>%
  replace(.,is.na(.), 0) %>%
  transform(OM.sum = rowSums(.[4:18])) %>%
  gather(SPP, OM.tot, CalHam:Wood) %>%
  mutate(OM.rel = OM.tot/OM.sum) %>%
  group_by(Stream, Date, SPP) %>%
  summarise(OM.sum = mean(OM.sum, na.rm = T), OM.rel = mean(OM.rel, na.rm = T)) %>%
  #summarise(sum.rel = sum(OM.rel))
  mutate(OM.mean = OM.rel*OM.sum) %>%
  ungroup() %>%  dplyr::select(-Date, -OM.sum, -OM.rel) %>%
  spread(SPP, OM.mean) %>%
  column_to_rownames('Stream')

#### make sure dfBIO equals the mean BOM difference ###
dfBIO %>%
  rownames_to_column('Stream') %>%
  gather(SPP, OM.tot, Biofilm:Wood) %>%
  group_by(Stream) %>% dplyr::summarise(OM.tot = sum(OM.tot, na.rm = T))

# dfBIO = cast(j.price_long, Stream ~ SPP, value = 'OM.tot') 
# dfBIO_FULL = dfBIO[,-1]
# rownames(dfBIO_FULL) = dfBIO[,1]

#dfBIO_ref = dfBIO[which(dfBIO$Stream == "Hver" | dfBIO$Stream == "ST1"),]
#dfBIO_ref$Stream = factor(dfBIO_ref$Stream)
#now set the site by species matrix of functionaal contribution

dfFUNC_C<- j.price_long %>%
  dplyr::select((Stream:SPP),PerC) %>%
  mutate(PerC = PerC/100) %>%
  spread(SPP, PerC) %>%
  replace(.,is.na(.), 0) %>%
  gather(SPP, PerC, CalHam:Wood) %>%
  mutate(PerC = replace(PerC, which(PerC == 0), NA)) %>%
  group_by(Stream, SPP) %>%
  dplyr::summarize(PerC = mean(PerC, na.rm = T)) %>%
  ungroup() %>% 
  spread(SPP,PerC) %>%
  column_to_rownames('Stream')

dfFUNC_N<- j.price_long %>%
  dplyr::select((Stream:SPP),PerN) %>%
  mutate(PerN = PerN/100) %>%
  spread(SPP, PerN) %>%
  replace(.,is.na(.), 0) %>%
  gather(SPP, PerN, CalHam:Wood) %>%
  mutate(PerC = replace(PerN, which(PerN == 0), NA)) %>%
  group_by(Stream, SPP) %>%
  dplyr::summarize(PerN = mean(PerN, na.rm = T)) %>%
  ungroup() %>% 
  spread(SPP,PerN) %>%
  column_to_rownames('Stream')

dfFUNC_P<- j.price_long %>%
  dplyr::select((Stream:SPP),PerP) %>%
  mutate(PerP = PerP/100) %>%
  spread(SPP, PerP) %>%
  replace(.,is.na(.), 0) %>%
  gather(SPP, PerP, CalHam:Wood) %>%
  mutate(PerC = replace(PerP, which(PerP == 0), NA)) %>%
  group_by(Stream, SPP) %>%
  dplyr::summarize(PerP = mean(PerP, na.rm = T)) %>%
  ungroup() %>% 
  spread(SPP,PerP) %>%
  column_to_rownames('Stream')
#set the species names to eachother
dfSPECid = cbind(unique(levels(j.price_long$SPP)), unique(levels(j.price_long$SPP)))
##set the reference stream
stREF = rownames(data.frame(dfBIO)[which(data.frame(dfBIO)$Stream == "ST1")]) 
#stREF = rownames(dfBIO[1,])

#source("./functions/20170324_ELE_12773_Functions.R")
source("./functions/new_Functions.R")
#give the Price a go C######
#debugonce(price_eq)
priceout_C = price_eq(data.frame(dfBIO), data.frame(dfFUNC_C), dfSPECid, reference = stREF, norm = F, stat = 'mean', multipl= 1, ref_run = 1, global_correct = 0)
PRICE_C_RAW = priceout_C$price_raw
#give the Price for N#####
priceout_N = price_eq(data.frame(dfBIO), data.frame(dfFUNC_N), dfSPECid, reference = stREF, norm = F, stat = 'mean', multipl= 1, ref_run = 1, global_correct = 0)
PRICE_N_RAW = priceout_N$price_raw

#give the Price for P#####
priceout_P = price_eq(data.frame(dfBIO), data.frame(dfFUNC_P), dfSPECid, reference = stREF, norm = F, stat = 'mean', multipl= 1, ref_run = 1, global_correct = 0)
PRICE_P_RAW = priceout_P$price_raw
######

#####now merge pairwise and C price df######
PRICE_C_wide = spread(PRICE_C_RAW, price_term, value)
j.price.C = merge(j.summary_pair, PRICE_C_wide, by = c("ref_site", "comparison_site"))
PRICE_C_wide = ddply(PRICE_C_wide, .(ref_site, comparison_site), mutate, sumpart = sum(SCRE_LG, CDE_ABUN, CDE_FUNC, CDE_INTR, na.rm = T))

j.price.C$SCRE_LG[is.na(j.price.C$SCRE_LG) | is.infinite(j.price.C$SCRE_LG)] = NA
saveRDS(j.price.C, file = "./object_files/j.price.C.rds")

j.price.Cmean = apply(j.price.C[,c("SCRE_LG","CDE_ABUN","CDE_FUNC","CDE_INTR")], 2, FUN = mean, na.rm = T);j.price.Cmean
j.price.Csd = apply(j.price.C[,c("SCRE_LG","CDE_ABUN","CDE_FUNC","CDE_INTR")], 2, FUN = sd, na.rm = T); j.price.Csd

j.price.Cmedian = apply(j.price.C[,c("SCRE_LG","CDE_ABUN","CDE_FUNC","CDE_INTR")], 2, FUN = median, na.rm = T);j.price.Cmedian
j.price.Cmad = apply(j.price.C[,c("SCRE_LG","CDE_ABUN","CDE_FUNC","CDE_INTR")], 2, FUN = mad, na.rm = T); j.price.Cmad

#ggplot(PRICE_C_wide, aes( x = T_T, y = sumpart)) + geom_point(size = 2) + geom_abline(intercept = 0, slope = 1)#ensure parts equal the difference in function

#now merge pairwise and N price df#####
PRICE_N_wide = spread(PRICE_N_RAW, price_term, value)
j.price.N = merge(j.summary_pair, PRICE_N_wide, by = c("ref_site", "comparison_site"))
##### check that parts equal the functional difference #####
PRICE_N_wide = ddply(PRICE_N_wide, .(ref_site, comparison_site), mutate, sumpart = sum(SCRE_LG, CDE_ABUN, CDE_FUNC, CDE_INTR, na.rm = T))
#ggplot(PRICE_N_wide, aes( x = T_T, y = sumpart)) + geom_point(size = 6) + geom_abline(intercept = 0, slope = 1)#ensure parts equal the difference in function
# use SCRE_LG, CDE_ABUN, CDE_FUNC, & CDE_INTR

#now merge pairwise and P price df#####
PRICE_P_wide = spread(PRICE_P_RAW, price_term, value)
j.price.P = merge(j.summary_pair, PRICE_P_wide, by = c("ref_site", "comparison_site"))
##### check that parts equal the functional difference #####
PRICE_P_wide = ddply(PRICE_P_wide, .(ref_site, comparison_site), mutate, sumpart = sum(SCRE_LG, CDE_ABUN, CDE_FUNC, CDE_INTR, na.rm = T))
#ggplot(PRICE_P_wide, aes( x = T_T, y = sumpart)) + geom_point(size = 6) + geom_abline(intercept = 0, slope = 1)#ensure parts equal the difference in function

#now to do the stoichiometric change of price components
colnames(PRICE_C_RAW) = c("ref_site","comparison_site", "price_term", "C.value")
colnames(PRICE_N_RAW) = c("ref_site","comparison_site", "price_term", "N.value")
colnames(PRICE_P_RAW) = c("ref_site","comparison_site", "price_term", "P.value")

PRICE_CN_RAW = merge(PRICE_C_RAW, PRICE_N_RAW, by = c("ref_site", "comparison_site", "price_term"))
PRICE_CP_RAW = merge(PRICE_C_RAW, PRICE_P_RAW, by = c("ref_site", "comparison_site", "price_term"))
PRICE_NP_RAW = merge(PRICE_N_RAW, PRICE_P_RAW, by = c("ref_site", "comparison_site", "price_term"))

PRICE_CN_RAW <- PRICE_CN_RAW %>% mutate(CN.value = C.value/N.value)
PRICE_CP_RAW <- PRICE_CP_RAW %>% mutate(CP.value = C.value/P.value)
PRICE_NP_RAW <- PRICE_NP_RAW %>% mutate(NP.value = N.value/P.value)
#####Here we widen out the stoichiometry dfs#####
PRICE_CN_wide = spread(PRICE_CN_RAW[,-c(4:5)], price_term, CN.value)
j.price.CN = merge(j.summary_pair, PRICE_CN_wide, by = c("ref_site", "comparison_site"))

j.price.CN$Stream = j.price.CN$ref_site
j.price.CN = merge(j.price.CN, j.summary_stoic[,c("Stream","CN.mean")], by = c("Stream"))

j.price.CN$SCRE_LG[is.na(j.price.CN$SCRE_LG) | is.infinite(j.price.CN$SCRE_LG)] = NA
#plot(j.price.CN$CN.diff~j.price.CN$T_T)

j.price.delCN = ddply(j.price.CN, .(ref_site, comparison_site), transform, SCRE_LG = SCRE_LG - CN.mean, CDE_ABUN = CDE_ABUN - CN.mean,
                      CDE_FUNC = CDE_FUNC - CN.mean, CDE_INTR = CDE_INTR - CN.mean)
saveRDS(j.price.delCN,file = "./object_files/j.price.delCN.rds")

j.price.delCNmean = apply(j.price.delCN[,c("SCRE_LG","CDE_ABUN","CDE_FUNC","CDE_INTR")], 2, FUN = mean, na.rm = T);j.price.delCNmean
j.price.delCNsd = apply(j.price.delCN[,c("SCRE_LG","CDE_ABUN","CDE_FUNC","CDE_INTR")], 2, FUN = sd, na.rm = T); j.price.delCNsd

j.price.delCNmedian = apply(j.price.delCN[,c("SCRE_LG","CDE_ABUN","CDE_FUNC","CDE_INTR")], 2, FUN = median, na.rm = T);j.price.delCNmedian
j.price.delCNmad = apply(j.price.delCN[,c("SCRE_LG","CDE_ABUN","CDE_FUNC","CDE_INTR")], 2, FUN = mad, na.rm = T); j.price.delCNmad

#j.price.CN$sum = apply(j.price.delCN[,], 1, FUN = sum )
###### checking the relationships with CN differences and Price components  across different quantiles##########
#### dont have to check that parts equal for stoich #####
# use SCRE_LG, CDE_ABUN, CDE_FUNC, & CDE_INTR
PRICE_CP_wide = spread(PRICE_CP_RAW[,-c(4:5)], price_term, CP.value)
j.price.CP = merge(j.summary_pair, PRICE_CP_wide, by = c("ref_site", "comparison_site"))

j.price.CP$Stream = j.price.CP$ref_site
j.price.CP = merge(j.price.CP, j.summary_stoic[,c("Stream","CP.mean")], by = c("Stream"))

j.price.CP$SCRE_LG[is.na(j.price.CP$SCRE_LG) | is.infinite(j.price.CP$SCRE_LG)] = NA

j.price.delCP = ddply(j.price.CP, .(ref_site, comparison_site), transform, SCRE_LG = SCRE_LG - CP.mean, CDE_ABUN = CDE_ABUN - CP.mean,
                      CDE_FUNC = CDE_FUNC - CP.mean, CDE_INTR = CDE_INTR - CP.mean)
saveRDS(j.price.delCP, file = "./object_files/j.price.delcP.rds")

#j.price.delCP = j.price.delCP[-18,]

j.price.delCPmean = apply(j.price.delCP[,c("SCRE_LG","CDE_ABUN","CDE_FUNC","CDE_INTR")],2, FUN = mean,  na.rm = T);j.price.delCPmean
j.price.delCPsd = apply(j.price.delCP[,c("SCRE_LG","CDE_ABUN","CDE_FUNC","CDE_INTR")], 2, FUN = sd, na.rm = T); j.price.delCPsd

j.price.delCPmedian = apply(j.price.delCP[,c("SCRE_LG","CDE_ABUN","CDE_FUNC","CDE_INTR")],2, FUN = median,  na.rm = T);j.price.delCPmedian
j.price.delCPmad = apply(j.price.delCP[,c("SCRE_LG","CDE_ABUN","CDE_FUNC","CDE_INTR")], 2, FUN = mad, na.rm = T); j.price.delCPmad

# use SCRE_LG, CDE_ABUN, CDE_FUNC, & CDE_INTR
PRICE_NP_wide = spread(PRICE_NP_RAW[,-c(4:5)], price_term, NP.value)
j.price.NP = merge(j.summary_pair, PRICE_NP_wide, by = c("ref_site", "comparison_site"))

j.price.NP$Stream = j.price.NP$ref_site
j.price.NP = merge(j.price.NP, j.summary_stoic[,c("Stream","NP.mean")], by = c("Stream"))
j.price.NP$SCRE_LG[is.na(j.price.NP$SCRE_LG) | is.infinite(j.price.NP$SCRE_LG)] = NA

j.price.delNP = ddply(j.price.NP, .(ref_site, comparison_site), transform, SCRE_LG = SCRE_LG - NP.mean, CDE_ABUN = CDE_ABUN - NP.mean,
                      CDE_FUNC = CDE_FUNC - NP.mean, CDE_INTR = CDE_INTR - NP.mean)
saveRDS(j.price.delNP, file = "./object_files/j.price.delNP.rds")
#j.price.delNP = j.price.delNP[-18,]
j.price.delNPmean = apply(j.price.delNP[,c("SCRE_LG","CDE_ABUN","CDE_FUNC","CDE_INTR")], 2, FUN = mean, na.rm = T);j.price.delNPmean
j.price.delNPsd = apply(j.price.delNP[,c("SCRE_LG","CDE_ABUN","CDE_FUNC","CDE_INTR")], 2, FUN = sd, na.rm = T); j.price.delNPsd

j.price.delNPmedian = apply(j.price.delNP[,c("SCRE_LG","CDE_ABUN","CDE_FUNC","CDE_INTR")], 2, FUN = median, na.rm = T);j.price.delNPmedian
j.price.delNPmad = apply(j.price.delNP[,c("SCRE_LG","CDE_ABUN","CDE_FUNC","CDE_INTR")], 2, FUN = mad, na.rm = T); j.price.delNPmad
