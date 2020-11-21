##Community differences within streams
comm.wide  = j.df %>%
  dplyr::select(Stream:C.tot) %>%
  group_by(Stream, Date, Sample, SPP) %>%
  summarise(C.tot = sum(C.tot)) %>%
  ungroup() %>% group_by(Stream, Date, Sample) %>%
  spread(SPP, C.tot) %>% 
  mutate(Biofilm = CBOMe+FBOMe) %>%
  dplyr::select(-one_of(c("CBOMe","FBOMe"))) %>%
  replace(.,is.na(.), 0) %>%
  left_join(j.summary_stoic)

df.comp = comm.wide %>%
  ungroup() %>% dplyr::select(-PotPer, -Horsetail) %>%
  dplyr::select(CalHam:Biofilm)

df.env = comm.wide %>%
  ungroup() %>% dplyr::select(c(Stream,median_Q:G.B)) %>%
  dplyr::select(-one_of(c("PC3","PC4","PC5","PC6")))

set.seed(201)
jul.NMDS <- metaMDS(df.comp, distance = "bray", trymax =1000, autotransform = T)
jul.NMDS
plot(jul.NMDS)
stressplot(jul.NMDS)

NMDS.scrs <- as.data.frame(scores(jul.NMDS, display = "sites"))
NMDS.scrs <- cbind(NMDS.scrs, comm.wide$Stream)
NMDS.scrs
colnames(NMDS.scrs) <- c("NMDS1", "NMDS2", "Stream")

NMDS.spp <- as.data.frame(scores(jul.NMDS, display = "species"))

set.seed(301)
vec.env <- envfit(jul.NMDS, df.env, permutations = 1000, na.rm = T)
vec.env

env.scrs <- as.data.frame(scores(vec.env, display = "vectors"))
env.scrs <- cbind(env.scrs, Species = rownames(env.scrs))
env.scrs
stream.scrs = as.data.frame(scores(vec.env, display = "factors"))
stream.scrs = cbind(stream.scrs, Stream = rownames(stream.scrs))
stream.scrs
env.vec = env.scrs  %>%
  filter(Species %in% c("mean_temp", 
                        "srp_mg_L", "PC1", "PC2","CV"))

set.seed(401)
vec.spp <- envfit(jul.NMDS, comm.wide[,c(1,4:15,17:18)], permutations = 1000, na.rm = T)
vec.spp
spp.scrs <- as.data.frame(scores(vec.spp, display = "vectors"))
#species = c("CalHam", "CalSta", "Fila.", "Font.", "Grass", "Equ.", "Jung.", "MyrAlt", "Nos.", "PotPer", "Wood", "CBOMe", "FBOMe", "FBOM", "CBOM","Wood")
spp.scrs <- cbind(spp.scrs, Species = row.names(spp.scrs))
spp.scrs
stream.scrs = as.data.frame(scores(vec.spp, display = "factors"))
stream.scrs = cbind(stream.scrs, Stream = rownames(stream.scrs))
stream.scrs
spp.vec = spp.scrs %>%
  filter(Species %in% c("CBOM","FBOM", "Font", "Biofilm", "Jung", "Nos","Fila","CalHam","Horsetail"))

#####
