## figure script ##

### Figure list ###
#1.  Distribution of biomass and stoichiometry (4 panel)
source("./scripts/analysis/submission_scripts/figure-1.R")
#2. Raw and Added-variable of C with flow & temp
source("./scripts/analysis/submission_scripts/raw_BOM-env_figures.R")

#3 CNP coupling across total BOM
source("./scripts/analysis/submission_scripts/RMA-regression_Scaling-figs.R")
#4. Imbalance between ecosystem BOM stoic & NP supply
source("./scripts/analysis/submission_scripts/pool-figure_script.R")

#Supplemental
#S2. hydrographs across streams
# file = hydrographs.tiff
#s3. PCA plot
source("./scripts/analysis/submission_scripts/pca_plot.R");make_pca_plot()
#S4. NMDS figure
source("./scripts/analysis/submission_scripts/NMDS_script.R")
source("./scripts/analysis/submission_scripts/NMDS_figure.R")
#S5. Patterns of G & B with flow & temp
source("./scripts/analysis/submission_scripts/raw_GB-env_figures.R");figS3()
#3. Total C.mean vs G/B
source("./scripts/analysis/submission_scripts/bio-dist_plot.R");figS5()
#X CN, CP, NP vs G/B
source("./scripts/analysis/submission_scripts/bio-dist-stoic_plot.R");figS6()
#S6. Patterns of compartment stoic
source("./scripts/analysis/submission_scripts/compartment-stoic_figure.R");fig3()
