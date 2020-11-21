Flow is more Important than Temperature in Driving Patterns of Organic
Matter Storage and Stoichiometry in Stream Ecosystems
================

This repository contains code and data needed to reproduce the article:

**Junker, JR, WF Cross, JP Benstead, AD Huryn, JM Hood, D Nelson, GM
Gíslason, JS Ólafsson.** Flow is more Important than Temperature in
Driving Patterns of Organic Matter Storage and Stoichiometry in Stream
Ecosystems. *Ecosystems* DOI: 10.1007/s10021-020-00585-6

**Code DOI:**

[![DOI](https://zenodo.org/badge/3756279.svg)](https://zenodo.org/badge/latestdoi/3756279)

All analyses were done in `R` <https://cran.r-project.org/>

The file structure of this repository is:

    ## .
    ## +-- data
    ## |   +-- derived_data
    ## |   |   +-- GB_samples.rda
    ## |   |   +-- GB_summary.rda
    ## |   |   +-- j.df.rda
    ## |   |   +-- j.stoic_df.rda
    ## |   |   \-- j.summary_stoic.rda
    ## |   +-- ocecolors.rda
    ## |   \-- raw_data
    ## |       +-- BOM_volumes.csv
    ## |       +-- cutC.csv
    ## |       +-- DM.csv
    ## |       +-- jul_nutrients.csv
    ## |       +-- PerCN.csv
    ## |       +-- PerP.csv
    ## |       \-- stream_metadata.csv
    ## +-- Junker_OMstoic.Rproj
    ## +-- output
    ## +-- README.md
    ## +-- README.Rmd
    ## \-- scripts
    ##     +-- analysis
    ##     |   +-- figures.R
    ##     |   +-- pca-analysis.R
    ##     |   \-- submission_scripts
    ##     |       +-- bio-dist-stoic_plot.R
    ##     |       +-- bio-dist_plot.R
    ##     |       +-- community_analysis-and-figs.R
    ##     |       +-- compartment-stoic_figure.R
    ##     |       +-- figure-1.R
    ##     |       +-- figure-1a.R
    ##     |       +-- figure-1b-d.R
    ##     |       +-- NMDS_figure.R
    ##     |       +-- NMDS_script.R
    ##     |       +-- pca_plot.R
    ##     |       +-- pie_bubbles.R
    ##     |       +-- pool-figure_script.R
    ##     |       +-- Price_analysis.R
    ##     |       +-- Price_figures.R
    ##     |       +-- raw_BOM-env_figures.R
    ##     |       +-- raw_GB-env_figures.R
    ##     |       \-- RMA-regression_Scaling-figs.R
    ##     +-- datascript.R
    ##     \-- install-packages.R

The raw data consist of 7 files:

`stream_metadata.csv` contains stream physicochemical data such as:
location, width, depth, annual temperature, and discharge summaries
(e.g., median Q, max Q, etc.).

`jul_nutrients.csv` contains summarize dissolved nutrient values from
July 2012

`BOM_volumes.csv` contains sample and subsample volumes from individual
samples

`DM.csv` contains measured drymass and organic matter content
calculations from individual subtypes

`cutC.csv` contains carbon and nitrogen concentrations for large macro
primary producers (e.g., macrophytes, brophytes, etc.)

`PerCN.csv` contains carbon and nitrogen concentrations for non-macro
BOM subtypes

`PerP.csv` contains phophorus concentrations for all sample subtypes
