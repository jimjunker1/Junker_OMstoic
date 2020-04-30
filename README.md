Patterns and Drivers of Ecosystem-level Biomass and Stoichiometry in
Streams
================

This repository contains code and data needed to reproduce the article:

**Junker, JR, WF Cross, JP Benstead, AD Huryn, JM Hood, D Nelson, GM
Gíslason, JS Ólafsson.** Patterns and Drivers of Ecosystem-level
Biomass and Stoichiometry in Streams. In review at *Ecosystems*

**Code DOI:**

[![DOI](https://zenodo.org/badge/256362584.svg)](https://zenodo.org/badge/latestdoi/256362584)

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
    ## +-- figures
    ## +-- Junker_OMstoic.Rproj
    ## +-- paper
    ## +-- README.md
    ## +-- README.Rmd
    ## \-- scripts
    ##     +-- analysis
    ##     |   \-- pca-analysis.R
    ##     +-- datascript.R
    ##     \-- install-packages.R

The raw data consist of 5 files:

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
