## Content of this repository

This repository contains code for the study "Efficacy of transcranial direct current stimulation (tDCS) in modulating neurocognitive markers of obsessive-compulsive disorder (OCD)" by Luisa Balzus, Julia Klawohn, Stephan Brandt, and Norbert Kathmann.  

&nbsp;
  
## Overview

This study aimed to investigate whether cathodal tDCS over the medial-frontal cortex modulates electrophysiological correlates of action monitoring and behavioral performance in patients with OCD and healthy control participants. 
The study is a randomized double-blind controlled placebo (sham tDCS) crossover trial. After receiving cathodal or sham stimulation (applied at an intensity of 1.5 mA for 20 minutes over the medial-frontal cortex) participants performed an arrow version of the flanker interference task, while EEG was recorded.
In this repository, we provide data and scripts used for statistical analyses and figure generation. 

&nbsp;

## Software Information

Analyses were conducted with R version 3.6.1 and R Studio version 1.2.5001.  

&nbsp;

## Analysis Scripts

Annotated analysis code is organized into separate R Markdown files:  

- **0_Demographics.Rmd**: This script gives an overview over dempgrapic and clinical variables.
- **1_Behavioral_Performance.Rmd**: This script performs the analysis of behavioral performance in the flanker task, including data cleaning, descriptive statistics, figure generation, (G)LMM analyses, and ANOVAs.
- **2_ERP_Analysis.Rmd**: This script performs the analysis of the electrophysiological correlates (ERPs) of action monitoring in the flanker task, including data cleaning, descriptive statistics, LMM analyses, and ANOVAs.
- **3_ERP_Visualization.Rmd**: This script generates figures of ERP trajectories and topograhies.  
- **4_Trait_Variables.Rmd**: This script generates an overview of the assessed trait variables in the two groups.  

The corresponding HTML files display the output from running the R Markdown files in HTML format.  

&nbsp;

## Usage

To reproduce statistical analyses, download the source files, open the file OCD_tDCS.Rproj and run `renv::restore()` to automatically install all required packages into the project library.




