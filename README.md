# etoh_correction_factors
Ethanol correction factors for seven North American freshwater species

## Summary
This repository contains the raw data and code to calculate ethanol correction factors for seven freshwater species: bluegill Lepomis macrochirus, brown bullhead Ameiurus nebulosus, largemouth bass Micropterus salmoides, pumpkinseed Lepomis gibbosus, rainbow trout Oncorhynchus mykiss, sockeye salmon Oncorhynchus nerka, and yellow perch Perca flavescens. 

## Structure
This repository contains three folders, 'data', 'code', and 'outputs'. The data folder contains files of stable isotope analysis data for all seven species. The code file contains all code necessary to calculate ethanol correction factors for the seven species, repeated measures ANOVAs to look at the effect of time, and generate figures. The output contains those outputs includes figures, results, and model outputs we generated in our analysis.

## Background 
Chemical preservation can affect carbon and nitrogen isotopic values. However, preservation in ethanol or formalin is a common practice in ecological research where storage in freezers may not be feasible or possible. We conducted an experiment to calculate the effect of preservation in 95% ethanol on dorsal muscle tissue for several species and found correction factors that varied greatly across species, as well as significant effects of time. In summary, correction factors such as the ones we calculated should be used with caution as they are dependent on preservation technique and duration, species, tissue-type, and what instrument and standards were used in stable isotope analysis.

## Methods
Fish were collected in western Washington in the spring/summer of 2022. We biopsied muscle tissues from 18-20 individuals and divided the biopsises into four groups: the control (only frozen), and three preservation treatments-- 3, 6, and 9 months of preservation in 95% ethanol. At the end of each ethanol preservation period, the ethanol was poured off and the samples were processed for stable isotope analysis. Muscle samples were either freeze dried using a VirTis Benchtop SLC freeze drier for 24 hours, or dried in an oven at 60 °C for 48 hours. The muscle was then ground into a fine, homogenous powder using a mortar and pestle and dissecting scissors. The ground samples were weighed into 5 mm x 9 mm tin capsules, and analyzed on a Thermo-Fisher Delta V mass spectrometer at the Holtgrieve Ecosystem Ecology Lab at the University of Washington. L-Glutamic acid and salmon standards run with the sample sets were used to estimate the precision and accuracy of the isotope ratio measurements in accordance with the lab’s QAQC process. 

## Output
The code files in this repository contain all of the code to calculate ethanol correction factors for the seven species, repeated measures ANOVAs to look at the effect of time, and generate figures that are reported in Doran and Jameson 2025 (unpublished).
