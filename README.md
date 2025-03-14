[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15028221.svg)](https://doi.org/10.5281/zenodo.15028221)

# Data and R scripts for the manuscript 'Virtuality VS Reality: testing a virtual approach for the exploratory analysis of Bronze Age urns from Northern Italy'

This repository contains data and scripts used in the manuscript:

Virtuality VS Reality: testing a virtual approach for the exploratory analysis of Bronze Age urns from Northern Italy

Lucia Martina Scalise, Maria Pia Morigi, Claudio Cavazzuti, Rosa Brancaccio, Enrico R. Crema, Marco Seracini, Stefano Benazzi, Emma Pomeroy

The repository is organised into the following four main directories:

- data - Contains data frames to run the permutation test and to create the distribution plots.
- figures - Contains each figure generated from R in PDF format.
- scripts - Contains the script for the permutation test and the distribution plots.
- table_2 - Contains the data frames resulted from the permutation test that are contained in table 2 in the manuscript.

# File structure

**[Legend for anatomical classes:
CB = Cranial Bones
LB = Long Bones
SB = Spongy Bone
UL = Upper Limbs
LL = Lower Limbs
T = Trunk]**

**data**
- horizontal_HAB_physical.csv - Counts of each anatomical class in Half A and Half B of the urn (loaded data for Permutation test)
- horizontal_HAB_virtual.csv -  Area occupied by each anatomical class in Half A and Half B of the urn calculated from the virtual analysis (loaded data for Permutation test)
- horizontal_HCD_physical.csv - Counts of each anatomical class in Half C and Half D of the urn (loaded data for Permutation test)
- horizontal_HCD_virtual.csv - Area occupied by each anatomical class in Half C and Half D of the urn calculated from the virtual analysis (loaded data for Permutation test)
- vertical_physical.csv - Counts of each anatomical class in the Upper, Central, and Lower sections of the urn (loaded data for Permutation test)
- vertical_virtual.csv - Area occupied by each anatomical class in the Upper, Central, and Lower sections of the urn calculated from the virtual analysis (loaded data for Permutation test)
- virt_phys.csv - Combined data frame containing proportion of each anatomical class in each section (horizontal and vertical) of the urn calculated from the physical and virtual analysis (loaded data for Distribution plots)

**figures**
- fig.10 - main composed figure
  * Spongy_Bone_Horizontal_A.pdf 
  * Trunk_Horizontal_B.pdf
- fig.12 - main composed figure
  * Lower_Limbs_Horizontal.pdf
  * Upper_Limbs_Horizontal_A.pdf
- fig.7 - main composed figure
  * Cranial_Bones_Vertical_A.pdf
  * Long_Bones_Vertical_B.pdf
  * Spongy_Bone_Vertical_C.pdf
- fig.8 - main composed figure
  * Lower_Limbs_Vertical_B.pdf
  * Upper_Limbs_Vertical_A
  * Trunk_Vertical_C.pdf
- fig.9 - main composed figure
  * Cranial_Bones_Horizontal_A.pdf
  * Long_Bones_Horizontal_B.pdf
- fig.13.pdf - main figure
- fig.14.pdf - main figure
- fig.15.pdf - main figure

**scripts**
- Plots_virt_phys.R - Script to create figures 7, 8, 9, 10, 12
- permutation_test.R - Script to run permutation analysis, creating figures 13 - 15, and extracting data contained in table 2

**table_2**
- Observed_correlation_HA_B.csv - data frame containing the correlation coefficients calculated from the permutation test for the Halves A and B
- Observed_correlation_HC_D.csv - data frame containing the correlation coefficients calculated from the permutation test for the Halves C and D
- Observed_correlation_vertical.csv - data frame containing the correlation coefficients calculated from the permutation test for the sections upper, central, and lower
- Two-sided_p-value_HA_B.csv - data frame containing the p-values from the permutation test for the Halves A and B
- Two-sided_p-value_HC_D.csv - data frame containing the p-values from the permutation test for the Halves C and D
- Two-sided_p-value_vertical.csv - data frame containing the p-values from the permutation test for the sections upper, central, and lower

# Funding and ethics
This work is part of the first author’s PhD project funded by the Cambridge Trust International Scholarship, with the contribution of the Robert Sloley Travel Grant (St John’s College, Cambridge), Worts Travelling Scholars Fund and University Fieldwork Fund (Department of Archaeology, University of Cambridge). This research received a favourable ethical opinion from the Department of Archaeology Ethics Board (reference number: ARCH-01-2022-07) at the University of Cambridge.

# Licence
CC-BY 3.0
