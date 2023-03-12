

# Symbolica



## This code is under development, i.e. this is not the final version.
Symbolica is a symbolic computational code for automating multiscale model development via upscaling (e.g., asymptotic homogenization).



### An introduction to Symbolica can be found in the following paper:
K. Pietrzyk et al., Upscaling and automation: pushing the boundaries of multiscale modeling through symbolic computing,
Transport in Porous Media, 140, pp. 313-349, 2021.

(https://link.springer.com/article/10.1007/s11242-021-01628-9)



### More recent work on Symbolica can be found in the following preprints:

K. Pietrzyk and I. Battiato, Automated symbolic upscaling: model generation for extended applicability regimes, part 1,
ESS Open Archive (Submitted to Water Resources Research).

(https://essopenarchive.org/doi/full/10.1002/essoar.10512332.1)

K. Pietrzyk and I. Battiato, Automated symbolic upscaling: model generation for extended applicability regimes, part 2,
ESS Open Archive (Submitted to Water Resources Research).

(https://essopenarchive.org/doi/full/10.1002/essoar.10512333.1)



## Cite the code

[![DOI](https://zenodo.org/badge/613093394.svg)](https://zenodo.org/badge/latestdoi/613093394)



## To run the provided examples:

1. Open "Symbolica.nb" using Mathematica.

2. Specify the directory containing "Symbolica.nb" and all necessary functions in the variable "directory" under the section "Import Necessary Packages".

3. Specify the input file in the variable "inputNotebookName" under the section "Run Input File". The file names for the two example cases are provided (commented-out). Un-comment the name corresponding to the desired case to be run.

4. Evaluate the notebook by selecting the "Evaluate Notebook" option from the "Evaluation" tab at the top of the window. Symbolica will run and display the output file in around 18 seconds for problem 1, and around 7 minutes for problem 2. This file contains the multiple outputs (e.g., the upscaled equations, closure problems, dimensionless numbers, closure variables, etc.)

5. Before re-running or testing another input, quit the local kernel. This can be done by finding the "Quit Kernel" option from the "Evaluation" tab at the top of the window, and selecting the "Local" option. This will help avoid mixing previous results with those of the new inputs.
