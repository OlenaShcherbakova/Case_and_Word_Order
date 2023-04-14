#!/bin/bash
#SBATCH --cpus-per-task 20
#SBATCH --mem 122G
#SBATCH -J brms_kfold_Vf

Rscript --verbose brms_kfold_Vf.R
