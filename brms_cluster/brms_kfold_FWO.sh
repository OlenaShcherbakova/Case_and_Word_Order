#!/bin/bash
#SBATCH --cpus-per-task 20
#SBATCH --mem 300G
#SBATCH -J brms_kfold_FWO

Rscript --verbose brms_kfold_FWO.R
