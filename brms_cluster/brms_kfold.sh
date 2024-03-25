#!/bin/bash
#SBATCH --cpus-per-task 20
#SBATCH --mem 300G
#SBATCH -J brms_kfold

Rscript --verbose brms_kfold.R
