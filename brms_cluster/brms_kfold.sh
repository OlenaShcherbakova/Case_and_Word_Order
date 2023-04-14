#!/bin/bash
#SBATCH --cpus-per-task 20
#SBATCH --mem 122G
#SBATCH -J brms_kfold

Rscript --verbose brms_kfold.R
