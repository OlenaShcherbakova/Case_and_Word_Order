#!/bin/bash
#SBATCH --cpus-per-task 20
#SBATCH --mem 80G
#SBATCH -J brms_Vf

Rscript --verbose brms_models_Vf.R
