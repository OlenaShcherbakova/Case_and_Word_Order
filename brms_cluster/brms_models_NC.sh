#!/bin/bash
#SBATCH --cpus-per-task 20
#SBATCH --mem 80G
#SBATCH -J brms_NC

Rscript --verbose brms_models_NC.R
