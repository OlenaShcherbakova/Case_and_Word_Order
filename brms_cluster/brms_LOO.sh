#!/bin/bash
#SBATCH --cpus-per-task 20
#SBATCH --mem 80G
#SBATCH -J brms_LOO

Rscript --verbose brms_LOO.R
