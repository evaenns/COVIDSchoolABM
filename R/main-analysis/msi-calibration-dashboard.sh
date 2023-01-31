#!/bin/bash -l
#SBATCH --ntasks=100
#SBATCH --mem=32gb
#SBATCH -t 24:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=chen8153@umn.edu

module load R
Rscript R/main-analysis/msi-calibration-dashboard.R