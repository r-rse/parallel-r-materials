#!/bin/bash

#SBATCH --nodes=1
#SBATCH --ntasks=10
#SBATCH --mem=3G
#SBATCH --job-name=map_furrr_array
#SBATCH --time=00:05:00
#SBATCH --output=logs/R-%x.%j.out
#SBATCH --error=logs/R-%x.%j.err
#SBATCH --export=ALL,TZ=Europe/London
#SBATCH --array=1-4
#SBATCH --mail-type=ALL

# send mail to this address
#SBATCH --mail-user=youremail@here.com

module load gdal/3.9.2
module load proj/9.4.1
module load geos/3.13.0
module load R/4.4.0-gcc8

cd parallel-r-materials/
Rscript health_data/generate-maps-furrr.R 1 40
