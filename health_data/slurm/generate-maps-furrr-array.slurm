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

module load gdal/3.0.1
module load proj/6.1.1
module load geos/3.6.2
module load R/4.2.1

cd parallel-r-materials/
Rscript health_data/generate-maps-furrr.R 1 40
