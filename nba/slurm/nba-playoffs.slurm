#!/bin/bash

#SBATCH --nodes=1
#SBATCH --ntasks-per-node=2
#SBATCH --mem-per-cpu=500
#SBATCH --job-name=nba-playoffs
#SBATCH --time=00:03:00
#SBATCH --output=logs/R-%x.%j.out
#SBATCH --error=logs/R-%x.%j.err
#SBATCH --export=ALL,TZ=Europe/London
#SBATCH --mail-type=ALL

# send mail to this address
#SBATCH --mail-user=youremail@here.com

module load R/4.2.1
module load openmpi/4.1.4/intel

cd parallel-r-materials/
Rscript nba/nba-playoffs-slurm.R
