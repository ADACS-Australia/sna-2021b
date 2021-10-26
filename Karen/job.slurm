#!/bin/bash
#
#SBATCH --job-name=sna-karen
#SBATCH --ntasks=4
#SBATCH --cpus-per-task=1
#SBATCH --time=00:05:00
#SBATCH --mem=16G

ml r/4.1.1

srun ../mpi-Rscript call.Karen.R

# Clean up
rm *.txt *.log
