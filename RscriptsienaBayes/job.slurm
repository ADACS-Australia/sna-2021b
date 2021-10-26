#!/bin/bash
#
#SBATCH --job-name=sna_test
#SBATCH --ntasks=64
#SBATCH --cpus-per-task=1
#SBATCH --time=10:00:00
#SBATCH --mem=32G

ml r/4.1.1

srun ./mpi-Rscript RscriptsienaBayes.r
