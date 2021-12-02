#!/bin/bash
#
#SBATCH --job-name=karen-setup
#SBATCH --ntasks=4
#SBATCH --cpus-per-task=1
#SBATCH --time=168:00:00
#SBATCH --mem=10G

ml r/4.1.1

Rscript setup.Karen.R

# Clean up
rm *.txt *.log
