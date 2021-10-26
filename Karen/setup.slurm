#!/bin/bash
#
#SBATCH --job-name=karen-setup
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --time=01:00:00
#SBATCH --mem=16G

ml r/4.1.1

Rscript call.Karen.R

# Clean up
rm *.txt *.log
