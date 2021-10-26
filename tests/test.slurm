#!/bin/bash
#
#SBATCH --job-name=rmpi_test
#SBATCH --nodes=2
#SBATCH --ntasks-per-node=32
#SBATCH --cpus-per-task=1
#SBATCH --time=00:00:30
#SBATCH --mem=10G

ml r/4.1.1

srun mpi-Rscript snowtest.r
