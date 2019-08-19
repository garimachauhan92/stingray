#!/bin/sh
# SLURM directives
#
#SBATCH --job-name=ALFALFA
#SBATCH --time=5:00:00
#SBATCH --nodes=1
#SBATCH --tasks-per-node=4

./stingray make_all -parameterfile parameters.txt
