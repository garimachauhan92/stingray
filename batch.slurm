#!/bin/sh
# SLURM directives
#
#SBATCH --job-name=ALFALFA
#SBATCH --time=5:00:00
#SBATCH --nodes=1
#SBATCH --tasks-per-node=4

./stingray make_all -parameterfile parameters_1.txt
./stingray make_all -parameterfile parameters_2.txt
./stingray make_all -parameterfile parameters_3.txt
./stingray make_all -parameterfile parameters_4.txt
./stingray make_all -parameterfile parameters_5.txt
./stingray make_all -parameterfile parameters_6.txt
./stingray make_all -parameterfile parameters_7.txt
./stingray make_all -parameterfile parameters_8.txt
./stingray make_all -parameterfile parameters_9.txt

