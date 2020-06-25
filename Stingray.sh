#!/bin/bash
#
#SBATCH --job-name=medi_Stingray

#SBATCH --nodes=1
#SBATCH --time=20:00:00
#SBATCH --ntasks-per-node=4

#SBATCH --mem=90GB


##module load hdf5/1.10.2
##module load gfortran/6.3.0
##module load gcc/6.3.0


#./stingray make_all -parameterfile parameters_1.txt
./stingray make_all -parameterfile parameters_2.txt
./stingray make_all -parameterfile parameters_3.txt
./stingray make_all -parameterfile parameters_4.txt
./stingray make_all -parameterfile parameters_5.txt
./stingray make_all -parameterfile parameters_6.txt
./stingray make_all -parameterfile parameters_7.txt
./stingray make_all -parameterfile parameters_8.txt
./stingray make_all -parameterfile parameters_9.txt


