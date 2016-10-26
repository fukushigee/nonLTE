#!/usr/bin/sh
#PBS -N test_rhd 
#PBS -q large-t 
#PBS -l mppwidth=1
#PBS -me
#PBS -M wada@astrophysics.jp
cd /work/wadaki/mpi_test
#export OMP_NUM_THREADS=16
time aprun -n 1 -d 1 ./rhd.out >& mpi_test22.log
