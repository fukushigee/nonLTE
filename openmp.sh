#!/usr/bin/sh
#PBS -N test_rhd 
#PBS -q large-t 
#PBS -l mppwidth=16
#PBS -me
#PBS -M wada@astrophysics.jp
cd /work/wadaki/mpi_test
export OMP_NUM_THREADS=8
time aprun -n 1 -d 8 ./rhd.out >& mi.log 
