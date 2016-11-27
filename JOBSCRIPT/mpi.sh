#!/usr/bin/sh
#PBS -N model_pgh
##PBS -q bulk-b 
#PBS -q large-b 
#PBS -l mppwidth=384
#PBS -me
#PBS -M wada@astrophysics.jp
cd /work/wadaki/run
export OMP_NUM_THREADS=16
#time aprun -n 4 -d 16 ./rhd.out >& mpi_test51.log
time aprun -n 16 -N 1 -d 16  ./rhd.out >& pgh.log
#time aprun -n 4 -d 8  ./rhd.out >& ozh.log
#time aprun -n 64 -d 2  ./rhd.out >& mpi_test87.log
#time aprun -n 4 -d 8 ./rhd.out >& mpi_test38.log
#time aprun -n 16 -N 4 -S 2 -d 8 ./rhd.out >& mpi_test35.log
