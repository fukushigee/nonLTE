#!/usr/bin/sh
#PBS -N model_pcb
##PBS -q debug 
#PBS -q large-t 
#PBS -l mppwidth=144
##PBS -l mppwidth=72
#PBS -me
#PBS -M wada@astrophysics.jp
cd /work/wadaki/run
export OMP_NUM_THREADS=2
#time aprun -n 4 -d 16 ./rhd.out >& mpi_test51.log
time aprun -n 64 -d 2  ./debug.out >& pcf.log 
#time aprun -n 64 -d 2  ./rhd.out >& mpi_test87.log
#time aprun -n 4 -d 8 ./rhd.out >& mpi_test38.log
#time aprun -n 16 -N 4 -S 2 -d 8 ./rhd.out >& mpi_test35.log
