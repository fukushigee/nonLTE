#PBS -N test
#PBS -l mppwidth=128
##PBS -l mppnppn=1
#PBS -me
#PBS -M leanard838@ezweb.ne.jp


source /opt/modules/default/init/bash
module switch PrgEnv-cray PrgEnv-intel

cd /work/fukushigers/nonlte2
make -f mpi_makefile.7.8 clean
make -f mpi_makefile.7.8
cd /work/fukushigers/nonlte2/zikkou
#aprun -n 96 ./fuku.out
time aprun -n 64 ./fuku.out >& fuku.log

