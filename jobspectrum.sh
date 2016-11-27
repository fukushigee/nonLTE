#PBS -N test
#PBS -l mppwidth=128
##PBS -l mppnppn=1
#PBS -me
#PBS -M leanard838@ezweb.ne.jp


source /opt/modules/default/init/bash
module switch PrgEnv-cray PrgEnv-intel

cd /work/fukushigers/nonlte2/getspectrum
make -f makefile_spect clean
make -f makefile_spect

time aprun  ./getspectrum143
