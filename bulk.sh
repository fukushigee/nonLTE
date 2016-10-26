#PBS -N test_nonlte
#PBS -q bulk-b 
#PBS -l mppwidth=128
##PBS -l mpplabels=DEBUG
##PBS -l mppnppn=1
#PBS -me
#PBS -M wada@astrophysics.jp
cd /work/wadaki/nonlte
time aprun -n 64 ./yamada.out >& yamada_co_test7.log
