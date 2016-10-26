#PBS -N test_nonlte
#PBS -q debug 
#PBS -l mppwidth=72
#PBS -l mpplabels=DEBUG
##PBS -l mppnppn=1
#PBS -me
#PBS -M wada@astrophysics.jp
cd /work/wadaki/nonlte
time aprun -n 64 ./yamada.out >& yamada_test2.log
