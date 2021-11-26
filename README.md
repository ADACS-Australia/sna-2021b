# SNA_2021B

## Installing RSienaTest
```
R CMD INSTALL ./RSienaTest
```

- There are some issues when compiling with MPI on Mac, you can turn it off with the `--configure-vars="HAS_MPICC=no"`.
- Another useful flag is `--preclean`, which forces all C code to be recompiled (i.e. it effectively does a `make clean`).
- If you are only developing pure R code, you can skip the configure/compile step entirely with `--no-configure`.
- The flags `--no-test-load` and `--no-byte-compile` are also useful to speed up the development cycle.

## Running with MPI
The run the code with MPI, you can install https://github.com/ADACS-Australia/mpi-Rscript. It provides a wrapper around Rscript for running MPI enabled R programs with a master-slave parallelism.

You can then run your MPI enabled script with
```
mpirun -n 4 mpi-Rscript <my-script>
```
Just remember to set `clusterType = "MPI"` when calling the `sienaBayes()` or `siena07()` functions.
