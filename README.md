## Introduction

A fortran implementation of the Hungarian algorithm,
based on the original paper "The Hungarian method for the assignment problem" of H. W. Khun
(https://doi.org/10.1002/nav.3800020109).

## Build

The library can be compiled with
```
make lib
```
This will produce `mod_khun.mod` and `libkhun.a` files,
which can be copied into the project directory that requires the Hungarian method.

## Usage

The library `libkhun.a` contains a single public subroutine: `khun()`.

`khun()` takes five mandatory and one optional arguments, which are listed below in order.

- `d` (input) integer stating the size of the square matrix
- `m` (input) square matrix of integer values on which to apply the Hungarian method
- `mode` (input) character string, the two possible values are `min` to request the minimum sum, and `max` for the maximum sum
- `ind` (output) integer array of size `d` that will cointain the column indices of the matrix elements selected by the Hungarian method
  (whereas their respective row indices match those of the `ind` array)
- `sum` (output) integer, sum of the matrix elements in the positions given by `ind`
- `verbose` (input, optional) logical, set to `.true.` if you want to print information while the algorithm is running.
  By default it is set to `.false.`
