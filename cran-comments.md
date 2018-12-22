
# Updates to 0.1.5

 * Compatibility with rlang 0.3.0.
 * Removed `cases` function
    (`case_func` and `case_trfunc` replace it).

## Test environments

* local OS X (Mojave) install, R 3.5.1
* ubuntu 14.04 (on travis-ci), R 3.3, 3.4, 3.5
* win-builder (devel and release)
* RHub:
    - Debian Linux, R-devel, GCC ASAN/UBSAN
    - Debian Linux, R-release, GCC
    - Ubuntu Linux 16.04 LTS, R-devel with rchk
    - Ubuntu Linux 16.04 LTS, R-devel, GCC
    - CentOS 6, stock R from EPEL
    - Ubuntu Linux 16.04 LTS, R-release, GCC
    - Windows Server 2008 R2 SP1, R-oldrel, 32/64 bit
  
## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

 - tailr (checks without errors)
