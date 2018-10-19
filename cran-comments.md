


# Update to 0.1.4

 * New constructor code. This gives an substantial speedup when generating objects.
 * Added `case_func` as a much faster replacement for using the `cases` function. 
 * Suggests `ggraph` and `tidygraph` for vignettes
 * Pattern matching on multiple patterns and values using
   `..` notation.
 * Fixed a rewrite bug when using qualified names.
 * Uses `foolbox` for a safer rewrite function.

## Test environments

* local OS X (Mojave) install, R 3.4.4
* ubuntu 14.04 (on travis-ci), R 3.3, 3.4, 3.5
* win-builder (devel and release)
* RHub:
  
  - Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  - Ubuntu Linux 16.04 LTS, R-release, GCC
  
  On Windows and Fedora, I get a Pandoc warning because 
  it cannot find the README badges. Other than that, the 
  checks pass.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

 * Package `tailr` checks without problems with this release.
