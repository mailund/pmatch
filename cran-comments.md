
# Update to 0.1.3

 * Formula syntax in cases as an alternative: 
 
      cases(f(x), foo ~ bar, baz ~ qux).
   
   This is easier to get to static code checkes such as lintr and through the
   byte compiler that can complain about "assignments" to literals.
   
 * bind[x,y,z] <- 1:3 syntax for binding variables.
 
 * transformation function transform_cases_function for modifying a function
   instead of calling `cases`.

## Test environments

* local OS X install, R 3.4.4
* ubuntu 14.04 (on travis-ci), R 3.4.4
* win-builder (devel and release)
* RHub:
  
  - Debian Linux, R-release, GCC
  - Ubuntu Linux 16.04 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran
  - Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  
  On Windows and Fedora, I get a Pandoc warning because 
  it cannot find the README badges. Other than that, the 
  checks passes.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

 * Package `tailr` checks without problems with this release.
