
# Update to 0.1.0.1.3

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
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit via RHub
* Ubuntu Linux 16.04 LTS, R-release, GCC via RHub
* Fedora Linux, R-devel, clang, gfortran via RHub

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

There are currently no downstream dependencies for this package.
