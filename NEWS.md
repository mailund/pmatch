# pmatch 0.1.3

 * Formula syntax in cases as an alternative: cases(f(x), foo ~ bar, baz ~ qux).
   This is easier to get to static code checkes such as lintr and through the
   byte compiler that can complain about "assignments" to literals.
 * bind[x,y,z] <- 1:3 syntax for binding variables.
 * transformation function transform_cases_function for modifying a function
   instead of calling `cases`.
 

# pmatch 0.1.2

 * Transformation rules to make pattern matching work with tail-recursion from tailr package.
 * Function cases_expr generates an expression (a call object) based on pattern rules.
 * Exporting test_pattern and test_pattern_ to test one pattern against a value.

# pmatch 0.1.1

 * Initial release.

