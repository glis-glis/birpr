## Resubmission 4

This is a resubmission. Although the package was initially accepted by CRAN, 
there was a compilation error on its Fedora server, which was unfortunately not solved
by the previous resubmission 3. I was still not able to reproduce the error locally, 
but I have now removed the step that locates the R library on the system, 
which failed on the Fedora server of CRAN. 
This likely solves the issue - but as I cannot reproduce the error, it's hard to be sure.

## Resubmission 3

This is a resubmission. Although the package was initially accepted by CRAN, 
there was a compilation error on its Fedora server, and the package was thus removed.
Despite not being able to reproduce the error on two local Fedora systems as well as 
Fedora on R-hub v2, I think I figured out the issue and fixed it accordingly. 
I apologize for contacting a CRAN team member directly. I was simply replying to the email and was unaware that it was a policy violation.
In this version, I now have:

* Likely fixed the compilation error on the Fedora CRAN server (as I cannot reproduce the error, it's hard to confirm this, but I'm confident this will work).
* Added a vignette.
* Improved the documentation.

## Resubmission 2
This is a resubmission. Thank you for your comments. In this version I have:

* Replaced instances of cat() by message() to make them suppressible.
* Removed gzstream.*, as this was an artifact that was never used.
* Added more contributors in DESCRIPTION.
* Added inst/AUTHORS file that explains in detail the contributions of all authors.
* inst/AUTHORS also describes the use of the fast-float library.
* inst/AUTHORS is referenced in DESCRIPTION.
* Added URL to DESCRIPTION.

## Resubmission 1
This is a resubmission. Thank you for your comments. In this version I have:

* Omitted the redundant "Provides functions to" from the description.
* Added more details about package functionality and implemented methods in the Description text.
* Added a reference to the description field.
* Replaced all T and F with TRUE and FALSE, respectively.
* Made sure all functions documentations define \value.
* Removed \dontrun{} and added small files needed for the examples to inst/extdata.
* Added a verbose argument that allows to turn off messages to the console completely.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
