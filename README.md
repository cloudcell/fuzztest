# fuzztest

**The purpose of the package:** To isolate errors through identifying groups of parameter combinations that cause them.

**What this package does:** Tests all possible combinations of argument values for any function 
to produce test statistics and graphs.
    
**What this package may do in addition:** One may create unit tests **_automatically_** -- 
generate all possible combination of inputs and save them in an .RData file with "reference" 
results. Next, during unit testing, one may load the file with reference results, adjust the 
internal test function and report failures in textual and convenient visual formats. 

Below are a several demo test graphs showing parameter settings leading to
function failures (in color). Axes are used only to order and group tests by
_cardinal_numbers_ of argument parameters.
  

![demo](http://i.imgur.com/z5Ivxw0.png)

![demo](http://i.imgur.com/P9vt78Y.png)
  
PASS only:  
![demo](http://i.imgur.com/vUrh2y0.png)
  
FAIL only:  
![demo](http://i.imgur.com/Cr73DJL.png)

