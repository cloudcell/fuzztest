# fuzztest

### Purpose 
To isolate errors through identifying groups of parameter combinations 
that cause them.  

### What the Package Does
Tests all possible combinations of argument values for any function 
to produce test statistics and graphs to help identify and fix defects 
faster than by using more traditional approaches. The key feature of 
the package is its ability to present multiple test results 
in a very compact form. 
    
### Alternative Use 
Every run **_automatically_ creates data for unit testing.** Simply compare 
saved "reference" results against the ongoing development results. For now, 
one can check them by loading data files with test results and checking 
whether exit status is identical for all combinations of input parameters. 
(Timestamped data is saved in a current work directory after every test.)                            

### Roadmap
1. To save a function output value along with an exit status ("PASS/FAIL")
for use in unit tests 
2. To enable automatic comparison and visualization of a current test against
saved reference data. 
2. To report "incremental" "PASS/FAIL" i.e. only differences between the 
current test and previously recorded reference data.
3. To enable saving test log in a separate log file. 
           
***

### Demo Visual Output  
Below are a several demo test graphs showing parameter settings leading to
function failures (in color). Axes are used only to order and group tests by
_cardinal_numbers_ of argument parameters.
  

![demo](http://i.imgur.com/z5Ivxw0.png)

![demo](http://i.imgur.com/P9vt78Y.png)
  
**PASS Results Only**  
![demo](http://i.imgur.com/vUrh2y0.png)
  
**FAIL Results Only**  
![demo](http://i.imgur.com/Cr73DJL.png)

