
# A Function to Be Used as an Example 
# -= a work in progress =-
# 
# TODO: add some some calculations; make a function more compact with 
#       fewer options but with multiple combinations !


exfun <- function(a, b, c, d, mat, f=stop("Argument f must be set"), g=NULL)
{
    
    switch(a,
           "a_one"={
               switch(b,
                      "b_one"={},
                      "b_two"={},
                      "b_three"={
                          switch(c,           
                                 "one"={},
                                 "two"={},
                                 "three"={},
                                 {
                                     stop("Wrong setting")
                                 }
                          )
                      },
                      "b_four"={},
                      "b_five"={},
                      {
                          stop("Wrong setting")
                      }
               )
           },
           "a_two"={
               solve(mat)
               
           },
           "a_three"={
               print(nonexistent_variable)
           },
           "a_four"={
               switch(c,           
                      "one"={},
                      "two"={},
                      "three"={},
                      {
                          stop("Wrong setting")
                      }
               )
           },
           "a_five"={
               if(!is.null(g)) {
                   print(f*g)
               }
               
           },
           {
               stop("Wrong setting")
           }
    )
    
    
    
}



# 
if(0) {
    
    # generate error
    m1 <- matrix(c(1,1,1,1), nrow = 2)
    solve(m1)
    
    # generate error
    
    exfun(1,2,3)
    exfun(a="two",b=2,d=3, mat=m1)
    
}
