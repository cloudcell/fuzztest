

fuzzdemo.R




# src: http://r-pkgs.had.co.nz/demo.html


# or also this: http://r.789695.n4.nabble.com/need-help-in-pausing-a-script-td3909815.html





#------------------------------------------------------------------------------#

# src: http://stackoverflow.com/questions/7439415/how-to-create-a-demo-for-a-presentation-tutorial

Another way to do it:
    
    Save your script in a file (demo.R)
Edit the script and sprinkle it with pause() in strategic places
In R, define pause <- function() invisible(readline())
Run the script with source("demo.R", echo=TRUE)
It will then print & run your commands and stop and wait for input at the sprinkled pause(). Just hit <Enter> to continue.

EDIT: I don't know a good way to hide the pause() statement. A possible way would be to copy the code for source() and modify it to skip printing calls to pause(), but that's a little overkill I think...

...but you could rename the pause function to anything you like - including '....', but you still need to call it like this: ....()

Hmmm. Maybe something like this:
    
    '....' <- function(...) invisible(readline())
Then sprinkle your script with either:
    
    ....('Press Enter to continue')
# Or
....(Press_Enter_to_continue)
Another possibility if you rename the pause function to Pausing...:
    
    Pausing...(Press_Enter)
