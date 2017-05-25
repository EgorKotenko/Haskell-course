import IO
import List
import Char
import Numeric

main = do
    hSetBuffering stdin LineBuffering
    putStrLn "\n Available actions: \n\n  Press '0' to Exit \n  Press '1' to add value to your sorted list \n  Press '2' to remove value from your list \n  Press '3' to print your list \n"
    doLoop []

doLoop list = do
    
    choise <- getLine
    
    case choise of
        "0" -> return ()
        "1" -> do
            putStr ("\n Input the number to add, please: ")
            value <- getLine
            doLoop (insert value list)                  
        "2" -> do
            putStr ("\n Input the number to remove, please: ")
            value <- getLine
            doLoop (delete value list)
        "3" -> do 
            print list
            doLoop list
        _   -> doLoop list 


