import IO
import List
import Char
import Numeric
import System.Environment
import System.Exit

main = do
    hSetBuffering stdin LineBuffering
    putStrLn "\n Available actions:\n\n  Press '0' to Exit\n  Press '1' to add new contact\n  Press '2' to search by name\n  Press '3' to search by phone number\n  Press '4' to save your phone book\n  Press '5' to load a phone book\n"
    doLoop []

doLoop list = do
	putStr ("\n Input the number of option: ")
	choise <- getLine
	
	case choise of
		"0" -> return ()
		"1" -> do
			putStrLn $ "  Input the name: "
			name <- getLine
			putStr $ "  Input the phone number: "
			number <- getLine
			doLoop (insert (name, number) list)
		"2" -> do
			putStr $ "  Input the name to search: "
			name <- getLine
			putStrLn $ "   Number: " ++ searchByName name list ++ "\n"
			doLoop list
		"3" -> do
			putStr $ "  Input the number to search: "
			number <- getLine
			putStrLn $ "   Name: " ++ searchByNumber number list ++ "\n"
			doLoop list
		"4" -> do
			save list
			putStrLn $ "   Phone book is saved"
			doLoop list
		"5" -> do
			putStr $ "  Input filepath: "
			path <- getLine
			load path list
			putStrLn $ "   Phone book is loaded: "
			doLoop list
		"6" -> do
			print list
			doLoop list
		_   -> doLoop list

save list = writeFile "phoneBook.txt" (listToString list 0) where
	listToString [] _ = "]"
	listToString (x:xs) k
		| k == 0 = "[(" ++ (fst x) ++ "," ++ (snd x) ++ ")" ++ listToString xs (k + 1)
		| otherwise = ",(" ++ (fst x) ++ "," ++ (snd x) ++ ")" ++ listToString xs (k + 10)

load path list = do
	catch (readFile path) $ \_ -> do
		putStrLn (" Error on reading file: " ++ path)
		doLoop list
		exitWith ExitSuccess
	x <- readFile path
	y <- rList x
	print y

rList :: String -> IO [(String, String)]	  
rList = readIO

searchByName _ [] = fail "\n    There is no such name in your phone book\n"
searchByName name (x:xs)
	| fst x == name = snd x
	| otherwise = searchByName name xs

searchByNumber _ [] = fail "\n    There is no such name in your phone book\n"
searchByNumber number (x:xs)
	| snd x == number = fst x
	| otherwise = searchByNumber number xs	