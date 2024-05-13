import System.IO
import Data.Char
import Control.Monad
import Text.Read

main = do
    line <- getLine
    if isQuit line == True
            then do
                putStrLn "bye" 
                return()
            else do
                let nospace = noSpaces line
                    operatorindex = indexCheck nospace 0
                    int1string = returnString nospace 0 operatorindex
                    int2string = returnString nospace (operatorindex + 1) (length nospace)
                    int1true = onlyDigits int1string
                    int2true = onlyDigits int2string
                    check_operator = checkOperator nospace operatorindex
                if (int1true == True) && (int2true == True) && (check_operator == True)
                    then do
                        let result = countResult nospace int1string int2string operatorindex
                        putStrLn $ "=" ++ show result
                    else do
                        putStrLn error_message
                main
                    where   error_message = "I cannot calculate that. Possible operators include: '+', '-', '*' and '/'."
              
checkOperator :: String -> Int -> Bool
checkOperator [] _ = False
checkOperator s index
                    | s !! index == '+' = True
                    | s !! index == '-' = True
                    | s !! index == '*' = True
                    | s !! index == '/' = True
                    | otherwise = False

returnString :: String -> Int -> Int -> String
returnString [] _ _ = []
returnString s indexs indexe
                   | indexs < indexe = (s !! indexs) : returnString s (indexs + 1) indexe
                   | otherwise = []

isQuit :: String -> Bool
isQuit [] = False
isQuit x
        | x == "quit" = True
        | otherwise = False

noSpaces :: String -> String
noSpaces [] = []
noSpaces s = [a | a <- s, a /= ' ']

indexCheck :: String -> Int -> Int
indexCheck [] _ = 0
indexCheck (x:xs) i
               | x == '+' || x == '-' || x == '*' || x == '/' = i
               | otherwise = indexCheck xs i+1

onlyDigits :: String -> Bool
onlyDigits [] = False
onlyDigits [s] = Main.isDigit s
onlyDigits (x:xs)
    | Main.isDigit x = onlyDigits xs
    | otherwise = False

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

countResult :: String-> String -> String -> Int -> Int
countResult s s1 s2 index
            | s !! index == '+' = n1asInt + n2asInt
            | s !! index == '-' = n1asInt - n2asInt
            | s !! index == '*' = n1asInt * n2asInt
            | s !! index == '/' = n1asInt `div` n2asInt
             where n1asInt = read s1 :: Int
                   n2asInt = read s2 :: Int