module ArgumentHandling (main) where
import System.Environment
import System.Exit

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse ["-h"]        = usage >> exit
parse ["--help"]    = usage >> exit
parse []            = die84
parse args
    | not (isSingleArg args) = die84
    | not (isValidExpression (head args)) = die84
    | otherwise = return()

usage :: IO ()
usage   = putStrLn "Usage: ./funEvalExpr math_expression"

exit :: IO ()
exit    = exitSuccess

die84 :: IO ()
die84   = exitWith (ExitFailure 84)

isSingleArg :: [String] -> Bool
isSingleArg args = length args == 1

isValidExpression :: String -> Bool
isValidExpression expression = lengthCompare expression (expressionFiltered expression) == EQ

expressionFiltered :: String -> String
expressionFiltered expression = filter (`elem` authorizedCharacters) expression

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend` (x `compare` y)

authorizedCharacters :: String
authorizedCharacters = "0123456789 ().+-/*%"