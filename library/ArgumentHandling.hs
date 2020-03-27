module ArgumentHandling (main) where
import System.Environment
import System.Exit

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse ["-h"]        = usage >> exit
parse ["--help"]    = usage >> exit
parse []            = die84
parse args          =
    if not (isValidExpression args)
        then die84
        else return()

usage :: IO ()
usage   = putStrLn "Usage: ./funEvalExpr math_expression"
exit :: IO ()
exit    = exitSuccess
die84 :: IO ()
die84   = exitWith (ExitFailure 84)

isValidExpression :: [String] -> Bool
isValidExpression args =
    if (length args == 1)
        then True
        else False
