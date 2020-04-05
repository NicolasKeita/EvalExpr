module EvalExpr (main) where
import Control.Applicative ((<$>), (<*>))
import System.Exit
import Data.Fixed
import Text.Printf

round6dp :: Double -> Double
round6dp x = fromIntegral (round $ x * 1e2) / 1e2

die84 :: IO ()
die84   = exitWith (ExitFailure 84)

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace needle replacement haystack
  = case begins haystack needle of
      Just remains -> replacement ++ remains
      Nothing      -> case haystack of
                        []     -> []
                        x : xs -> x : replace needle replacement xs

begins :: Eq a => [a] -> [a] -> Maybe [a]
begins haystack []                = Just haystack
begins (x : xs) (y : ys) | x == y = begins xs ys
begins _        _                 = Nothing

modifExpression :: String -> String
modifExpression expression = modifExpressionMod (modifExpressionDiv (modifExpressionSub (modifExpressionMul (modifExpressionAdd expression))))

modifExpressionAdd :: String -> String
modifExpressionAdd = replace "+" " + "

modifExpressionMul :: String -> String
modifExpressionMul = replace "*" " * "

modifExpressionSub :: String -> String
modifExpressionSub = replace "-" " - "

modifExpressionDiv :: String -> String
modifExpressionDiv = replace "/" " / "

modifExpressionMod :: String -> String
modifExpressionMod = replace "%" " % "

main :: String -> IO ()
main expression = case calculate (modifExpression expression) of
    Just n | isInfinite n -> die84
    Just n -> printf "%.2f" (round6dp n)
    Nothing -> die84

type Operator = Double -> Double -> Double
type Entry = (String, Operator)
type Register = [Entry]

operatorRegister :: Register
operatorRegister = [
                ("+", (+)),
                ("-", (-)),
                ("*", (*)),
                ("/", (/)),
                ("%", mod')
            ]

calculate :: String -> Maybe Double
calculate expression = eval operatorRegister (splitExpression expression)

splitExpression :: String -> [String]
splitExpression expression = [ w | w <- words expression, all isAuthorized w ]

isAuthorized :: Char -> Bool
isAuthorized character = character `elem` authorizedCharacters

eval :: Register -> [String] -> Maybe Double
eval [] _ = Nothing
eval _ [] = Nothing
eval _ [number] = Just (read number)
eval ((operator, function):rest) unparsed =
    case span (/=operator) unparsed of
        (_, []) -> eval rest unparsed
        (beforeOperator, afterOperator) -> 
            function
                <$> eval operatorRegister beforeOperator
                <*> eval operatorRegister (drop 1 afterOperator)

authorizedCharacters :: String
authorizedCharacters = "0123456789().+-/*%"