module EvalExpr (main) where
import Control.Applicative ((<$>), (<*>))
import Data.Fixed
import Text.Printf

round6dp :: Double -> Double
round6dp x = fromIntegral (round $ x * 1e2) / 1e2

main :: String -> IO ()
main expression = printf "%.2f" (case calculate expression of
    Just n -> round6dp n 
    Nothing -> 0)
--main expression = print (calculate expression)

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