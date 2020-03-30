module EvalExpr (main) where
import Control.Applicative ((<$>), (<*>))
import Data.Fixed


main :: String -> IO ()
main expression = print (calculate expression)

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
splitExpression expression =  words expression

eval :: Register -> [String] -> Maybe Double
eval [] _ = Nothing
eval _ [] = Nothing -- If a operator don't have anything to operate on.
eval _ [number] = Just (read number)
eval ((operator, function):rest) unparsed =
    case span (/=operator) unparsed of
        (_, []) -> eval rest unparsed
        (beforeOperator, afterOperator) -> 
            function
                <$> eval operatorRegister beforeOperator
                <*> eval operatorRegister (drop 1 afterOperator)