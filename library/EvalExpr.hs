-- | An example module.
module EvalExpr (main) where
import Control.Applicative ((<$>), (<*>))


main :: IO ()

main = print $ calculate "3 + 4 * 4"

type Operator = Double -> Double -> Double
type Entry = (String, Operator)
type Register = [Entry]

modulu :: Double -> Double -> Double
modulu a b = fromIntegral $ mod (round a) (round b)

operatorRegister :: Register
operatorRegister = [
                ("-", (-)),
                ("+", (+)),
                ("/", (/)),
                ("*", (*)),
                ("%", modulu)
            ]

calculate :: String -> Maybe Double
calculate = eval operatorRegister . words
-- calculate = do
--  return 3.0 + 2.

eval :: Register -> [String] -> Maybe Double
eval [] _ = Nothing -- No operator found.
eval _ [] = Nothing -- If a operator don't have anything to operate on.
eval _ [number] = Just $ read number
eval ((operator, function):rest) unparsed =
    case span (/=operator) unparsed of
        (_, []) -> eval rest unparsed
        (beforeOperator, afterOperator) -> 
            function
                <$> eval operatorRegister beforeOperator
                <*> eval operatorRegister (drop 1 afterOperator)