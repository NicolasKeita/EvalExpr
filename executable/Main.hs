import System.Environment
import qualified EvalExpr
import qualified ArgumentHandling

main :: IO ()
main = do
    ArgumentHandling.main
    args <- getArgs
    EvalExpr.main (head args)