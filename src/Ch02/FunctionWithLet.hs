module Ch02.FunctionWithLet where

import Ch02.FunctionWithWhere as Let (printInc)

printInc2 :: (Show a, Num a) => a -> IO ()
printInc2 = printInc
