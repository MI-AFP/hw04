module StackMachine where

import qualified Data.Sequence as S

import Control.Program
import Data.Stack

-- | Input is sequence of values (type Value is defined in Control.Program)
type Input = S.Seq Value
-- | Output is sequence of values (type Value is defined in Control.Program)
type Output = S.Seq Value

-- | Memory of computer stores on address of type Address a value of type Value (see Control.Program)
type Memory = Bool -- TODO: replace by your choice to implement computers memory


-- | Run program with given input (memory and stack are empty at start)
-- | If there is problem, error is raised ("Empty stack", "Not value", "Not address", "No input", "Unknown label")
-- TODO: implement running the program, use your Memory and Stack (from Data.Stack)
runProgram :: Program -> Input -> Output
runProgram = undefined
