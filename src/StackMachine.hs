module StackMachine where

import qualified Data.Sequence as S
import qualified Data.Map as M

import Control.Program
import Data.Stack

-- | Input is sequence of values (type Value is defined in Control.Program)
type Input = S.Seq Value
-- | Output is sequence of values (type Value is defined in Control.Program)
type Output = S.Seq Value

-- | Memory of computer stores on address of type Address a value of type Value (see Control.Program)
type Memory = M.Map Address Value

-- | Lookup directory of subprograms (labels are marking starts of parts of program where you can jump)
type SubprogramDir = M.Map Label Program

-- | Computer stack can store addresses and values
type ComputerStack = Stack (Either Address Value)


-- | Run program with given input (memory and stack are empty at start)
-- | If there is problem, error is raised ("Empty stack", "Not value", "Not address", "No input", "Unknown label", "Division by 0", "Uninitalized memory")
-- TODO: implement running the program
runProgram :: Program -> Input -> Output
runProgram = undefined

-- Feel free to create more helper functions
