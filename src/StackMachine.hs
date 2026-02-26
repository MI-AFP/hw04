module StackMachine where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Stack as Stack

import Control.Program

-- | Input is sequence of values (type Value is defined in Control.Program)
type Input = Seq.Seq Value
-- | Output is sequence of values (type Value is defined in Control.Program)
type Output = Seq.Seq Value

-- | Memory of computer stores on address of type Address a value of type Value (see Control.Program)
type Memory = Map.Map Address Value
-- | Lookup directory of subprograms (labels are marking starts of parts of a program where you can jump with JU or JZ)
type SubprogramDir = Map.Map Label Program
-- | Computer stack can store addresses and values
type ComputerStack = Stack.Stack (Either Address Value)

-- | Errors that may appear while running program on Stack Machine
data Error = EmptyStack
           | NotValue
           | NotAddress
           | NoInput
           | UnknownLabel
           | DivisionByZero
           | UninitializedMemory
           deriving (Show, Read, Eq)

-- | Result of program exection on Stack Machine with some input
type Result = Either Error Output

-- | Run program with given input (memory and stack should be empty at start)
-- | If there is a problem, result should indicate error (using Error type in Left)
-- TODO: implement running the program
runProgram :: Program -> Input -> Result
runProgram = undefined

-- Feel free to create more helper functions
