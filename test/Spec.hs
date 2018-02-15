import Test.Hspec

import qualified ListsSpec
import qualified StackSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Lists"      ListsSpec.spec
  describe "Data.Stack" StackSpec.spec
