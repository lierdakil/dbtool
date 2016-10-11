import Test.Hspec
import FDTools

main :: IO ()
main = do
  hspec $ do
    describe "FDTools.Util" $ do
      describe "minimize" $ do
        it "doesn't change FD closure" $ property $
          \x -> fullext (minimize x) == fullext x
        it "returns collected FDs" $ property $
          \x -> collect (minimize x) == minimize x
        it "does not contain trivial FDs" $ property $
          \x -> nontrivial (minimize x) == minimize x
