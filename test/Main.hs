import Test.Hspec
import FDTools
import Test.QuickCheck (property)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

instance Arbitrary Vertex where
  arbitrary = Vertex . return <$> elements ['a'..'z']

main :: IO ()
main = do
  hspec $ do
    describe "FDTools.Util" $ do
      describe "minimize" $ do
        it "doesn't change FD closure" $ property $
          \x -> fullext (minimize x) == collect (fullext x)
        it "returns collected FDs" $ property $
          \x -> collect (minimize x) == minimize x
        it "does not contain trivial FDs" $ property $
          \x -> nontrivial (minimize x) == minimize x
