{-# LANGUAGE ViewPatterns #-}
import Test.Hspec
import FDTools
import qualified Test.QuickCheck as Q
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import qualified Data.Set as S

instance Arbitrary Vertex where
  arbitrary = Vertex . return <$> elements ['a'..'j']

newtype LimitedGraph = LimitedGraph Graph

instance Show LimitedGraph where
  show (LimitedGraph g)= graphToString g

instance Arbitrary LimitedGraph where
  arbitrary = sized $ \s -> do
    let
        edge :: Gen Edge
        edge = do
                left <- S.fromList <$> listOf1 arbitrary
                right <- S.fromList <$> listOf1 arbitrary
                return (left, right)
    g <- S.fromList <$> vectorOf s edge
    return (LimitedGraph g)

main :: IO ()
main = do
  hspec $ do
    describe "FDTools.Util" $ do
      modifyMaxSize (const 10) $ describe "minimize" $ do
        modifyMaxSize (const 4) $
          it "doesn't change FD closure" $
            Q.property $
              \(LimitedGraph x) -> fullext (conservative minimize x) == collect (fullext x)
        it "returns collected FDs" $ Q.property $
          \(LimitedGraph x) -> collect (minimize x) == minimize x
        it "does not contain trivial FDs" $ Q.property $
          \(LimitedGraph x) -> nontrivial (minimize x) == minimize x S.\\ S.singleton (S.empty, S.empty)
      describe "collect-expand" $ do
        it "collect . expand = collect" $ Q.property $
          \(LimitedGraph x) -> collect (expand x) == collect x
        it "expand . collect = expand" $ Q.property $
          \(LimitedGraph x) -> expand (collect x) == expand x
      describe "potkeys-superkeys" $ do
        modifyMaxSize (const 5) $ it "potkeys ⊂ superkeys" $ Q.property $
          \(LimitedGraph x) -> potkeys x `S.isSubsetOf` superkeys x
      describe "nontrivial" $ do
        modifyMaxSize (const 5) $ it "doesn't change FD closure" $ Q.property $
          \(LimitedGraph x) -> fullext (conservative nontrivial x) == fullext x
      describe "normalize-project" $ do
        modifyMaxSize (const 5) $ it "doesn't change FD closure" $ Q.property $
          \(LimitedGraph x) ->
            let
              norm = normalize x
              ps = S.fromList $ map (flip project (fullext x) . S.fromList) norm
            in  fullext (unions ps) == fullext x
