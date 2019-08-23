module LensExamples.PlatedSpec where

import Test.Hspec

import Control.Lens
import Data.Data (Data)
import Data.Data.Lens

data GExists a
  = GExists
  { _geTable :: !String
  , _geWhere :: !(GBoolExp a)
  } deriving (Show, Eq, Functor, Foldable, Traversable, Data)

data GBoolExp a
  = BoolAnd ![GBoolExp a]
  | BoolOr  ![GBoolExp a]
  | BoolNot !(GBoolExp a)
  | BoolExists !(GExists a)
  | BoolFld !a
  deriving (Show, Eq, Functor, Foldable, Traversable, Data)

$(makeLenses ''GExists)
$(makePrisms ''GBoolExp)

allBoolExists :: (Data a) => Traversal' (GBoolExp a) (GExists a)
allBoolExists = deepOf template _BoolExists

mapBoolExists :: (Data a) => (a -> a) -> GBoolExp a -> GBoolExp a
mapBoolExists f = over allBoolExists (fmap f)

traverseBoolExists :: (Data a, Applicative f) => (a -> f a) -> GBoolExp a -> f (GBoolExp a)
traverseBoolExists f = traverseOf allBoolExists (traverse f)

updateTableInBoolExp :: (String, String) -> GBoolExp () -> GBoolExp ()
updateTableInBoolExp (oldQT, newQT) =
  transformOf uniplate $ _BoolExists.geTable %~ \rqfQT ->
    if rqfQT == oldQT then newQT else rqfQT

spec :: Spec
spec = do
  specify "mapBoolExists" $ do
    let input = BoolAnd
          [ BoolExists (GExists "" (BoolFld True))
          , BoolExists (GExists "" (BoolOr [BoolFld True, BoolFld False]))
          , BoolFld False
          , BoolNot (BoolExists (GExists "" (BoolFld False)))
          ]
        output = BoolAnd
          [ BoolExists (GExists "" (BoolFld False))
          , BoolExists (GExists "" (BoolOr [BoolFld False, BoolFld True]))
          , BoolFld False
          , BoolNot (BoolExists (GExists "" (BoolFld True)))
          ]
    mapBoolExists not input `shouldBe` output

  specify "updateTableInBoolExp" $ do
    let bExp = BoolAnd [BoolExists (GExists "one" (BoolFld ()))]
    updateTableInBoolExp ("one", "two") bExp `shouldBe`
      BoolAnd [BoolExists (GExists "two" (BoolFld ()))]
