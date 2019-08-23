module LensExamples.LensesSpec where

import Test.Hspec

import Control.Lens
import Data.Text (Text)

data User = User
  { _userName :: Text
  , _userEmail :: Text
  , _userProfile :: Profile
  } deriving (Show, Eq)

data Profile = Profile
  { _profileWebsite :: Text
  , _profileAge :: Int
  } deriving (Show, Eq)

$(makeLenses ''User)
$(makeLenses ''Profile)

alyssa :: User
alyssa = User
  { _userName = "Alyssa P. Hacker"
  , _userEmail = "alyssa@example.com"
  , _userProfile = Profile
    { _profileWebsite = "http://example.com/~alyssa/"
    , _profileAge = 40
    }
  }

spec :: Spec
spec = do
  specify "derived lenses" $
    alyssa ^. userName `shouldBe` "Alyssa P. Hacker"

  specify "lenses compose with (.)" $
    alyssa ^. userProfile.profileAge `shouldBe` 40

  specify "built-in lenses" $ do
    let pair = (42 :: Integer, "hello" :: Text)
    pair ^. _1 `shouldBe` 42
    pair ^. _2 `shouldBe` "hello"

  specify "type-changing sets" $ do
    let pair = (42 :: Integer, "hello" :: Text)
    (pair & _2 .~ True) `shouldBe` (42, True)
