module LensExamples.BasicLensesSpec (spec) where

import Test.Hspec

import qualified Data.Text as T

import Data.Function ((&))
import Data.Text (Text)

data User = User
  { _userId :: Int
  , _userName :: Text
  , _userEmail :: Text
  , _userProfile :: Profile
  } deriving (Show, Eq)

data Profile = Profile
  { _profileWebsite :: Text
  , _profileAge :: Int
  } deriving (Show, Eq)

alyssa :: User
alyssa = User
  { _userId = 1
  , _userName = "Alyssa P. Hacker"
  , _userEmail = "alyssa@example.com"
  , _userProfile = Profile
    { _profileWebsite = "http://example.com/~alyssa/"
    , _profileAge = 40
    }
  }


withoutLenses :: Spec
withoutLenses = do
  specify "getting fields" $ do
    _userId alyssa `shouldBe` 1
    _userName alyssa `shouldBe` "Alyssa P. Hacker"

  specify "setting fields" $ do
    let alyssa' = alyssa { _userName = "A Lisp Hacker" }
    _userName alyssa `shouldBe` "Alyssa P. Hacker"
    _userName alyssa' `shouldBe` "A Lisp Hacker"

  specify "getting nested fields" $ do
    _profileAge (_userProfile alyssa) `shouldBe` 40
    _profileWebsite (_userProfile alyssa) `shouldBe` "http://example.com/~alyssa/"

  specify "setting nested fields" $ do
    let incrementAge user =
          let profile = _userProfile user
          in user { _userProfile = profile
                      { _profileAge = _profileAge profile + 1 } }

        alyssa' = incrementAge alyssa
    _profileAge (_userProfile alyssa') `shouldBe` 41


data Lens a b = Lens
  { view :: a -> b
  , set :: b -> a -> a }

over :: Lens a b -> (b -> b) -> a -> a
over l f s = set l (f (view l s)) s

infixr 9 .>
(.>) :: Lens a b -> Lens b c -> Lens a c
Lens view1 set1 .> Lens view2 set2 = Lens
  { view = view2 . view1
  , set = \a s -> set1 (set2 a (view1 s)) s }

infixl 8 ^.
(^.) :: a -> Lens a b -> b
(^.) = flip view

infixr 4 .~
(.~) :: Lens a b -> b -> a -> a
(.~) = set

infixr 4 %~
(%~) :: Lens a b -> (b -> b) -> a -> a
(%~) = over

withLenses :: Spec
withLenses = do
  let userId = Lens _userId (\a s -> s { _userId = a })
      userName = Lens _userName (\a s -> s { _userName = a})
      userProfile = Lens _userProfile (\a s -> s { _userProfile = a})
      profileWebsite = Lens _profileWebsite (\a s -> s { _profileWebsite = a})
      profileAge = Lens _profileAge (\a s -> s { _profileAge = a})

  context "without operators" $ do
    specify "getting fields" $ do
      view userId alyssa `shouldBe` 1
      view userName alyssa `shouldBe` "Alyssa P. Hacker"

    specify "setting fields" $ do
      let alyssa' = set userName "A Lisp Hacker" alyssa
      view userName alyssa' `shouldBe` "A Lisp Hacker"

    specify "transforming fields" $ do
      let alyssa' = over userName T.toUpper alyssa
      view userName alyssa' `shouldBe` "ALYSSA P. HACKER"

    specify "getting nested fields" $ do
      view (userProfile.>profileAge) alyssa `shouldBe` 40
      view (userProfile.>profileWebsite) alyssa `shouldBe` "http://example.com/~alyssa/"

    specify "transforming nested fields" $ do
      let alyssa' = over (userProfile.>profileAge) (+1) alyssa
      view (userProfile.>profileAge) alyssa' `shouldBe` 41

  context "with operators" $ do
    specify "getting fields" $ do
      alyssa ^. userId `shouldBe` 1
      alyssa ^. userName `shouldBe` "Alyssa P. Hacker"

    specify "setting fields" $ do
      let alyssa' = alyssa & userName .~ "A Lisp Hacker"
      alyssa' ^. userName `shouldBe` "A Lisp Hacker"

    specify "transforming fields" $ do
      let alyssa' = alyssa & userName %~ T.toUpper
      alyssa' ^. userName `shouldBe` "ALYSSA P. HACKER"

    specify "getting nested fields" $ do
      alyssa ^. userProfile.>profileAge `shouldBe` 40
      alyssa ^. userProfile.>profileWebsite `shouldBe` "http://example.com/~alyssa/"

    specify "transforming nested fields" $ do
      let alyssa' = alyssa & userProfile.>profileAge %~ (+1)
      alyssa' ^. userProfile.>profileAge `shouldBe` 41

    specify "setting multiple fields" $ do
      let alyssa' = alyssa & userName .~ "A Lisp Hacker"
                           & userProfile.>profileAge %~ (+1)

      alyssa' ^. userName `shouldBe` "A Lisp Hacker"
      alyssa' ^. userProfile.>profileAge `shouldBe` 41


spec :: Spec
spec = do
  context "without lenses" withoutLenses
  context "with lenses" withLenses
