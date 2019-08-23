module LensExamples.PrismsAndTraversalsSpec where

import Test.Hspec

import Control.Lens
import Data.Text (Text)

data User = User
  { _userName :: Text
  , _userEmail :: Text
  , _userPrivileges :: Privileges
  } deriving (Show, Eq)

data Privileges
  = PrivilegesAdmin
  | PrivilegesUser UserPrivileges
  deriving (Show, Eq)

data UserPrivileges = UserPrivileges
  { _canPost :: Bool
  , _canComment :: Bool
  , _canVote :: Bool
  } deriving (Show, Eq)

$(makeLenses ''User)
$(makePrisms ''Privileges)
$(makeLenses ''UserPrivileges)

-- _PrivilegesAdmin :: Prism' Privileges ()
-- _PrivilegesUser :: Prism' Privileges UserPrivileges

alyssa :: User
alyssa = User
  { _userName = "Alyssa P. Hacker"
  , _userEmail = "alyssa@example.com"
  , _userPrivileges = PrivilegesAdmin
  }

noCommentPrivileges :: UserPrivileges
noCommentPrivileges = UserPrivileges
  { _canPost = True
  , _canComment = False
  , _canVote = True
  }

ben :: User
ben = User
  { _userName = "Ben Bitdiddle"
  , _userEmail = "ben@example.com"
  , _userPrivileges = PrivilegesUser noCommentPrivileges
  }

spec :: Spec
spec = do
  specify "prisms are partial getters" $ do
    PrivilegesAdmin ^? _PrivilegesUser `shouldBe` Nothing
    PrivilegesUser noCommentPrivileges ^? _PrivilegesUser `shouldBe` Just noCommentPrivileges

  specify "prisms are constructors" $ do
    review _PrivilegesAdmin () `shouldBe` PrivilegesAdmin
    review _PrivilegesUser noCommentPrivileges `shouldBe` PrivilegesUser noCommentPrivileges

  specify "prisms compose with lenses to form partial getters" $ do
    alyssa ^? userPrivileges._PrivilegesUser.canPost `shouldBe` Nothing
    ben ^? userPrivileges._PrivilegesUser.canPost `shouldBe` Just True

  specify "prisms compose with lenses to form partial setters" $ do
    let disallowPosting = userPrivileges._PrivilegesUser.canPost .~ False
    disallowPosting alyssa ^? userPrivileges._PrivilegesUser.canPost `shouldBe` Nothing
    disallowPosting ben ^? userPrivileges._PrivilegesUser.canPost `shouldBe` Just False

  specify "prisms composed with lenses are traversals" $ do
    let userCanPost :: Traversal' User Bool
        userCanPost = userPrivileges._PrivilegesUser.canPost

        disallowPosting = userCanPost .~ False
    disallowPosting alyssa ^? userCanPost `shouldBe` Nothing
    disallowPosting ben ^? userCanPost `shouldBe` Just False

  specify "traverse is a valid traversal and can have multiple targets" $ do
    let userCanPost :: Traversal' User Bool
        userCanPost = userPrivileges._PrivilegesUser.canPost

        usersCanPost :: Traversal' [User] Bool
        usersCanPost = traverse.userCanPost

        ben' = ben & userCanPost .~ False
    [alyssa, ben, ben'] ^.. usersCanPost `shouldBe` [True, False]
