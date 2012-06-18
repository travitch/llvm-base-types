module Data.LLVM.Types.Identifiers (
  -- * Types
  Identifier,
  -- * Accessor
  identifierAsString,
  identifierContent,
  isAnonymousIdentifier,
  -- * Builders
  makeAnonymousLocal,
  makeLocalIdentifier,
  makeGlobalIdentifier,
  makeMetaIdentifier
  ) where

import Control.DeepSeq
import Data.Hashable
import Data.SmallString
import Data.Text ( Text, unpack, pack )

data Identifier = LocalIdentifier { _identifierContent :: !SmallString
                                  , _identifierHash :: !Int
                                  }
                | AnonymousLocalIdentifier { _identifierNumber :: !Int }
                | GlobalIdentifier { _identifierContent :: !SmallString
                                   , _identifierHash :: !Int
                                   }
                | MetaIdentifier { _identifierContent :: !SmallString
                                 , _identifierHash :: !Int
                                 }
                  deriving (Eq, Ord)

instance Show Identifier where
  show LocalIdentifier { _identifierContent = t } = '%' : toString t
  show AnonymousLocalIdentifier { _identifierNumber = n } = '%' : show n
  show GlobalIdentifier { _identifierContent = t } = '@' : toString t
  show MetaIdentifier { _identifierContent = t } = '!' : toString t

instance Hashable Identifier where
  hash (AnonymousLocalIdentifier n) = hash n
  hash i = _identifierHash i

instance NFData Identifier where
  rnf AnonymousLocalIdentifier {} = ()
  rnf i = _identifierContent i `seq` _identifierHash i `seq` ()

makeAnonymousLocal :: Int -> Identifier
makeAnonymousLocal = AnonymousLocalIdentifier

makeLocalIdentifier :: Text -> Identifier
makeLocalIdentifier t =
  LocalIdentifier { _identifierContent = fromText t
                  , _identifierHash = hash t
                  }

makeGlobalIdentifier :: Text -> Identifier
makeGlobalIdentifier t =
  GlobalIdentifier { _identifierContent = fromText t
                   , _identifierHash = hash t
                   }

makeMetaIdentifier :: Text -> Identifier
makeMetaIdentifier t =
  MetaIdentifier { _identifierContent = fromText t
                 , _identifierHash = hash t
                 }

identifierAsString :: Identifier -> String
identifierAsString (AnonymousLocalIdentifier n) = show n
identifierAsString i = unpack (identifierContent i)

identifierContent :: Identifier -> Text
identifierContent (AnonymousLocalIdentifier n) = pack (show n)
identifierContent i = toText $ _identifierContent i

isAnonymousIdentifier :: Identifier -> Bool
isAnonymousIdentifier AnonymousLocalIdentifier {} = True
isAnonymousIdentifier _ = False