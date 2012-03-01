module Data.LLVM.Types.Identifiers (
  -- * Types
  Identifier,
  -- * Accessor
  identifierAsString,
  identifierContent,
  -- * Builders
  makeAnonymousLocal,
  makeLocalIdentifier,
  makeGlobalIdentifier,
  makeMetaIdentifier
  ) where

import Control.DeepSeq
import Data.Hashable
import Data.ByteString.Char8 ( ByteString, unpack, pack )

data Identifier = LocalIdentifier { _identifierContent :: !ByteString
                                  , _identifierHash :: !Int
                                  }
                | AnonymousLocalIdentifier { _identifierNumber :: !Int }
                | GlobalIdentifier { _identifierContent :: !ByteString
                                   , _identifierHash :: !Int
                                   }
                | MetaIdentifier { _identifierContent :: !ByteString
                                 , _identifierHash :: !Int
                                 }
                  deriving (Eq, Ord)

instance Show Identifier where
  show LocalIdentifier { _identifierContent = t } = '%' : unpack t
  show AnonymousLocalIdentifier { _identifierNumber = n } = '%' : show n
  show GlobalIdentifier { _identifierContent = t } = '@' : unpack t
  show MetaIdentifier { _identifierContent = t } = '!' : unpack t

instance Hashable Identifier where
  hash (AnonymousLocalIdentifier n) = hash n
  hash i = _identifierHash i

instance NFData Identifier where
  rnf AnonymousLocalIdentifier {} = ()
  rnf i = _identifierContent i `seq` _identifierHash i `seq` ()

makeAnonymousLocal :: Int -> Identifier
makeAnonymousLocal = AnonymousLocalIdentifier

makeLocalIdentifier :: ByteString -> Identifier
makeLocalIdentifier t =
  LocalIdentifier { _identifierContent = t
                  , _identifierHash = hash t
                  }

makeGlobalIdentifier :: ByteString -> Identifier
makeGlobalIdentifier t =
  GlobalIdentifier { _identifierContent = t
                   , _identifierHash = hash t
                   }

makeMetaIdentifier :: ByteString -> Identifier
makeMetaIdentifier t =
  MetaIdentifier { _identifierContent = t
                 , _identifierHash = hash t
                 }

identifierAsString :: Identifier -> String
identifierAsString (AnonymousLocalIdentifier n) = show n
identifierAsString i = unpack (identifierContent i)

identifierContent :: Identifier -> ByteString
identifierContent (AnonymousLocalIdentifier n) = pack (show n)
identifierContent i = _identifierContent i