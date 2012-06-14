{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.LLVM.Types (
  Module(..),
  moduleGlobals,
  findFunctionByName,
  findMain,
  module Data.LLVM.Types.Attributes,
  module Data.LLVM.Types.Dwarf,
  module Data.LLVM.Types.Identifiers,
  module Data.LLVM.Types.Referential,
  module Data.LLVM.Internal.DataLayout
  ) where

import Control.DeepSeq
import Control.Monad.State.Strict
import qualified Data.HashSet as S
import Data.List ( find, intersperse )
import Data.Monoid
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Text.Lazy ( toStrict )
import Data.Text.Lazy.Builder

import Data.LLVM.Internal.ForceModule
import Data.LLVM.Internal.Printers
import Data.LLVM.Internal.DataLayout
import Data.LLVM.Types.Attributes
import Data.LLVM.Types.Dwarf
import Data.LLVM.Types.Identifiers
import Data.LLVM.Types.Referential

-- | This is the top-level representation of a program in LLVM.  This
-- is the type returned from all of the parsers, and all analysis
-- begins at the Module level.
data Module = Module { moduleIdentifier :: Text
                     , moduleDataLayout :: DataLayout
                     , moduleDataLayoutString :: Text
                       -- ^ The layout of the primitive datatypes on
                       -- the architecture this module was generated
                       -- for
                     , moduleTarget :: Text -- TargetTriple
                       -- ^ The architecture that this module was
                       -- generated for
                     , moduleAssembly :: Assembly
                       -- ^ Module-level assembly declarations
                     , moduleDefinedFunctions :: [Function]
                     , moduleGlobalVariables :: [GlobalVariable]
                     , moduleExternalValues :: [ExternalValue]
                     , moduleExternalFunctions :: [ExternalFunction]
                     , moduleAliases :: [GlobalAlias]
                     , moduleEnumMetadata :: [Metadata]
                       -- ^ All enumerations in the Module.  These
                       -- should provide a bit more information than
                       -- just combing the types appearing in
                       -- interfaces.
                     , moduleRetainedTypeMetadata :: [Metadata]
                       -- ^ The retained types in the Module.  Only
                       -- available with new-style metadata.
                     , moduleRetainedTypes :: [Type]
                       -- ^ All of the IR-level types retained by the
                       -- module.
                     , moduleNextId :: UniqueId
                     }

-- | Implementation of the Show instance
--
-- FIXME: Print out the external values and functions
printModule :: Module -> Builder
printModule Module { moduleIdentifier = _
                   , moduleDataLayoutString = layout
                   , moduleTarget = triple
                   , moduleAssembly = asm
                   , moduleAliases = aliases
                   , moduleGlobalVariables = vars
                   , moduleDefinedFunctions = funcs
                   , moduleExternalValues = _ -- evars
                   , moduleExternalFunctions = _ -- efuncs
                   } =
  mconcat [ layoutS, singleton '\n', tripleS, singleton '\n', asmS, singleton '\n'
          , aliasesS, singleton '\n', varS, fromString "\n\n", funcS, singleton '\n'
          ]
  where
    layoutS = mconcat [ fromString "target datalayout = \""
                      , fromString (show layout), singleton '"'
                      ]
    tripleS = mconcat [ fromString "target triple = \"", fromString (show triple), singleton '"' ]
    asmS = printAsm asm
    aliasesS = mconcat $ intersperse (fromString "\n") $ map (printValue . Value) aliases
    varS = mconcat $ intersperse (fromString "\n") $ map (printValue . Value) vars
    funcS = mconcat $ intersperse (fromString "\n\n") $ map (printValue . Value) funcs

-- | Get a list of all types of globals in the Module (functions,
-- aliases, and global variables)
moduleGlobals :: Module -> [Value]
moduleGlobals m = concat [ map Value $ moduleAliases m
                         , map Value $ moduleGlobalVariables m
                         , map Value $ moduleDefinedFunctions m
                         , map Value $ moduleExternalValues m
                         , map Value $ moduleExternalFunctions m
                         ]

instance Show Module where
  show = T.unpack . toStrict . toLazyText . printModule

instance NFData Module where
  rnf m = evalState (forceModule m) (S.empty, S.empty) `seq` ()

-- | Force the module to be fully evaluated to rnf form from the
-- top-down.  There are cycles, so we have to be careful to avoid
-- traversing them infinitely.
forceModule :: Module -> ForceMonad Module
forceModule m = do
  mapM_ forceGlobalAlias (moduleAliases m)
  mapM_ forceGlobalVariable (moduleGlobalVariables m)
  mapM_ forceFunction (moduleDefinedFunctions m)
  mapM_ forceExternalValue (moduleExternalValues m)
  mapM_ forceExternalFunction (moduleExternalFunctions m)
  mapM_ forceMetadataT (moduleEnumMetadata m)
  mapM_ forceMetadataT (moduleRetainedTypeMetadata m)
  return $ moduleAssembly m `deepseq` m `seq` m

-- | Find a function in the Module by its name.
findFunctionByName :: Module -> String -> Maybe Function
findFunctionByName m s = find isFunc $ moduleDefinedFunctions m
  where
    funcIdent = makeGlobalIdentifier (T.pack s)
    isFunc f = functionName f == funcIdent

-- | Find the function named 'main' in the 'Module', if any.
findMain :: Module -> Maybe Function
findMain m = findFunctionByName m "main"
