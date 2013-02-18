{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, GADTs, EmptyDataDecls, RankNTypes #-}
{-# LANGUAGE DataKinds, KindSignatures, InstanceSigs #-}
module Data.LLVM.Types.Referential (
  -- * Basic Types
  Type(..),
  structTypeToName,
  structBaseName,
  stripPointerTypes,
  UniqueId,
  IsValue(..),
  Value(..),
  toValue,
  valueContent',
  stripBitcasts,
  FromValue(..),
  -- * Functions
  Function(..),
  HasFunction(..),
  functionBody,
  functionInstructions,
  functionReturnType,
  functionExitBlock,
  functionExitBlocks,
  functionIsVararg,
  functionEntryInstruction,
  functionExitInstruction,
  functionExitInstructions,
  -- * External Functions
  ExternalFunction(..),
  externalIsIntrinsic,
  externalFunctionParameterTypes,
  -- * Arguments
  Argument(..),
  argumentIndex,
  -- * Basic Blocks
  BasicBlock(..),
  basicBlockInstructions,
  basicBlockTerminatorInstruction,
  firstNonPhiInstruction,
  isFirstNonPhiInstruction,
  basicBlockSplitPhiNodes,
  -- * Instructions
  InstrKind(..),
  InstrTag(..),
  Instruction(..),
  instructionType,
  instructionName,
  instructionBasicBlock,
  instructionMetadata,
  instructionUniqueId,
  instructionFunction,
  instructionIsTerminator,
  instructionIsEntry,
  instructionIsPhiNode,
  -- * Globals
  GlobalVariable(..),
  GlobalAlias(..),
  ExternalValue(..),
  -- * Constants
  Constant(..),
  -- * Metadata
  Metadata(..),
  -- * Debug info
  llvmDebugVersion
  ) where

import Control.DeepSeq
import Control.Exception
import Control.Failure
import Data.Hashable
import Data.Int
import Data.List ( elemIndex )
import Data.Ord ( comparing )
import Data.Text ( Text, isPrefixOf )
import Data.Typeable
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Text.Printf
import Text.Regex.TDFA
import Unsafe.Coerce ( unsafeCoerce )

import Data.LLVM.Types.Attributes
import Data.LLVM.Types.Dwarf
import Data.LLVM.Types.Identifiers

-- | This is the version of LLVM's debug information that this library
-- supports.
llvmDebugVersion :: Integer
llvmDebugVersion = 524288

-- This isn't very honest, but Values are part of Modules and
-- are fully evaluated before the module is constructed.
instance NFData (Instruction k t) where
  rnf _ = ()
instance NFData Value where
  rnf _ = ()
instance NFData BasicBlock where
  rnf _ = ()
instance NFData Function where
  rnf _ = ()
instance NFData Argument where
  rnf _ = ()
instance NFData Type where
  rnf _ = ()

-- | The type system of LLVM
data Type = TypeInteger !Int
            -- ^ Integral types; the parameter holds the number of
            -- bits required to represent the type.
          | TypeFloat
          | TypeDouble
          | TypeFP128
          | TypeX86FP80
          | TypePPCFP128
          | TypeX86MMX
          | TypeVoid
          | TypeLabel
          | TypeMetadata
          | TypeArray !Int Type
            -- ^ Fixed-length arrays, where the Int holds the number
            -- of elements in arrays of this type.
          | TypeVector !Int Type
            -- ^ Vectors with a fixed length.  These are vectors in
            -- the SSE sense.
          | TypeFunction Type [Type] !Bool
            -- ^ Functions with a return type, list of argument types,
            -- and a flag that denotes whether or not the function
            -- accepts varargs
          | TypePointer Type !Int
          | TypeStruct (Maybe String) [Type] !Bool -- isPacked
            -- ^ A wrapper for typedefs

-- | Strip off the struct. prefix and any .NNN suffixes added by LLVM
-- to a struct type name.  If the type is not a struct type, return
-- Nothing.
structTypeToName :: Type -> Maybe String
structTypeToName (TypeStruct (Just n) _ _) = Just $ structBaseName n
structTypeToName _ = Nothing

structBaseName :: String -> String
structBaseName s =
  let pfx:_ = captures
  in pfx
  where
    pattern :: String
    pattern = "([[:alpha:]]+\\.[:<> [:alnum:]_]+)(\\.[[:digit:]]+)*"
    m :: (String, String, String, [String])
    m = s =~ pattern
    (_, _, _, captures) = m


-- | Take a type and remove all of its pointer wrappers
stripPointerTypes :: Type -> Type
stripPointerTypes t =
  case t of
    TypePointer t' _ -> stripPointerTypes t'
    _ -> t


-- Deriving an Ord instance won't work because Type is a cyclic data
-- structure and the derived instances end up stuck in infinite loops.
-- Defining a more traditional one that just breaks cycles is really
-- tedious here, so just base Ord off of equality and then make the
-- ordering arbitrary (but consistent) based on the Hashable instance.
instance Ord Type where
  t1 `compare` t2 = case t1 == t2 of
    True -> EQ
    False -> comparing hash t1 t2

instance Hashable Type where
  hashWithSalt s (TypeInteger i) =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` i
  hashWithSalt s TypeFloat = s `hashWithSalt` (2 :: Int)
  hashWithSalt s TypeDouble = s `hashWithSalt` (3 :: Int)
  hashWithSalt s TypeFP128 = s `hashWithSalt` (4 :: Int)
  hashWithSalt s TypeX86FP80 = s `hashWithSalt` (5 :: Int)
  hashWithSalt s TypePPCFP128 = s `hashWithSalt` (6 :: Int)
  hashWithSalt s TypeX86MMX = s `hashWithSalt` (7 :: Int)
  hashWithSalt s TypeVoid = s `hashWithSalt` (8 :: Int)
  hashWithSalt s TypeLabel = s `hashWithSalt` (9 :: Int)
  hashWithSalt s TypeMetadata = s `hashWithSalt` (10 :: Int)
  hashWithSalt s (TypeArray i t) =
    s `hashWithSalt` (11 :: Int) `hashWithSalt` i `hashWithSalt` t
  hashWithSalt s (TypeVector i t) =
    s `hashWithSalt` (12 :: Int) `hashWithSalt` i `hashWithSalt` t
  hashWithSalt s (TypeFunction r ts v) =
    s `hashWithSalt` (13 :: Int) `hashWithSalt` r `hashWithSalt` ts `hashWithSalt` v
  hashWithSalt s (TypePointer t as) =
    s `hashWithSalt` (15 :: Int) `hashWithSalt` t `hashWithSalt` as
  hashWithSalt s (TypeStruct (Just n) _ _) =
    s `hashWithSalt` (16 :: Int) `hashWithSalt` n
  hashWithSalt s (TypeStruct Nothing ts p) =
    s `hashWithSalt` (17 :: Int) `hashWithSalt` ts `hashWithSalt` p

instance Eq Type where
  TypeInteger i1 == TypeInteger i2 = i1 == i2
  TypeFloat == TypeFloat = True
  TypeDouble == TypeDouble = True
  TypeFP128 == TypeFP128 = True
  TypeX86FP80 == TypeX86FP80 = True
  TypePPCFP128 == TypePPCFP128 = True
  TypeX86MMX == TypeX86MMX = True
  TypeVoid == TypeVoid = True
  TypeLabel == TypeLabel = True
  TypeMetadata == TypeMetadata = True
  TypeArray i1 t1 == TypeArray i2 t2 = i1 == i2 && t1 == t2
  TypeVector i1 t1 == TypeVector i2 t2 = i1 == i2 && t1 == t2
  TypeFunction r1 ts1 v1 == TypeFunction r2 ts2 v2 =
    v1 == v2 && r1 == r2 && ts1 == ts2
  TypePointer t1 as1 == TypePointer t2 as2 = t1 == t2 && as1 == as2
  TypeStruct (Just n1) _ _ == TypeStruct (Just n2) _ _ = n1 == n2
  TypeStruct Nothing ts1 p1 == TypeStruct Nothing ts2 p2 =
    ts1 == ts2 && p1 == p2
  _ == _ = False

data Metadata =
  MetaSourceLocation { metaValueUniqueId :: UniqueId
                     , metaSourceRow :: !Int32
                     , metaSourceCol :: !Int32
                     , metaSourceScope :: Maybe Metadata
                     }
  | MetaDWLexicalBlock { metaValueUniqueId :: UniqueId
                       , metaLexicalBlockRow :: !Int32
                       , metaLexicalBlockCol :: !Int32
                       , metaLexicalBlockContext :: Maybe Metadata
                       }
  | MetaDWNamespace { metaValueUniqueId :: UniqueId
                    , metaNamespaceContext :: Maybe Metadata
                    , metaNamespaceName :: !Text
                    , metaNamespaceLine :: !Int32
                    }
  | MetaDWCompileUnit { metaValueUniqueId :: UniqueId
                      , metaCompileUnitLanguage :: !DW_LANG
                      , metaCompileUnitSourceFile :: !Text
                      , metaCompileUnitCompileDir :: !Text
                      , metaCompileUnitProducer :: !Text
                      , metaCompileUnitIsMain :: !Bool
                      , metaCompileUnitIsOpt :: !Bool
                      , metaCompileUnitFlags :: !Text
                      , metaCompileUnitVersion :: !Int32
                      , metaCompileUnitEnumTypes :: [Maybe Metadata]
                      , metaCompileUnitRetainedTypes :: [Maybe Metadata]
                      , metaCompileUnitSubprograms :: [Maybe Metadata]
                      , metaCompileUnitGlobalVariables :: [Maybe Metadata]
                      }
  | MetaDWFile { metaValueUniqueId :: UniqueId
               , metaFileSourceFile :: !Text
               , metaFileSourceDir :: !Text
               }
  | MetaDWVariable { metaValueUniqueId :: UniqueId
                   , metaGlobalVarContext :: Maybe Metadata
                   , metaGlobalVarName :: !Text
                   , metaGlobalVarDisplayName :: !Text
                   , metaGlobalVarLinkageName :: !Text
                   , metaGlobalVarLine :: !Int32
                   , metaGlobalVarType :: Maybe Metadata
                   , metaGlobalVarStatic :: !Bool
                   , metaGlobalVarNotExtern :: !Bool
                   }
  | MetaDWSubprogram { metaValueUniqueId :: UniqueId
                     , metaSubprogramContext :: Maybe Metadata
                     , metaSubprogramName :: !Text
                     , metaSubprogramDisplayName :: !Text
                     , metaSubprogramLinkageName :: !Text
                     , metaSubprogramLine :: !Int32
                     , metaSubprogramType :: Maybe Metadata
                     , metaSubprogramIsExplicit :: !Bool
                     , metaSubprogramIsPrototyped :: !Bool
                     , metaSubprogramStatic :: !Bool
                     , metaSubprogramNotExtern :: !Bool
                     , metaSubprogramVirtuality :: !DW_VIRTUALITY
                     , metaSubprogramVirtIndex :: !Int32
                     , metaSubprogramBaseType :: Maybe Metadata
                     , metaSubprogramArtificial :: !Bool
                     , metaSubprogramOptimized :: !Bool
                     }
  | MetaDWBaseType { metaValueUniqueId :: UniqueId
                   , metaBaseTypeContext :: Maybe Metadata
                   , metaBaseTypeName :: !Text
                   , metaBaseTypeFile :: Maybe Metadata
                   , metaBaseTypeLine :: !Int32
                   , metaBaseTypeSize :: !Int64
                   , metaBaseTypeAlign :: !Int64
                   , metaBaseTypeOffset :: !Int64
                   , metaBaseTypeFlags :: !Int32
                   , metaBaseTypeEncoding :: !DW_ATE
                   }
  | MetaDWDerivedType { metaValueUniqueId :: UniqueId
                      , metaDerivedTypeTag :: !DW_TAG
                      , metaDerivedTypeContext :: Maybe Metadata
                      , metaDerivedTypeName :: !Text
                      , metaDerivedTypeFile :: Maybe Metadata
                      , metaDerivedTypeLine :: !Int32
                      , metaDerivedTypeSize :: !Int64
                      , metaDerivedTypeAlign :: !Int64
                      , metaDerivedTypeOffset :: !Int64
                      , metaDerivedTypeIsArtificial :: !Bool
                      , metaDerivedTypeIsVirtual :: !Bool
                      , metaDerivedTypeIsForward :: !Bool
                      , metaDerivedTypeIsPrivate :: !Bool
                      , metaDerivedTypeIsProtected :: !Bool
                      , metaDerivedTypeParent :: Maybe Metadata
                      }
  | MetaDWCompositeType { metaValueUniqueId :: UniqueId
                        , metaCompositeTypeTag :: !DW_TAG
                        , metaCompositeTypeContext :: Maybe Metadata
                        , metaCompositeTypeName :: !Text
                        , metaCompositeTypeFile :: Maybe Metadata
                        , metaCompositeTypeLine :: !Int32
                        , metaCompositeTypeSize :: !Int64
                        , metaCompositeTypeAlign :: !Int64
                        , metaCompositeTypeOffset :: !Int64
                        , metaCompositeTypeFlags :: !Int32
                        , metaCompositeTypeParent :: Maybe Metadata
                        , metaCompositeTypeMembers :: Maybe Metadata
                        , metaCompositeTypeRuntime :: !Int32
                        , metaCompositeTypeContainer :: Maybe Metadata
                        , metaCompositeTypeTemplateParams :: Maybe Metadata
                        , metaCompositeTypeIsArtificial :: !Bool
                        , metaCompositeTypeIsVirtual :: !Bool
                        , metaCompositeTypeIsForward :: !Bool
                        , metaCompositeTypeIsProtected :: !Bool
                        , metaCompositeTypeIsPrivate :: !Bool
                        , metaCompositeTypeIsByRefStruct :: !Bool
                        }
  | MetaDWSubrange { metaValueUniqueId :: UniqueId
                   , metaSubrangeLow :: !Int64
                   , metaSubrangeHigh :: !Int64
                   }
  | MetaDWEnumerator { metaValueUniqueId :: UniqueId
                     , metaEnumeratorName :: !Text
                     , metaEnumeratorValue :: !Int64
                     }
  | MetaDWLocal { metaValueUniqueId :: UniqueId
                , metaLocalTag :: !DW_TAG
                , metaLocalContext :: Maybe Metadata
                , metaLocalName :: !Text
                , metaLocalLine :: !Int32
                , metaLocalArgNo :: !Int32
                , metaLocalType :: Maybe Metadata
                , metaLocalIsArtificial :: !Bool
                , metaLocalIsBlockByRefVar :: !Bool
                , metaLocalAddrElements :: [Int64]
                }
  | MetaDWTemplateTypeParameter { metaValueUniqueId :: UniqueId
                                , metaTemplateTypeParameterContext :: Maybe Metadata
                                , metaTemplateTypeParameterType :: Maybe Metadata
                                , metaTemplateTypeParameterLine :: !Int32
                                , metaTemplateTypeParameterCol :: !Int32
                                , metaTemplateTypeParameterName :: !Text
                                }
  | MetaDWTemplateValueParameter { metaValueUniqueId :: UniqueId
                                 , metaTemplateValueParameterContext :: Maybe Metadata
                                 , metaTemplateValueParameterType :: Maybe Metadata
                                 , metaTemplateValueParameterLine :: !Int32
                                 , metaTemplateValueParameterCol :: !Int32
                                 , metaTemplateValueParameterValue :: !Int64
                                 , metaTemplateValueParameterName :: !Text
                                 }
  | MetadataUnknown { metaValueUniqueId :: UniqueId
                    , metaUnknownValue :: !Text
                    }
  | MetadataList { metaValueUniqueId :: UniqueId
                 , metaListElements :: [Maybe Metadata]
                 }

-- | The type of the unique identifiers that let us to work with
-- 'Value's and 'Metadata`, despite the cycles in the object graph.
-- These ids are typically used as hash keys and give objects of these
-- types identity.
type UniqueId = Int

instance Eq Metadata where
  mv1 == mv2 = metaValueUniqueId mv1 == metaValueUniqueId mv2

instance Ord Metadata where
  compare = comparing metaValueUniqueId

instance Hashable Metadata where
  hashWithSalt s = hashWithSalt s . metaValueUniqueId

-- | A wrapper around 'ValueT' values that tracks the 'Type', name,
-- and attached metadata. valueName is mostly informational at this
-- point.  All references will be resolved as part of the graph, but
-- the name will be useful for visualization purposes and
-- serialization.
-- data Value = forall a . IsValue a => Value a

-- Functions have parameters if they are not external
data Value = FunctionC Function
           | ArgumentC Argument
           | BasicBlockC BasicBlock
           | GlobalVariableC GlobalVariable
           | GlobalAliasC GlobalAlias
           | ExternalValueC ExternalValue
           | ExternalFunctionC ExternalFunction
           | InstructionC (forall (k :: InstrKind) (t :: InstrTag) . Instruction k t)
           | ConstantC Constant

class IsValue a where
  valueType :: a -> Type
  valueName :: a -> Maybe Identifier
  valueMetadata :: a -> [Metadata]
  valueContent :: a -> Value
  valueUniqueId :: a -> UniqueId

instance IsValue Value where
  valueType a =
    case a of
      FunctionC f -> functionType f
      ArgumentC arg -> argumentType arg
      BasicBlockC _ -> TypeLabel
      GlobalVariableC g -> globalVariableType g
      GlobalAliasC g -> valueType g
      ExternalValueC e -> externalValueType e
      ExternalFunctionC e -> externalFunctionType e
      InstructionC i -> instructionType i
      ConstantC c -> constantType c
  valueName a =
    case a of
      FunctionC f -> valueName f
      ArgumentC arg -> valueName arg
      BasicBlockC b -> valueName b
      GlobalVariableC g -> valueName g
      GlobalAliasC g -> valueName g
      ExternalValueC e -> valueName e
      ExternalFunctionC e -> valueName e
      InstructionC i -> valueName i
      ConstantC _ -> Nothing
  valueMetadata a =
    case a of
      FunctionC f -> functionMetadata f
      ArgumentC arg -> argumentMetadata arg
      BasicBlockC b -> basicBlockMetadata b
      GlobalVariableC g -> globalVariableMetadata g
      GlobalAliasC g -> valueMetadata g
      ExternalValueC e -> externalValueMetadata e
      ExternalFunctionC e -> externalFunctionMetadata e
      InstructionC i -> instructionMetadata i
      ConstantC _ -> []
  valueContent = id
  valueUniqueId a =
    case a of
      FunctionC f -> functionUniqueId f
      ArgumentC arg -> argumentUniqueId arg
      BasicBlockC b -> basicBlockUniqueId b
      GlobalVariableC g -> globalVariableUniqueId g
      GlobalAliasC g -> valueUniqueId g
      ExternalValueC e -> externalValueUniqueId e
      ExternalFunctionC e -> externalFunctionUniqueId e
      InstructionC i -> instructionUniqueId i
      ConstantC c -> constantUniqueId c

toValue :: (IsValue a) => a -> Value
toValue = valueContent

data FailedCast = FailedCast String
                deriving (Typeable, Show)

instance Exception FailedCast

-- | Safely convert a 'Value' to a concrete type (like Argument or
-- Instruction).  This is most useful in pure Monads that handle
-- failure, like Maybe, MaybeT, and Either.  If the requested
-- conversion is not possible, the normal failure semantics are
-- provided.  Example:
--
-- >
class FromValue a where
  fromValue :: (Failure FailedCast f) => Value -> f a

instance FromValue Constant where
  fromValue v =
    case valueContent' v of
      ConstantC c -> return c
      _ -> failure $! FailedCast "Constant"

instance FromValue GlobalAlias where
  fromValue v =
    case valueContent' v of
      GlobalAliasC g -> return g
      _ -> failure $! FailedCast "GlobalAlias"

instance FromValue ExternalValue where
  fromValue v =
    case valueContent' v of
      ExternalValueC e -> return e
      _ -> failure $! FailedCast "ExternalValue"

instance FromValue GlobalVariable where
  fromValue v =
    case valueContent' v of
      GlobalVariableC g -> return g
      _ -> failure $! FailedCast "GlobalVariable"

instance FromValue Argument where
  fromValue v =
    case valueContent' v of
      ArgumentC a -> return a
      _ -> failure $! FailedCast "Argument"

instance FromValue Function where
  fromValue v =
    case valueContent' v of
      FunctionC f -> return f
      _ -> failure $! FailedCast "Function"

instance FromValue (Instruction k t) where
  fromValue v =
    case valueContent' v of
      InstructionC i -> return i
      _ -> failure $! FailedCast "Instruction"

instance FromValue ExternalFunction where
  fromValue v =
    case valueContent' v of
      ExternalFunctionC f -> return f
      _ -> failure $! FailedCast "ExternalFunction"

instance FromValue BasicBlock where
  fromValue v =
    case valueContent' v of
      BasicBlockC b -> return b
      _ -> failure $! FailedCast "BasicBlock"


instance Eq Value where
  (==) = valueEq

{-# INLINE valueEq #-}
valueEq :: Value -> Value -> Bool
valueEq v1 v2 =
  valueUniqueId v1 == valueUniqueId v2

instance Ord Value where
  v1 `compare` v2 = comparing valueUniqueId v1 v2

instance Hashable Value where
  hashWithSalt s = hashWithSalt s . valueUniqueId

class HasFunction a where
  getFunction :: a -> Function

instance HasFunction Function where
  getFunction = id

data Function = Function { functionType :: Type
                         , functionName :: !Identifier
                         , functionMetadata :: [Metadata]
                         , functionUniqueId :: UniqueId
                         , functionParameters :: [Argument]
                         , functionBodyVector :: Vector BasicBlock
                         , functionLinkage :: !LinkageType
                         , functionVisibility :: !VisibilityStyle
                         , functionCC :: !CallingConvention
                         , functionRetAttrs :: [ParamAttribute]
                         , functionAttrs :: [FunctionAttribute]
                         , functionSection :: !(Maybe Text)
                         , functionAlign :: !Int64
                         , functionGCName :: !(Maybe Text)
                         }

functionIsVararg :: Function -> Bool
functionIsVararg Function { functionType = TypeFunction _ _ isva } = isva
functionIsVararg v = error $ printf "Value %d is not a function" (valueUniqueId v)

{-# INLINABLE functionReturnType #-}
functionReturnType :: Function -> Type
functionReturnType f = rt where
  TypeFunction rt _ _ = functionType f

{-# INLINABLE functionBody #-}
functionBody :: Function -> [BasicBlock]
functionBody = V.toList . functionBodyVector

{-# INLINABLE functionInstructions #-}
functionInstructions :: Function -> [Instruction k t]
functionInstructions = concatMap basicBlockInstructions . functionBody

functionEntryInstruction :: Function -> Instruction k t
functionEntryInstruction f = e1
  where
    (bb1:_) = functionBody f
    (e1:_) = basicBlockInstructions bb1

-- | Get the ret instruction for a Function
functionExitInstruction :: Function -> Maybe (Instruction k t)
functionExitInstruction f =
  case filter isRetInst is of
    [] -> Nothing -- error $ "Function has no ret instruction: " ++ show (functionName f)
    [ri] -> Just ri
    _ -> Nothing -- error $ "Function has multiple ret instructions: " ++ show (functionName f)
  where
    is = concatMap basicBlockInstructions (functionBody f)
    isRetInst RetInst {} = True
    isRetInst _ = False

-- | Get all exit instructions for a Function (ret, unreachable, unwind)
functionExitInstructions :: Function -> [Instruction k t]
functionExitInstructions f = filter isRetInst is
  where
    is = concatMap basicBlockInstructions (functionBody f)
    isRetInst RetInst {} = True
    isRetInst UnreachableInst {} = True
    isRetInst _ = False

functionExitBlock :: Function -> BasicBlock
functionExitBlock f =
  case filter terminatorIsExitInst bbs of
    [] -> error $ "Function has no ret instruction: " ++ show (functionName f)
    [rb] -> rb
    _ -> error $ "Function has multiple ret instructions: " ++ show (functionName f)
  where
    bbs = functionBody f
    terminatorIsExitInst bb =
      case basicBlockTerminatorInstruction bb of
        RetInst {} -> True
        _ -> False

functionExitBlocks :: Function -> [BasicBlock]
functionExitBlocks f =
  case filter terminatorIsExitInst bbs of
    [] -> error $ "Function has no ret instruction: " ++ show (functionName f)
    rbs -> rbs
  where
    bbs = functionBody f
    terminatorIsExitInst bb =
      case basicBlockTerminatorInstruction bb of
        RetInst {} -> True
        UnreachableInst {} -> True
        ResumeInst {} -> True
        _ -> False

instance IsValue Function where
  valueType = functionType
  valueName = Just . functionName
  valueMetadata = functionMetadata
  valueContent = FunctionC
  valueUniqueId = functionUniqueId

instance Eq Function where
  f1 == f2 = functionUniqueId f1 == functionUniqueId f2

instance Hashable Function where
  hashWithSalt s = hashWithSalt s . functionUniqueId

instance Ord Function where
  f1 `compare` f2 = comparing functionUniqueId f1 f2

data Argument = Argument { argumentType :: Type
                         , argumentName :: !Identifier
                         , argumentMetadata :: [Metadata]
                         , argumentUniqueId :: UniqueId
                         , argumentParamAttrs :: [ParamAttribute]
                         , argumentFunction :: Function
                         }

instance IsValue Argument where
  valueType = argumentType
  valueName = Just . argumentName
  valueMetadata = argumentMetadata
  valueContent = ArgumentC
  valueUniqueId = argumentUniqueId

instance Hashable Argument where
  hashWithSalt s = hashWithSalt s . argumentUniqueId

instance Eq Argument where
  a1 == a2 = argumentUniqueId a1 == argumentUniqueId a2

instance Ord Argument where
  a1 `compare` a2 = comparing argumentUniqueId a1 a2

-- | Find the zero-based index into the argument list of the 'Function'
-- containing this 'Argument'.
argumentIndex :: Argument -> Int
argumentIndex a = ix
  where
    f = argumentFunction a
    Just ix = elemIndex a (functionParameters f)

data BasicBlock =
  BasicBlock { basicBlockName :: !Identifier
             , basicBlockMetadata :: [Metadata]
             , basicBlockUniqueId :: UniqueId
             , basicBlockInstructionVector :: forall k t . Vector (Instruction k t)
             , basicBlockTerminatorInstruction :: forall t . Instruction Terminator t
             , basicBlockFunction :: Function
             }

{-# INLINABLE basicBlockInstructions #-}
basicBlockInstructions :: BasicBlock -> [Instruction k t]
basicBlockInstructions = V.toList . basicBlockInstructionVector

-- {-# INLINABLE basicBlockTerminatorInstruction #-}
-- basicBlockTerminatorInstruction :: BasicBlock -> Instruction Terminator t
-- basicBlockTerminatorInstruction = V.last . basicBlockTerminatorInstruction

{-# INLINABLE firstNonPhiInstruction #-}
-- | Get the first instruction in a basic block that is not a Phi
-- node.  This is total because basic blocks cannot be empty and must
-- end in a terminator instruction (Phi nodes are not terminators).
firstNonPhiInstruction :: BasicBlock -> Instruction k t
firstNonPhiInstruction bb = i
  where
    i : _ = dropWhile instructionIsPhiNode (basicBlockInstructions bb)

{-# INLINABLE instructionIsPhiNode #-}
-- | Predicate to test an instruction to see if it is a phi node
instructionIsPhiNode :: Instruction k t -> Bool
instructionIsPhiNode v = case v of
  PhiNode {} -> True
  _ -> False

{-# INLINABLE isFirstNonPhiInstruction #-}
-- | Determine if @i@ is the first non-phi instruction in its block.
isFirstNonPhiInstruction :: Instruction k t -> Bool
isFirstNonPhiInstruction i = i == firstNonPhiInstruction bb
  where
    Just bb = instructionBasicBlock i

{-# INLINABLE basicBlockSplitPhiNodes #-}
-- | Split a block's instructions into phi nodes and the rest
basicBlockSplitPhiNodes :: BasicBlock -> ([Instruction Phi t], [Instruction k t])
basicBlockSplitPhiNodes = undefined -- span instructionIsPhiNode . basicBlockInstructions

instance IsValue BasicBlock where
  valueType _ = TypeLabel
  valueName = Just . basicBlockName
  valueMetadata = basicBlockMetadata
  valueContent = BasicBlockC
  valueUniqueId = basicBlockUniqueId

instance Hashable BasicBlock where
  hashWithSalt s = hashWithSalt s . basicBlockUniqueId

instance Eq BasicBlock where
  f1 == f2 = basicBlockUniqueId f1 == basicBlockUniqueId f2

instance Ord BasicBlock where
  b1 `compare` b2 = comparing basicBlockUniqueId b1 b2

data GlobalVariable = GlobalVariable { globalVariableType :: Type
                                     , globalVariableName :: !Identifier
                                     , globalVariableMetadata :: [Metadata]
                                     , globalVariableUniqueId :: UniqueId
                                     , globalVariableLinkage :: !LinkageType
                                     , globalVariableVisibility :: !VisibilityStyle
                                     , globalVariableInitializer :: Maybe Value
                                     , globalVariableAlignment :: !Int64
                                     , globalVariableSection :: !(Maybe Text)
                                     , globalVariableIsThreadLocal :: !Bool
                                     , globalVariableIsConstant :: !Bool
                                     }

instance IsValue GlobalVariable where
  valueType = globalVariableType
  valueName = Just . globalVariableName
  valueMetadata = globalVariableMetadata
  valueContent = GlobalVariableC
  valueUniqueId = globalVariableUniqueId

instance Eq GlobalVariable where
  f1 == f2 = globalVariableUniqueId f1 == globalVariableUniqueId f2

instance Hashable GlobalVariable where
  hashWithSalt s = hashWithSalt s . globalVariableUniqueId

instance Ord GlobalVariable where
  g1 `compare` g2 = comparing globalVariableUniqueId g1 g2

data GlobalAlias = GlobalAlias { globalAliasTarget :: Value
                               , globalAliasLinkage :: !LinkageType
                               , globalAliasName :: !Identifier
                               , globalAliasVisibility :: !VisibilityStyle
                               , globalAliasMetadata :: [Metadata]
                               , globalAliasUniqueId :: UniqueId
                               }

instance IsValue GlobalAlias where
  valueType = valueType . globalAliasTarget
  valueName = Just . globalAliasName
  valueMetadata = globalAliasMetadata
  valueContent = GlobalAliasC
  valueUniqueId = globalAliasUniqueId

instance Eq GlobalAlias where
  f1 == f2 = globalAliasUniqueId f1 == globalAliasUniqueId f2

instance Hashable GlobalAlias where
  hashWithSalt s = hashWithSalt s . globalAliasUniqueId

instance Ord GlobalAlias where
  g1 `compare` g2 = comparing globalAliasUniqueId g1 g2

data ExternalValue = ExternalValue { externalValueType :: Type
                                   , externalValueName :: !Identifier
                                   , externalValueMetadata :: [Metadata]
                                   , externalValueUniqueId :: UniqueId
                                   }

instance IsValue ExternalValue where
  valueType = externalValueType
  valueName = Just . externalValueName
  valueMetadata = externalValueMetadata
  valueContent = ExternalValueC
  valueUniqueId = externalValueUniqueId

instance Eq ExternalValue where
  f1 == f2 = externalValueUniqueId f1 == externalValueUniqueId f2

instance Hashable ExternalValue where
  hashWithSalt s = hashWithSalt s . externalValueUniqueId

instance Ord ExternalValue where
  e1 `compare` e2 = comparing externalValueUniqueId e1 e2

data ExternalFunction = ExternalFunction { externalFunctionType :: Type
                                         , externalFunctionName :: !Identifier
                                         , externalFunctionMetadata :: [Metadata]
                                         , externalFunctionUniqueId :: UniqueId
                                         , externalFunctionAttrs :: [FunctionAttribute]
                                         }

instance Show ExternalFunction where
  show = show . externalFunctionName

instance IsValue ExternalFunction where
  valueType = externalFunctionType
  valueName = Just . externalFunctionName
  valueMetadata = externalFunctionMetadata
  valueContent = ExternalFunctionC
  valueUniqueId = externalFunctionUniqueId

instance Eq ExternalFunction where
  f1 == f2 = externalFunctionUniqueId f1 == externalFunctionUniqueId f2

instance Hashable ExternalFunction where
  hashWithSalt s = hashWithSalt s . externalFunctionUniqueId

instance Ord ExternalFunction where
  f1 `compare` f2 = comparing externalFunctionUniqueId f1 f2

externalFunctionParameterTypes :: ExternalFunction -> [Type]
externalFunctionParameterTypes ef = ts
  where
    TypeFunction _ ts _ = externalFunctionType ef

externalIsIntrinsic :: ExternalFunction -> Bool
externalIsIntrinsic =
  isPrefixOf "llvm." . identifierContent . externalFunctionName


{-# INLINABLE instructionIsTerminator #-}
-- | Determine if an instruction is a Terminator instruction (i.e.,
-- ends a BasicBlock).  Note that this takes generic instructions; if
-- you already have an @Instruction Terminator t@ then you don't need
-- this function.
instructionIsTerminator :: Instruction k t -> Bool
instructionIsTerminator RetInst {} = True
instructionIsTerminator UnconditionalBranchInst {} = True
instructionIsTerminator BranchInst {} = True
instructionIsTerminator SwitchInst {} = True
instructionIsTerminator IndirectBranchInst {} = True
instructionIsTerminator ResumeInst {} = True
instructionIsTerminator UnreachableInst {} = True
instructionIsTerminator InvokeInst {} = True
instructionIsTerminator _ = False

instructionIsEntry :: Instruction k t -> Bool
instructionIsEntry i = i == ei
  where
    ei = V.unsafeHead $ basicBlockInstructionVector bb
    Just bb = instructionBasicBlock i

instructionFunction :: Instruction k t -> Maybe Function
instructionFunction i = do
  bb <- instructionBasicBlock i
  return $ basicBlockFunction bb

instructionType :: Instruction k t -> Type
instructionType i =
  case i of
    RetInst {} -> TypeVoid
    UnconditionalBranchInst {} -> TypeVoid
    BranchInst {} -> TypeVoid
    SwitchInst {} -> TypeVoid
    IndirectBranchInst {} -> TypeVoid
    ResumeInst {} -> TypeVoid
    UnreachableInst {} -> TypeVoid
    StoreInst {} -> TypeVoid
    FenceInst {} -> TypeVoid
    AtomicCmpXchgInst {} -> TypeVoid
    AtomicRMWInst {} -> TypeVoid
    ExtractElementInst { extractElementType = t } -> t
    InsertElementInst { insertElementType = t } -> t
    ShuffleVectorInst { shuffleVectorType = t } -> t
    ExtractValueInst { extractValueType = t } -> t
    InsertValueInst { insertValueType = t } -> t
    AllocaInst { allocaType = t } -> t
    LoadInst { loadType = t } -> t
    AddInst { binaryType = t } -> t
    SubInst { binaryType = t } -> t
    MulInst { binaryType = t } -> t
    DivInst { binaryType = t } -> t
    RemInst { binaryType = t } -> t
    ShlInst { binaryType = t } -> t
    LshrInst { binaryType = t } -> t
    AshrInst { binaryType = t } -> t
    AndInst { binaryType = t } -> t
    OrInst { binaryType = t } -> t
    XorInst { binaryType = t } -> t
    TruncInst { castType = t } -> t
    ZExtInst { castType = t } -> t
    SExtInst { castType = t } -> t
    FPTruncInst { castType = t } -> t
    FPExtInst { castType = t } -> t
    FPToSIInst { castType = t } -> t
    FPToUIInst { castType = t } -> t
    SIToFPInst { castType = t } -> t
    UIToFPInst { castType = t } -> t
    PtrToIntInst { castType = t } -> t
    IntToPtrInst { castType = t } -> t
    BitcastInst { castType = t } -> t
    ICmpInst { cmpType = t } -> t
    FCmpInst { cmpType = t } -> t
    SelectInst { selectType = t } -> t
    CallInst { callType = t } -> t
    GetElementPtrInst { getElementPtrType = t } -> t
    InvokeInst { invokeType = t } -> t
    VaArgInst { vaArgType = t } -> t
    LandingPadInst { landingPadType = t } -> t
    PhiNode { phiType = t } -> t

instructionUniqueId :: Instruction k t -> UniqueId
instructionUniqueId i =
  case i of
    RetInst { retInstUniqueId = t } -> t
    UnconditionalBranchInst { branchUniqueId = t } -> t
    BranchInst { branchUniqueId = t } -> t
    SwitchInst { switchUniqueId = t } -> t
    IndirectBranchInst { indirectBranchUniqueId = t } -> t
    ResumeInst { resumeUniqueId = t } -> t
    UnreachableInst { unreachableUniqueId = t } -> t
    StoreInst { storeUniqueId = t } -> t
    FenceInst { fenceUniqueId = t } -> t
    AtomicCmpXchgInst { atomicCmpXchgUniqueId = t } -> t
    AtomicRMWInst { atomicRMWUniqueId = t } -> t
    ExtractElementInst { extractElementUniqueId = t } -> t
    InsertElementInst { insertElementUniqueId = t } -> t
    ShuffleVectorInst { shuffleVectorUniqueId = t } -> t
    ExtractValueInst { extractValueUniqueId = t } -> t
    InsertValueInst { insertValueUniqueId = t } -> t
    AllocaInst { allocaUniqueId = t } -> t
    LoadInst { loadUniqueId = t } -> t
    AddInst { binaryUniqueId = t } -> t
    SubInst { binaryUniqueId = t } -> t
    MulInst { binaryUniqueId = t } -> t
    DivInst { binaryUniqueId = t } -> t
    RemInst { binaryUniqueId = t } -> t
    ShlInst { binaryUniqueId = t } -> t
    LshrInst { binaryUniqueId = t } -> t
    AshrInst { binaryUniqueId = t } -> t
    AndInst { binaryUniqueId = t } -> t
    OrInst { binaryUniqueId = t } -> t
    XorInst { binaryUniqueId = t } -> t
    TruncInst { castUniqueId = t } -> t
    ZExtInst { castUniqueId = t } -> t
    SExtInst { castUniqueId = t } -> t
    FPTruncInst { castUniqueId = t } -> t
    FPExtInst { castUniqueId = t } -> t
    FPToSIInst { castUniqueId = t } -> t
    FPToUIInst { castUniqueId = t } -> t
    SIToFPInst { castUniqueId = t } -> t
    UIToFPInst { castUniqueId = t } -> t
    PtrToIntInst { castUniqueId = t } -> t
    IntToPtrInst { castUniqueId = t } -> t
    BitcastInst { castUniqueId = t } -> t
    ICmpInst { cmpUniqueId = t } -> t
    FCmpInst { cmpUniqueId = t } -> t
    SelectInst { selectUniqueId = t } -> t
    CallInst { callUniqueId = t } -> t
    GetElementPtrInst { getElementPtrUniqueId = t } -> t
    InvokeInst { invokeUniqueId = t } -> t
    VaArgInst { vaArgUniqueId = t } -> t
    LandingPadInst { landingPadUniqueId = t } -> t
    PhiNode { phiUniqueId = t } -> t

instructionBasicBlock :: Instruction k t -> Maybe BasicBlock
instructionBasicBlock i =
  case i of
    RetInst { retInstBasicBlock = t } -> t
    UnconditionalBranchInst { branchBasicBlock = t } -> t
    BranchInst { branchBasicBlock = t } -> t
    SwitchInst { switchBasicBlock = t } -> t
    IndirectBranchInst { indirectBranchBasicBlock = t } -> t
    ResumeInst { resumeBasicBlock = t } -> t
    UnreachableInst { unreachableBasicBlock = t } -> t
    StoreInst { storeBasicBlock = t } -> t
    FenceInst { fenceBasicBlock = t } -> t
    AtomicCmpXchgInst { atomicCmpXchgBasicBlock = t } -> t
    AtomicRMWInst { atomicRMWBasicBlock = t } -> t
    ExtractElementInst { extractElementBasicBlock = t } -> t
    InsertElementInst { insertElementBasicBlock = t } -> t
    ShuffleVectorInst { shuffleVectorBasicBlock = t } -> t
    ExtractValueInst { extractValueBasicBlock = t } -> t
    InsertValueInst { insertValueBasicBlock = t } -> t
    AllocaInst { allocaBasicBlock = t } -> t
    LoadInst { loadBasicBlock = t } -> t
    AddInst { binaryBasicBlock = t } -> t
    SubInst { binaryBasicBlock = t } -> t
    MulInst { binaryBasicBlock = t } -> t
    DivInst { binaryBasicBlock = t } -> t
    RemInst { binaryBasicBlock = t } -> t
    ShlInst { binaryBasicBlock = t } -> t
    LshrInst { binaryBasicBlock = t } -> t
    AshrInst { binaryBasicBlock = t } -> t
    AndInst { binaryBasicBlock = t } -> t
    OrInst { binaryBasicBlock = t } -> t
    XorInst { binaryBasicBlock = t } -> t
    TruncInst { castBasicBlock = t } -> t
    ZExtInst { castBasicBlock = t } -> t
    SExtInst { castBasicBlock = t } -> t
    FPTruncInst { castBasicBlock = t } -> t
    FPExtInst { castBasicBlock = t } -> t
    FPToSIInst { castBasicBlock = t } -> t
    FPToUIInst { castBasicBlock = t } -> t
    SIToFPInst { castBasicBlock = t } -> t
    UIToFPInst { castBasicBlock = t } -> t
    PtrToIntInst { castBasicBlock = t } -> t
    IntToPtrInst { castBasicBlock = t } -> t
    BitcastInst { castBasicBlock = t } -> t
    ICmpInst { cmpBasicBlock = t } -> t
    FCmpInst { cmpBasicBlock = t } -> t
    SelectInst { selectBasicBlock = t } -> t
    CallInst { callBasicBlock = t } -> t
    GetElementPtrInst { getElementPtrBasicBlock = t } -> t
    InvokeInst { invokeBasicBlock = t } -> t
    VaArgInst { vaArgBasicBlock = t } -> t
    LandingPadInst { landingPadBasicBlock = t } -> t
    PhiNode { phiBasicBlock = t } -> t

instructionMetadata :: Instruction k t -> [Metadata]
instructionMetadata i =
  case i of
    RetInst { retInstMetadata = t } -> t
    UnconditionalBranchInst { branchMetadata = t } -> t
    BranchInst { branchMetadata = t } -> t
    SwitchInst { switchMetadata = t } -> t
    IndirectBranchInst { indirectBranchMetadata = t } -> t
    ResumeInst { resumeMetadata = t } -> t
    UnreachableInst { unreachableMetadata = t } -> t
    StoreInst { storeMetadata = t } -> t
    FenceInst { fenceMetadata = t } -> t
    AtomicCmpXchgInst { atomicCmpXchgMetadata = t } -> t
    AtomicRMWInst { atomicRMWMetadata = t } -> t
    ExtractElementInst { extractElementMetadata = t } -> t
    InsertElementInst { insertElementMetadata = t } -> t
    ShuffleVectorInst { shuffleVectorMetadata = t } -> t
    ExtractValueInst { extractValueMetadata = t } -> t
    InsertValueInst { insertValueMetadata = t } -> t
    AllocaInst { allocaMetadata = t } -> t
    LoadInst { loadMetadata = t } -> t
    AddInst { binaryMetadata = t } -> t
    SubInst { binaryMetadata = t } -> t
    MulInst { binaryMetadata = t } -> t
    DivInst { binaryMetadata = t } -> t
    RemInst { binaryMetadata = t } -> t
    ShlInst { binaryMetadata = t } -> t
    LshrInst { binaryMetadata = t } -> t
    AshrInst { binaryMetadata = t } -> t
    AndInst { binaryMetadata = t } -> t
    OrInst { binaryMetadata = t } -> t
    XorInst { binaryMetadata = t } -> t
    TruncInst { castMetadata = t } -> t
    ZExtInst { castMetadata = t } -> t
    SExtInst { castMetadata = t } -> t
    FPTruncInst { castMetadata = t } -> t
    FPExtInst { castMetadata = t } -> t
    FPToSIInst { castMetadata = t } -> t
    FPToUIInst { castMetadata = t } -> t
    SIToFPInst { castMetadata = t } -> t
    UIToFPInst { castMetadata = t } -> t
    PtrToIntInst { castMetadata = t } -> t
    IntToPtrInst { castMetadata = t } -> t
    BitcastInst { castMetadata = t } -> t
    ICmpInst { cmpMetadata = t } -> t
    FCmpInst { cmpMetadata = t } -> t
    SelectInst { selectMetadata = t } -> t
    CallInst { callMetadata = t } -> t
    GetElementPtrInst { getElementPtrMetadata = t } -> t
    InvokeInst { invokeMetadata = t } -> t
    VaArgInst { vaArgMetadata = t } -> t
    LandingPadInst { landingPadMetadata = t } -> t
    PhiNode { phiMetadata = t } -> t

instructionName :: Instruction k t -> Maybe Identifier
instructionName i =
  case i of
    RetInst {} -> Nothing
    UnconditionalBranchInst {} -> Nothing
    BranchInst {} -> Nothing
    SwitchInst {} -> Nothing
    IndirectBranchInst {} -> Nothing
    ResumeInst {} -> Nothing
    UnreachableInst {} -> Nothing
    StoreInst {} -> Nothing
    FenceInst {} -> Nothing
    AtomicCmpXchgInst {} -> Nothing
    AtomicRMWInst {} -> Nothing
    ExtractElementInst { extractElementName = t } -> t
    InsertElementInst { insertElementName = t } -> t
    ShuffleVectorInst { shuffleVectorName = t } -> t
    ExtractValueInst { extractValueName = t } -> t
    InsertValueInst { insertValueName = t } -> t
    AllocaInst { allocaName = t } -> t
    LoadInst { loadName = t } -> t
    AddInst { binaryName = t } -> t
    SubInst { binaryName = t } -> t
    MulInst { binaryName = t } -> t
    DivInst { binaryName = t } -> t
    RemInst { binaryName = t } -> t
    ShlInst { binaryName = t } -> t
    LshrInst { binaryName = t } -> t
    AshrInst { binaryName = t } -> t
    AndInst { binaryName = t } -> t
    OrInst { binaryName = t } -> t
    XorInst { binaryName = t } -> t
    TruncInst { castName = t } -> t
    ZExtInst { castName = t } -> t
    SExtInst { castName = t } -> t
    FPTruncInst { castName = t } -> t
    FPExtInst { castName = t } -> t
    FPToSIInst { castName = t } -> t
    FPToUIInst { castName = t } -> t
    SIToFPInst { castName = t } -> t
    UIToFPInst { castName = t } -> t
    PtrToIntInst { castName = t } -> t
    IntToPtrInst { castName = t } -> t
    BitcastInst { castName = t } -> t
    ICmpInst { cmpName = t } -> t
    FCmpInst { cmpName = t } -> t
    SelectInst { selectName = t } -> t
    CallInst { callName = t } -> t
    GetElementPtrInst { getElementPtrName = t } -> t
    InvokeInst { invokeName = t } -> t
    VaArgInst { vaArgName = t } -> t
    LandingPadInst { landingPadName = t } -> t
    PhiNode { phiName = t } -> t

data InstrKind = Terminator
               | BinaryOp
               | VectorOp
               | AggregateOp
               | MemoryOp
               | ConversionOp
               | CompareOp
               | OtherOp
               | Phi

data InstrTag = CallLike
              | ConstLike
              | OtherTag

-- | The
data Instruction :: InstrKind -> InstrTag -> * where -- k t where
  RetInst :: { retInstMetadata :: [Metadata]
             , retInstUniqueId :: UniqueId
             , retInstBasicBlock :: Maybe BasicBlock
             , retInstValue :: Maybe Value
             } -> Instruction Terminator OtherTag
  UnconditionalBranchInst :: { branchMetadata :: [Metadata]
                             , branchUniqueId :: UniqueId
                             , branchBasicBlock :: Maybe BasicBlock
                             , unconditionalBranchTarget :: BasicBlock
                             } -> Instruction Terminator OtherTag
  BranchInst :: { branchMetadata :: [Metadata]
                , branchUniqueId :: UniqueId
                , branchBasicBlock :: Maybe BasicBlock
                , branchCondition :: Value
                , branchTrueTarget :: BasicBlock
                , branchFalseTarget :: BasicBlock
                } -> Instruction Terminator OtherTag
  SwitchInst :: { switchMetadata :: [Metadata]
                , switchUniqueId :: UniqueId
                , switchBasicBlock :: Maybe BasicBlock
                , switchValue :: Value
                , switchDefaultTarget :: BasicBlock
                , switchCases :: [(Value, BasicBlock)]
                } -> Instruction Terminator OtherTag
  IndirectBranchInst :: { indirectBranchMetadata :: [Metadata]
                        , indirectBranchUniqueId :: UniqueId
                        , indirectBranchBasicBlock :: Maybe BasicBlock
                        , indirectBranchAddress :: Value
                        , indirectBranchTargets :: [BasicBlock]
                        } -> Instruction Terminator OtherTag
                        -- ^ The target must be derived from a blockaddress constant
                        -- The list is a list of possible target destinations
  ResumeInst :: { resumeMetadata :: [Metadata]
                , resumeUniqueId :: UniqueId
                , resumeBasicBlock :: Maybe BasicBlock
                , resumeException :: Value
                } -> Instruction Terminator OtherTag
  UnreachableInst :: { unreachableMetadata :: [Metadata]
                     , unreachableUniqueId :: UniqueId
                     , unreachableBasicBlock :: Maybe BasicBlock
                     } -> Instruction Terminator OtherTag
  ExtractElementInst :: { extractElementType :: Type
                        , extractElementName :: !(Maybe Identifier)
                        , extractElementMetadata :: [Metadata]
                        , extractElementUniqueId :: UniqueId
                        , extractElementBasicBlock :: Maybe BasicBlock
                        , extractElementVector :: Value
                        , extractElementIndex :: Value
                        } -> Instruction VectorOp ConstLike
  InsertElementInst :: { insertElementType :: Type
                       , insertElementName :: !(Maybe Identifier)
                       , insertElementMetadata :: [Metadata]
                       , insertElementUniqueId :: UniqueId
                       , insertElementBasicBlock :: Maybe BasicBlock
                       , insertElementVector :: Value
                       , insertElementValue :: Value
                       , insertElementIndex :: Value
                       } -> Instruction VectorOp ConstLike
  ShuffleVectorInst :: { shuffleVectorType :: Type
                       , shuffleVectorName :: !(Maybe Identifier)
                       , shuffleVectorMetadata :: [Metadata]
                       , shuffleVectorUniqueId :: UniqueId
                       , shuffleVectorBasicBlock :: Maybe BasicBlock
                       , shuffleVectorV1 :: Value
                       , shuffleVectorV2 :: Value
                       , shuffleVectorMask :: Value
                       } -> Instruction VectorOp ConstLike
  ExtractValueInst :: { extractValueType :: Type
                      , extractValueName :: !(Maybe Identifier)
                      , extractValueMetadata :: [Metadata]
                      , extractValueUniqueId :: UniqueId
                      , extractValueBasicBlock :: Maybe BasicBlock
                      , extractValueAggregate :: Value
                      , extractValueIndices :: [Int]
                      } -> Instruction AggregateOp ConstLike
  InsertValueInst :: { insertValueType :: Type
                     , insertValueName :: !(Maybe Identifier)
                     , insertValueMetadata :: [Metadata]
                     , insertValueUniqueId :: UniqueId
                     , insertValueBasicBlock :: Maybe BasicBlock
                     , insertValueAggregate :: Value
                     , insertValueValue :: Value
                     , insertValueIndices :: [Int]
                     } -> Instruction AggregateOp ConstLike
  AllocaInst :: { allocaType :: Type
                , allocaName :: !(Maybe Identifier)
                , allocaMetadata :: [Metadata]
                , allocaUniqueId :: UniqueId
                , allocaBasicBlock :: Maybe BasicBlock
                , allocaNumElements :: Value
                , allocaAlign :: !Int64
                } -> Instruction MemoryOp OtherTag
  LoadInst :: { loadType :: Type
              , loadName :: !(Maybe Identifier)
              , loadMetadata :: [Metadata]
              , loadUniqueId :: UniqueId
              , loadBasicBlock :: Maybe BasicBlock
              , loadIsVolatile :: !Bool
              , loadAddress :: Value
              , loadAlignment :: !Int64
              } -> Instruction MemoryOp OtherTag
  StoreInst :: { storeMetadata :: [Metadata]
               , storeUniqueId :: UniqueId
               , storeBasicBlock :: Maybe BasicBlock
               , storeIsVolatile :: !Bool
               , storeValue :: Value
               , storeAddress :: Value
               , storeAlignment :: !Int64
               , storeAddressSpace :: !Int
               } -> Instruction MemoryOp OtherTag
  FenceInst :: { fenceMetadata :: [Metadata]
               , fenceUniqueId :: UniqueId
               , fenceBasicBlock :: Maybe BasicBlock
               , fenceOrdering :: !AtomicOrdering
               , fenceScope :: !SynchronizationScope
               } -> Instruction MemoryOp OtherTag
  AtomicCmpXchgInst :: { atomicCmpXchgMetadata :: [Metadata]
                       , atomicCmpXchgUniqueId :: UniqueId
                       , atomicCmpXchgBasicBlock :: Maybe BasicBlock
                       , atomicCmpXchgOrdering :: !AtomicOrdering
                       , atomicCmpXchgScope :: !SynchronizationScope
                       , atomicCmpXchgIsVolatile :: !Bool
                       , atomicCmpXchgAddressSpace :: !Int
                       , atomicCmpXchgPointer :: Value
                       , atomicCmpXchgComparison :: Value
                       , atomicCmpXchgNewValue :: Value
                       } -> Instruction MemoryOp OtherTag
  AtomicRMWInst :: { atomicRMWMetadata :: [Metadata]
                   , atomicRMWUniqueId :: UniqueId
                   , atomicRMWBasicBlock :: Maybe BasicBlock
                   , atomicRMWOrdering :: !AtomicOrdering
                   , atomicRMWScope :: !SynchronizationScope
                   , atomicRMWOperation :: !AtomicOperation
                   , atomicRMWIsVolatile :: !Bool
                   , atomicRMWPointer :: Value
                   , atomicRMWValue :: Value
                   , atomicRMWAddressSpace :: !Int
                   } -> Instruction MemoryOp OtherTag
  AddInst :: { binaryType :: Type
             , binaryName :: !(Maybe Identifier)
             , binaryMetadata :: [Metadata]
             , binaryUniqueId :: UniqueId
             , binaryBasicBlock :: Maybe BasicBlock
             , binaryArithFlags :: !ArithFlags
             , binaryLhs :: Value
             , binaryRhs :: Value
             } -> Instruction BinaryOp ConstLike
  SubInst :: { binaryType :: Type
             , binaryName :: !(Maybe Identifier)
             , binaryMetadata :: [Metadata]
             , binaryUniqueId :: UniqueId
             , binaryBasicBlock :: Maybe BasicBlock
             , binaryArithFlags :: !ArithFlags
             , binaryLhs :: Value
             , binaryRhs :: Value
             } -> Instruction BinaryOp ConstLike
  MulInst :: { binaryType :: Type
             , binaryName :: !(Maybe Identifier)
             , binaryMetadata :: [Metadata]
             , binaryUniqueId :: UniqueId
             , binaryBasicBlock :: Maybe BasicBlock
             , binaryArithFlags :: !ArithFlags
             , binaryLhs :: Value
             , binaryRhs :: Value
             } -> Instruction BinaryOp ConstLike
  DivInst :: { binaryType :: Type
             , binaryName :: !(Maybe Identifier)
             , binaryMetadata :: [Metadata]
             , binaryUniqueId :: UniqueId
             , binaryBasicBlock :: Maybe BasicBlock
             , binaryLhs :: Value
             , binaryRhs :: Value
             } -> Instruction BinaryOp ConstLike
  RemInst :: { binaryType :: Type
             , binaryName :: !(Maybe Identifier)
             , binaryMetadata :: [Metadata]
             , binaryUniqueId :: UniqueId
             , binaryBasicBlock :: Maybe BasicBlock
             , binaryLhs :: Value
             , binaryRhs :: Value
             } -> Instruction BinaryOp ConstLike
  ShlInst :: { binaryType :: Type
             , binaryName :: !(Maybe Identifier)
             , binaryMetadata :: [Metadata]
             , binaryUniqueId :: UniqueId
             , binaryBasicBlock :: Maybe BasicBlock
             , binaryLhs :: Value
             , binaryRhs :: Value
             } -> Instruction BinaryOp ConstLike
  LshrInst :: { binaryType :: Type
              , binaryName :: !(Maybe Identifier)
              , binaryMetadata :: [Metadata]
              , binaryUniqueId :: UniqueId
              , binaryBasicBlock :: Maybe BasicBlock
              , binaryLhs :: Value
              , binaryRhs :: Value
              } -> Instruction BinaryOp ConstLike
  AshrInst :: { binaryType :: Type
              , binaryName :: !(Maybe Identifier)
              , binaryMetadata :: [Metadata]
              , binaryUniqueId :: UniqueId
              , binaryBasicBlock :: Maybe BasicBlock
              , binaryLhs :: Value
              , binaryRhs :: Value
              } -> Instruction BinaryOp ConstLike
  AndInst :: { binaryType :: Type
             , binaryName :: !(Maybe Identifier)
             , binaryMetadata :: [Metadata]
             , binaryUniqueId :: UniqueId
             , binaryBasicBlock :: Maybe BasicBlock
             , binaryLhs :: Value
             , binaryRhs :: Value
             } -> Instruction BinaryOp ConstLike
  OrInst :: { binaryType :: Type
            , binaryName :: !(Maybe Identifier)
            , binaryMetadata :: [Metadata]
            , binaryUniqueId :: UniqueId
            , binaryBasicBlock :: Maybe BasicBlock
            , binaryLhs :: Value
            , binaryRhs :: Value
            } -> Instruction BinaryOp ConstLike
  XorInst :: { binaryType :: Type
             , binaryName :: !(Maybe Identifier)
             , binaryMetadata :: [Metadata]
             , binaryUniqueId :: UniqueId
             , binaryBasicBlock :: Maybe BasicBlock
             , binaryLhs :: Value
             , binaryRhs :: Value
             } -> Instruction BinaryOp ConstLike
  TruncInst :: { castType :: Type
               , castName :: !(Maybe Identifier)
               , castMetadata :: [Metadata]
               , castUniqueId :: UniqueId
               , castBasicBlock :: Maybe BasicBlock
               , castedValue :: Value
               } -> Instruction ConversionOp ConstLike
  ZExtInst :: { castType :: Type
              , castName :: !(Maybe Identifier)
              , castMetadata :: [Metadata]
              , castUniqueId :: UniqueId
              , castBasicBlock :: Maybe BasicBlock
              , castedValue :: Value
              } -> Instruction ConversionOp ConstLike
  SExtInst :: { castType :: Type
              , castName :: !(Maybe Identifier)
              , castMetadata :: [Metadata]
              , castUniqueId :: UniqueId
              , castBasicBlock :: Maybe BasicBlock
              , castedValue :: Value
              } -> Instruction ConversionOp ConstLike
  FPTruncInst :: { castType :: Type
                 , castName :: !(Maybe Identifier)
                 , castMetadata :: [Metadata]
                 , castUniqueId :: UniqueId
                 , castBasicBlock :: Maybe BasicBlock
                 , castedValue :: Value
                 } -> Instruction ConversionOp ConstLike
  FPExtInst :: { castType :: Type
               , castName :: !(Maybe Identifier)
               , castMetadata :: [Metadata]
               , castUniqueId :: UniqueId
               , castBasicBlock :: Maybe BasicBlock
               , castedValue :: Value
               } -> Instruction ConversionOp ConstLike
  FPToSIInst :: { castType :: Type
                , castName :: !(Maybe Identifier)
                , castMetadata :: [Metadata]
                , castUniqueId :: UniqueId
                , castBasicBlock :: Maybe BasicBlock
                , castedValue :: Value
                } -> Instruction ConversionOp ConstLike
  FPToUIInst :: { castType :: Type
                , castName :: !(Maybe Identifier)
                , castMetadata :: [Metadata]
                , castUniqueId :: UniqueId
                , castBasicBlock :: Maybe BasicBlock
                , castedValue :: Value
                } -> Instruction ConversionOp ConstLike
  SIToFPInst :: { castType :: Type
                , castName :: !(Maybe Identifier)
                , castMetadata :: [Metadata]
                , castUniqueId :: UniqueId
                , castBasicBlock :: Maybe BasicBlock
                , castedValue :: Value
                } -> Instruction ConversionOp ConstLike
  UIToFPInst :: { castType :: Type
                , castName :: !(Maybe Identifier)
                , castMetadata :: [Metadata]
                , castUniqueId :: UniqueId
                , castBasicBlock :: Maybe BasicBlock
                , castedValue :: Value
                } -> Instruction ConversionOp ConstLike
  PtrToIntInst :: { castType :: Type
                  , castName :: !(Maybe Identifier)
                  , castMetadata :: [Metadata]
                  , castUniqueId :: UniqueId
                  , castBasicBlock :: Maybe BasicBlock
                  , castedValue :: Value
                  } -> Instruction ConversionOp ConstLike
  IntToPtrInst :: { castType :: Type
                  , castName :: !(Maybe Identifier)
                  , castMetadata :: [Metadata]
                  , castUniqueId :: UniqueId
                  , castBasicBlock :: Maybe BasicBlock
                  , castedValue :: Value
                  } -> Instruction ConversionOp ConstLike
  BitcastInst :: { castType :: Type
                 , castName :: !(Maybe Identifier)
                 , castMetadata :: [Metadata]
                 , castUniqueId :: UniqueId
                 , castBasicBlock :: Maybe BasicBlock
                 , castedValue :: Value
                 } -> Instruction ConversionOp ConstLike
  ICmpInst :: { cmpType :: Type
              , cmpName :: !(Maybe Identifier)
              , cmpMetadata :: [Metadata]
              , cmpUniqueId :: UniqueId
              , cmpBasicBlock :: Maybe BasicBlock
              , cmpPredicate :: !CmpPredicate
              , cmpV1 :: Value
              , cmpV2 :: Value
              } -> Instruction CompareOp ConstLike
  FCmpInst :: { cmpType :: Type
              , cmpName :: !(Maybe Identifier)
              , cmpMetadata :: [Metadata]
              , cmpUniqueId :: UniqueId
              , cmpBasicBlock :: Maybe BasicBlock
              , cmpPredicate :: !CmpPredicate
              , cmpV1 :: Value
              , cmpV2 :: Value
              } -> Instruction CompareOp ConstLike
  SelectInst :: { selectType :: Type
                , selectName :: !(Maybe Identifier)
                , selectMetadata :: [Metadata]
                , selectUniqueId :: UniqueId
                , selectBasicBlock :: Maybe BasicBlock
                , selectCondition :: Value
                , selectTrueValue :: Value
                , selectFalseValue :: Value
                } -> Instruction OtherOp ConstLike
  CallInst :: { callType :: Type
              , callName :: !(Maybe Identifier)
              , callMetadata :: [Metadata]
              , callUniqueId :: UniqueId
              , callBasicBlock :: Maybe BasicBlock
              , callIsTail :: !Bool
              , callConvention :: !CallingConvention
              , callParamAttrs :: [ParamAttribute]
              , callFunction :: Value
              , callArguments :: [(Value, [ParamAttribute])]
              , callAttrs :: [FunctionAttribute]
              , callHasSRet :: !Bool
              } -> Instruction OtherOp CallLike
  GetElementPtrInst :: { getElementPtrType :: Type
                       , getElementPtrName :: !(Maybe Identifier)
                       , getElementPtrMetadata :: [Metadata]
                       , getElementPtrUniqueId :: UniqueId
                       , getElementPtrBasicBlock :: Maybe BasicBlock
                       , getElementPtrInBounds :: !Bool
                       , getElementPtrValue :: Value
                       , getElementPtrIndices :: [Value]
                       , getElementPtrAddrSpace :: !Int
                       } -> Instruction MemoryOp ConstLike
  InvokeInst :: { invokeType :: Type
                , invokeName :: !(Maybe Identifier)
                , invokeMetadata :: [Metadata]
                , invokeUniqueId :: UniqueId
                , invokeBasicBlock :: Maybe BasicBlock
                , invokeConvention :: !CallingConvention
                , invokeParamAttrs :: [ParamAttribute]
                , invokeFunction :: Value
                , invokeArguments :: [(Value, [ParamAttribute])]
                , invokeAttrs :: [FunctionAttribute]
                , invokeNormalLabel :: BasicBlock
                , invokeUnwindLabel :: BasicBlock
                , invokeHasSRet :: !Bool
                } -> Instruction Terminator CallLike
  VaArgInst :: { vaArgType :: Type
               , vaArgName :: !(Maybe Identifier)
               , vaArgMetadata :: [Metadata]
               , vaArgUniqueId :: UniqueId
               , vaArgBasicBlock :: Maybe BasicBlock
               , vaArgValue :: Value
               } -> Instruction OtherOp OtherTag
  LandingPadInst :: { landingPadType :: Type
                    , landingPadName :: !(Maybe Identifier)
                    , landingPadMetadata :: [Metadata]
                    , landingPadUniqueId :: UniqueId
                    , landingPadBasicBlock :: Maybe BasicBlock
                    , landingPadPersonality :: Value
                    , landingPadIsCleanup :: !Bool
                    , landingPadClauses :: [(Value, LandingPadClause)]
                    } -> Instruction OtherOp OtherTag
  PhiNode :: { phiType :: Type
             , phiName :: !(Maybe Identifier)
             , phiMetadata :: [Metadata]
             , phiUniqueId :: UniqueId
             , phiBasicBlock :: Maybe BasicBlock
             , phiIncomingValues :: [(Value, Value)]
             } -> Instruction Phi OtherTag

instance IsValue (Instruction k t) where -- (Instruction (k :: InstrKind) (t :: InstrTag)) where
  valueType = instructionType
  valueName = instructionName
  valueMetadata = instructionMetadata
  valueContent :: Instruction k t -> Value
  valueContent i = InstructionC (unsafeCoerce i)
  valueUniqueId = instructionUniqueId

instance Eq (Instruction k t) where
  i1 == i2 = instructionUniqueId i1 == instructionUniqueId i2

instance Hashable (Instruction k t) where
  hashWithSalt s = hashWithSalt s . instructionUniqueId

instance Ord (Instruction k t) where
  i1 `compare` i2 = comparing instructionUniqueId i1 i2

data Constant = UndefValue { constantType :: Type
                           , constantUniqueId :: UniqueId
                           }
              | ConstantAggregateZero { constantType :: Type
                                      , constantUniqueId :: UniqueId
                                      }
              | ConstantPointerNull { constantType :: Type
                                    , constantUniqueId :: UniqueId
                                    }
              | BlockAddress { constantType :: Type
                             , constantUniqueId :: UniqueId
                             , blockAddressFunction :: Function
                             , blockAddressBlock :: BasicBlock
                             }
              | ConstantArray { constantType :: Type
                              , constantUniqueId :: UniqueId
                              , constantArrayValues :: [Value]
                              }
              | ConstantFP { constantType :: Type
                           , constantUniqueId :: UniqueId
                           , constantFPValue :: !Double
                           }
              | ConstantInt { constantType :: Type
                            , constantUniqueId :: UniqueId
                            , constantIntValue :: !Integer
                            }
              | ConstantString { constantType :: Type
                               , constantUniqueId :: UniqueId
                               , constantStringValue :: !Text
                               }
              | ConstantStruct { constantType :: Type
                               , constantUniqueId :: UniqueId
                               , constantStructValues :: [Value]
                               }
              | ConstantVector { constantType :: Type
                               , constantUniqueId :: UniqueId
                               , constantVectorValues :: [Value]
                               }
              | ConstantValue { constantType :: Type
                              , constantUniqueId :: UniqueId
                              , constantInstruction :: forall k . Instruction k ConstLike
                                -- ^ Only open instructions are
                                -- allowed; terminators don't make any
                                -- sense.
                              }
              | InlineAsm { constantType :: Type
                          , constantUniqueId :: UniqueId
                          , inlineAsmString :: !Text
                          , inlineAsmConstraints :: !Text
                          }

instance IsValue Constant where
  valueType = constantType
  valueName _ = Nothing
  valueMetadata _ = []
  valueContent = ConstantC
  valueUniqueId = constantUniqueId

instance Eq Constant where
  c1 == c2 = constantUniqueId c1 == constantUniqueId c2

instance Hashable Constant where
  hashWithSalt s = hashWithSalt s . constantUniqueId

instance Ord Constant where
  c1 `compare` c2 = comparing constantUniqueId c1 c2

{-# INLINABLE valueContent' #-}
-- | A version of @valueContent@ that ignores (peeks through)
-- bitcasts.  This is most useful in view patterns.
valueContent' :: IsValue a => a -> Value
valueContent' v =
  case valueContent v of
    InstructionC BitcastInst { castedValue = cv } -> valueContent' cv
    ConstantC ConstantValue { constantInstruction = BitcastInst { castedValue = cv } } -> valueContent' cv
    _ -> valueContent v

{-# INLINABLE stripBitcasts #-}
-- | Strip all wrapper bitcasts from a Value
stripBitcasts :: IsValue a => a -> Value
stripBitcasts v =
  case valueContent v of
    InstructionC BitcastInst { castedValue = cv } -> stripBitcasts cv
    ConstantC ConstantValue { constantInstruction = BitcastInst { castedValue = cv } } -> stripBitcasts cv
    _ -> valueContent v
