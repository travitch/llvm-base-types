{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.LLVM.Types.Referential (
  -- * Basic Types
  Type(..),
  StructTypeNameError(..),
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
  Instruction(..),
  instructionType,
  instructionName,
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
import Data.Word ( Word64 )
import Text.Printf
import Text.Regex.TDFA

import Data.LLVM.Types.Attributes
import Data.LLVM.Types.Dwarf
import Data.LLVM.Types.Identifiers

-- | This is the version of LLVM's debug information that this library
-- supports.
llvmDebugVersion :: Integer
llvmDebugVersion = 524288

-- This isn't very honest, but Values are part of Modules and
-- are fully evaluated before the module is constructed.
instance NFData Instruction where
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
          | TypeStruct (Either Word64 String) [Type] !Bool
            -- ^ Struct types have a list of types in thes struct and
            -- a flag that is True if they are packed.  Named structs
            -- have a (Right stringName) as the name.  Anonymous
            -- structs have a meaningless (but unique within the
            -- Module) integer identifier in the (Left structId).

data StructTypeNameError = NotStructType
                         deriving (Typeable, Show)

-- | Strip off the struct. prefix and any .NNN suffixes added by LLVM
-- to a struct type name.  If the type is not a struct type, return
-- Nothing.
structTypeToName :: (Failure StructTypeNameError m) => Type -> m String
structTypeToName (TypeStruct (Right n) _ _) = return $ structBaseName n
structTypeToName _ = failure NotStructType

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
  compare = compareType

compareType :: Type -> Type -> Ordering
compareType ty1 ty2
  | ty1 == ty2 = EQ
  | otherwise =
    case (ty1, ty2) of
      (TypeStruct (Right n1) _ _, TypeStruct (Right n2) _ _) ->
        compare n1 n2
      (TypeStruct (Left n1) _ _, TypeStruct (Left n2) _ _) ->
        compare n1 n2
      (TypePointer t1 p1, TypePointer t2 p2) -> compare (t1, p1) (t2, p2)
      (TypeArray i1 t1, TypeArray i2 t2) -> compare (i1, t1) (i2, t2)
      (TypeVector i1 t1, TypeVector i2 t2) -> compare (i1, t1) (i2, t2)
      (TypeFunction r1 ts1 v1, TypeFunction r2 ts2 v2) ->
        compare (r1, ts1, v1) (r2, ts2, v2)
      _ -> compare (typeOrdKey ty1) (typeOrdKey ty2)

typeOrdKey :: Type -> Int
typeOrdKey (TypeInteger i) = negate i
typeOrdKey TypeFloat = 1
typeOrdKey TypeDouble = 2
typeOrdKey TypeFP128 = 3
typeOrdKey TypeX86MMX = 4
typeOrdKey TypeX86FP80 = 5
typeOrdKey TypePPCFP128 = 6
typeOrdKey TypeVoid = 7
typeOrdKey TypeLabel = 8
typeOrdKey TypeMetadata = 9
typeOrdKey (TypeArray _ _) = 10
typeOrdKey (TypeVector _ _) = 11
typeOrdKey (TypeFunction _ _ _) = 12
typeOrdKey (TypePointer _ _) = 13
typeOrdKey (TypeStruct (Right _) _ _) = 14
typeOrdKey (TypeStruct (Left _) _ _) = 15

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
  hashWithSalt s (TypeStruct (Right n) _ _) =
    s `hashWithSalt` (16 :: Int) `hashWithSalt` n
  hashWithSalt s (TypeStruct (Left tid) _ p) =
    s `hashWithSalt` (17 :: Int) `hashWithSalt` tid `hashWithSalt` p

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
  TypeStruct (Right n1) _ _ == TypeStruct (Right n2) _ _ = n1 == n2
  TypeStruct (Left tid1) _ _ == TypeStruct (Left tid2) _ _ =
    tid1 == tid2
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
           | InstructionC Instruction
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

instance FromValue Instruction where
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
functionInstructions :: Function -> [Instruction]
functionInstructions = concatMap basicBlockInstructions . functionBody

functionEntryInstruction :: Function -> Instruction
functionEntryInstruction f = e1
  where
    (bb1:_) = functionBody f
    (e1:_) = basicBlockInstructions bb1

-- | Get the ret instruction for a Function
functionExitInstruction :: Function -> Maybe Instruction
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
functionExitInstructions :: Function -> [Instruction]
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

data BasicBlock = BasicBlock { basicBlockName :: !Identifier
                             , basicBlockMetadata :: [Metadata]
                             , basicBlockUniqueId :: UniqueId
                             , basicBlockInstructionVector :: Vector Instruction
                             , basicBlockFunction :: Function
                             }

{-# INLINABLE basicBlockInstructions #-}
basicBlockInstructions :: BasicBlock -> [Instruction]
basicBlockInstructions = V.toList . basicBlockInstructionVector

{-# INLINABLE basicBlockTerminatorInstruction #-}
basicBlockTerminatorInstruction :: BasicBlock -> Instruction
basicBlockTerminatorInstruction = V.last . basicBlockInstructionVector

{-# INLINABLE firstNonPhiInstruction #-}
-- | Get the first instruction in a basic block that is not a Phi
-- node.  This is total because basic blocks cannot be empty and must
-- end in a terminator instruction (Phi nodes are not terminators).
firstNonPhiInstruction :: BasicBlock -> Instruction
firstNonPhiInstruction bb = i
  where
    i : _ = dropWhile instructionIsPhiNode (basicBlockInstructions bb)

{-# INLINABLE instructionIsPhiNode #-}
-- | Predicate to test an instruction to see if it is a phi node
instructionIsPhiNode :: Instruction -> Bool
instructionIsPhiNode v = case v of
  PhiNode {} -> True
  _ -> False

{-# INLINABLE isFirstNonPhiInstruction #-}
-- | Determine if @i@ is the first non-phi instruction in its block.
isFirstNonPhiInstruction :: Instruction -> Bool
isFirstNonPhiInstruction i = i == firstNonPhiInstruction bb
  where
    Just bb = instructionBasicBlock i

{-# INLINABLE basicBlockSplitPhiNodes #-}
-- | Split a block's instructions into phi nodes and the rest
basicBlockSplitPhiNodes :: BasicBlock -> ([Instruction], [Instruction])
basicBlockSplitPhiNodes = span instructionIsPhiNode . basicBlockInstructions

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
-- ends a BasicBlock)
instructionIsTerminator :: Instruction -> Bool
instructionIsTerminator RetInst {} = True
instructionIsTerminator UnconditionalBranchInst {} = True
instructionIsTerminator BranchInst {} = True
instructionIsTerminator SwitchInst {} = True
instructionIsTerminator IndirectBranchInst {} = True
instructionIsTerminator ResumeInst {} = True
instructionIsTerminator UnreachableInst {} = True
instructionIsTerminator InvokeInst {} = True
instructionIsTerminator _ = False

instructionIsEntry :: Instruction -> Bool
instructionIsEntry i = i == ei
  where
    ei = V.unsafeHead $ basicBlockInstructionVector bb
    Just bb = instructionBasicBlock i

instructionFunction :: Instruction -> Maybe Function
instructionFunction i = do
  bb <- instructionBasicBlock i
  return $ basicBlockFunction bb

instructionType :: Instruction -> Type
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
    _ -> _instructionType i

instructionName :: Instruction -> Maybe Identifier
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
    _ -> _instructionName i

data Instruction = RetInst { instructionMetadata :: [Metadata]
                           , instructionUniqueId :: UniqueId
                           , instructionBasicBlock :: Maybe BasicBlock
                           , retInstValue :: Maybe Value
                           }
                 | UnconditionalBranchInst { instructionMetadata :: [Metadata]
                                           , instructionUniqueId :: UniqueId
                                           , instructionBasicBlock :: Maybe BasicBlock
                                           , unconditionalBranchTarget :: BasicBlock
                                           }
                 | BranchInst { instructionMetadata :: [Metadata]
                              , instructionUniqueId :: UniqueId
                              , instructionBasicBlock :: Maybe BasicBlock
                              , branchCondition :: Value
                              , branchTrueTarget :: BasicBlock
                              , branchFalseTarget :: BasicBlock
                              }
                 | SwitchInst { instructionMetadata :: [Metadata]
                              , instructionUniqueId :: UniqueId
                              , instructionBasicBlock :: Maybe BasicBlock
                              , switchValue :: Value
                              , switchDefaultTarget :: BasicBlock
                              , switchCases :: [(Value, BasicBlock)]
                              }
                 | IndirectBranchInst { instructionMetadata :: [Metadata]
                                      , instructionUniqueId :: UniqueId
                                      , instructionBasicBlock :: Maybe BasicBlock
                                      , indirectBranchAddress :: Value
                                      , indirectBranchTargets :: [BasicBlock]
                                      }
                   -- ^ The target must be derived from a blockaddress constant
                   -- The list is a list of possible target destinations
                 | ResumeInst { instructionMetadata :: [Metadata]
                              , instructionUniqueId :: UniqueId
                              , instructionBasicBlock :: Maybe BasicBlock
                              , resumeException :: Value
                              }
                 | UnreachableInst { instructionMetadata :: [Metadata]
                                   , instructionUniqueId :: UniqueId
                                   , instructionBasicBlock :: Maybe BasicBlock
                                   }
                 | ExtractElementInst { _instructionType :: Type
                                      , _instructionName :: !(Maybe Identifier)
                                      , instructionMetadata :: [Metadata]
                                      , instructionUniqueId :: UniqueId
                                      , instructionBasicBlock :: Maybe BasicBlock
                                      , extractElementVector :: Value
                                      , extractElementIndex :: Value
                                      }
                 | InsertElementInst { _instructionType :: Type
                                     , _instructionName :: !(Maybe Identifier)
                                     , instructionMetadata :: [Metadata]
                                     , instructionUniqueId :: UniqueId
                                     , instructionBasicBlock :: Maybe BasicBlock
                                     , insertElementVector :: Value
                                     , insertElementValue :: Value
                                     , insertElementIndex :: Value
                                     }
                 | ShuffleVectorInst { _instructionType :: Type
                                     , _instructionName :: !(Maybe Identifier)
                                     , instructionMetadata :: [Metadata]
                                     , instructionUniqueId :: UniqueId
                                     , instructionBasicBlock :: Maybe BasicBlock
                                     , shuffleVectorV1 :: Value
                                     , shuffleVectorV2 :: Value
                                     , shuffleVectorMask :: Value
                                     }
                 | ExtractValueInst { _instructionType :: Type
                                    , _instructionName :: !(Maybe Identifier)
                                    , instructionMetadata :: [Metadata]
                                    , instructionUniqueId :: UniqueId
                                    , instructionBasicBlock :: Maybe BasicBlock
                                    , extractValueAggregate :: Value
                                    , extractValueIndices :: [Int]
                                    }
                 | InsertValueInst { _instructionType :: Type
                                   , _instructionName :: !(Maybe Identifier)
                                   , instructionMetadata :: [Metadata]
                                   , instructionUniqueId :: UniqueId
                                   , instructionBasicBlock :: Maybe BasicBlock
                                   , insertValueAggregate :: Value
                                   , insertValueValue :: Value
                                   , insertValueIndices :: [Int]
                                   }
                 | AllocaInst { _instructionType :: Type
                              , _instructionName :: !(Maybe Identifier)
                              , instructionMetadata :: [Metadata]
                              , instructionUniqueId :: UniqueId
                              , instructionBasicBlock :: Maybe BasicBlock
                              , allocaNumElements :: Value
                              , allocaAlign :: !Int64
                              }
                 | LoadInst { _instructionType :: Type
                            , _instructionName :: !(Maybe Identifier)
                            , instructionMetadata :: [Metadata]
                            , instructionUniqueId :: UniqueId
                            , instructionBasicBlock :: Maybe BasicBlock
                            , loadIsVolatile :: !Bool
                            , loadAddress :: Value
                            , loadAlignment :: !Int64
                            }
                 | StoreInst { instructionMetadata :: [Metadata]
                             , instructionUniqueId :: UniqueId
                             , instructionBasicBlock :: Maybe BasicBlock
                             , storeIsVolatile :: !Bool
                             , storeValue :: Value
                             , storeAddress :: Value
                             , storeAlignment :: !Int64
                             , storeAddressSpace :: !Int
                             }
                 | FenceInst { instructionMetadata :: [Metadata]
                             , instructionUniqueId :: UniqueId
                             , instructionBasicBlock :: Maybe BasicBlock
                             , fenceOrdering :: !AtomicOrdering
                             , fenceScope :: !SynchronizationScope
                             }
                 | AtomicCmpXchgInst { instructionMetadata :: [Metadata]
                                     , instructionUniqueId :: UniqueId
                                     , instructionBasicBlock :: Maybe BasicBlock
                                     , atomicCmpXchgOrdering :: !AtomicOrdering
                                     , atomicCmpXchgScope :: !SynchronizationScope
                                     , atomicCmpXchgIsVolatile :: !Bool
                                     , atomicCmpXchgAddressSpace :: !Int
                                     , atomicCmpXchgPointer :: Value
                                     , atomicCmpXchgComparison :: Value
                                     , atomicCmpXchgNewValue :: Value
                                     }
                 | AtomicRMWInst { instructionMetadata :: [Metadata]
                                 , instructionUniqueId :: UniqueId
                                 , instructionBasicBlock :: Maybe BasicBlock
                                 , atomicRMWOrdering :: !AtomicOrdering
                                 , atomicRMWScope :: !SynchronizationScope
                                 , atomicRMWOperation :: !AtomicOperation
                                 , atomicRMWIsVolatile :: !Bool
                                 , atomicRMWPointer :: Value
                                 , atomicRMWValue :: Value
                                 , atomicRMWAddressSpace :: !Int
                                 }
                 | AddInst { _instructionType :: Type
                           , _instructionName :: !(Maybe Identifier)
                           , instructionMetadata :: [Metadata]
                           , instructionUniqueId :: UniqueId
                           , instructionBasicBlock :: Maybe BasicBlock
                           , binaryArithFlags :: !ArithFlags
                           , binaryLhs :: Value
                           , binaryRhs :: Value
                           }
                 | SubInst { _instructionType :: Type
                           , _instructionName :: !(Maybe Identifier)
                           , instructionMetadata :: [Metadata]
                           , instructionUniqueId :: UniqueId
                           , instructionBasicBlock :: Maybe BasicBlock
                           , binaryArithFlags :: !ArithFlags
                           , binaryLhs :: Value
                           , binaryRhs :: Value
                           }
                 | MulInst { _instructionType :: Type
                           , _instructionName :: !(Maybe Identifier)
                           , instructionMetadata :: [Metadata]
                           , instructionUniqueId :: UniqueId
                           , instructionBasicBlock :: Maybe BasicBlock
                           , binaryArithFlags :: !ArithFlags
                           , binaryLhs :: Value
                           , binaryRhs :: Value
                           }
                 | DivInst { _instructionType :: Type
                           , _instructionName :: !(Maybe Identifier)
                           , instructionMetadata :: [Metadata]
                           , instructionUniqueId :: UniqueId
                           , instructionBasicBlock :: Maybe BasicBlock
                           , binaryLhs :: Value
                           , binaryRhs :: Value
                           }
                 | RemInst { _instructionType :: Type
                           , _instructionName :: !(Maybe Identifier)
                           , instructionMetadata :: [Metadata]
                           , instructionUniqueId :: UniqueId
                           , instructionBasicBlock :: Maybe BasicBlock
                           , binaryLhs :: Value
                           , binaryRhs :: Value
                           }
                 | ShlInst { _instructionType :: Type
                           , _instructionName :: !(Maybe Identifier)
                           , instructionMetadata :: [Metadata]
                           , instructionUniqueId :: UniqueId
                           , instructionBasicBlock :: Maybe BasicBlock
                           , binaryLhs :: Value
                           , binaryRhs :: Value
                           }
                 | LshrInst { _instructionType :: Type
                            , _instructionName :: !(Maybe Identifier)
                            , instructionMetadata :: [Metadata]
                            , instructionUniqueId :: UniqueId
                            , instructionBasicBlock :: Maybe BasicBlock
                            , binaryLhs :: Value
                            , binaryRhs :: Value
                            }
                 | AshrInst { _instructionType :: Type
                            , _instructionName :: !(Maybe Identifier)
                            , instructionMetadata :: [Metadata]
                            , instructionUniqueId :: UniqueId
                            , instructionBasicBlock :: Maybe BasicBlock
                            , binaryLhs :: Value
                            , binaryRhs :: Value
                           }
                 | AndInst { _instructionType :: Type
                           , _instructionName :: !(Maybe Identifier)
                           , instructionMetadata :: [Metadata]
                           , instructionUniqueId :: UniqueId
                           , instructionBasicBlock :: Maybe BasicBlock
                           , binaryLhs :: Value
                           , binaryRhs :: Value
                           }
                 | OrInst { _instructionType :: Type
                          , _instructionName :: !(Maybe Identifier)
                          , instructionMetadata :: [Metadata]
                          , instructionUniqueId :: UniqueId
                          , instructionBasicBlock :: Maybe BasicBlock
                          , binaryLhs :: Value
                          , binaryRhs :: Value
                          }
                 | XorInst { _instructionType :: Type
                           , _instructionName :: !(Maybe Identifier)
                           , instructionMetadata :: [Metadata]
                           , instructionUniqueId :: UniqueId
                           , instructionBasicBlock :: Maybe BasicBlock
                           , binaryLhs :: Value
                           , binaryRhs :: Value
                           }
                 | TruncInst { _instructionType :: Type
                             , _instructionName :: !(Maybe Identifier)
                             , instructionMetadata :: [Metadata]
                             , instructionUniqueId :: UniqueId
                             , instructionBasicBlock :: Maybe BasicBlock
                             , castedValue :: Value
                             }
                 | ZExtInst { _instructionType :: Type
                            , _instructionName :: !(Maybe Identifier)
                            , instructionMetadata :: [Metadata]
                            , instructionUniqueId :: UniqueId
                            , instructionBasicBlock :: Maybe BasicBlock
                            , castedValue :: Value
                            }
                 | SExtInst { _instructionType :: Type
                            , _instructionName :: !(Maybe Identifier)
                            , instructionMetadata :: [Metadata]
                            , instructionUniqueId :: UniqueId
                            , instructionBasicBlock :: Maybe BasicBlock
                            , castedValue :: Value
                            }
                 | FPTruncInst { _instructionType :: Type
                               , _instructionName :: !(Maybe Identifier)
                               , instructionMetadata :: [Metadata]
                               , instructionUniqueId :: UniqueId
                               , instructionBasicBlock :: Maybe BasicBlock
                               , castedValue :: Value
                               }
                 | FPExtInst { _instructionType :: Type
                             , _instructionName :: !(Maybe Identifier)
                             , instructionMetadata :: [Metadata]
                             , instructionUniqueId :: UniqueId
                             , instructionBasicBlock :: Maybe BasicBlock
                             , castedValue :: Value
                             }
                 | FPToSIInst { _instructionType :: Type
                              , _instructionName :: !(Maybe Identifier)
                              , instructionMetadata :: [Metadata]
                              , instructionUniqueId :: UniqueId
                              , instructionBasicBlock :: Maybe BasicBlock
                              , castedValue :: Value
                              }
                 | FPToUIInst { _instructionType :: Type
                              , _instructionName :: !(Maybe Identifier)
                              , instructionMetadata :: [Metadata]
                              , instructionUniqueId :: UniqueId
                              , instructionBasicBlock :: Maybe BasicBlock
                              , castedValue :: Value
                              }
                 | SIToFPInst { _instructionType :: Type
                              , _instructionName :: !(Maybe Identifier)
                              , instructionMetadata :: [Metadata]
                              , instructionUniqueId :: UniqueId
                              , instructionBasicBlock :: Maybe BasicBlock
                              , castedValue :: Value
                              }
                 | UIToFPInst { _instructionType :: Type
                              , _instructionName :: !(Maybe Identifier)
                              , instructionMetadata :: [Metadata]
                              , instructionUniqueId :: UniqueId
                              , instructionBasicBlock :: Maybe BasicBlock
                              , castedValue :: Value
                              }
                 | PtrToIntInst { _instructionType :: Type
                                , _instructionName :: !(Maybe Identifier)
                                , instructionMetadata :: [Metadata]
                                , instructionUniqueId :: UniqueId
                                , instructionBasicBlock :: Maybe BasicBlock
                                , castedValue :: Value
                                }
                 | IntToPtrInst { _instructionType :: Type
                                , _instructionName :: !(Maybe Identifier)
                                , instructionMetadata :: [Metadata]
                                , instructionUniqueId :: UniqueId
                                , instructionBasicBlock :: Maybe BasicBlock
                                , castedValue :: Value
                                }
                 | BitcastInst { _instructionType :: Type
                               , _instructionName :: !(Maybe Identifier)
                               , instructionMetadata :: [Metadata]
                               , instructionUniqueId :: UniqueId
                               , instructionBasicBlock :: Maybe BasicBlock
                               , castedValue :: Value
                               }
                 | ICmpInst { _instructionType :: Type
                            , _instructionName :: !(Maybe Identifier)
                            , instructionMetadata :: [Metadata]
                            , instructionUniqueId :: UniqueId
                            , instructionBasicBlock :: Maybe BasicBlock
                            , cmpPredicate :: !CmpPredicate
                            , cmpV1 :: Value
                            , cmpV2 :: Value
                            }
                 | FCmpInst { _instructionType :: Type
                            , _instructionName :: !(Maybe Identifier)
                            , instructionMetadata :: [Metadata]
                            , instructionUniqueId :: UniqueId
                            , instructionBasicBlock :: Maybe BasicBlock
                            , cmpPredicate :: !CmpPredicate
                            , cmpV1 :: Value
                            , cmpV2 :: Value
                            }
                 | SelectInst { _instructionType :: Type
                              , _instructionName :: !(Maybe Identifier)
                              , instructionMetadata :: [Metadata]
                              , instructionUniqueId :: UniqueId
                              , instructionBasicBlock :: Maybe BasicBlock
                              , selectCondition :: Value
                              , selectTrueValue :: Value
                              , selectFalseValue :: Value
                              }
                 | CallInst { _instructionType :: Type
                            , _instructionName :: !(Maybe Identifier)
                            , instructionMetadata :: [Metadata]
                            , instructionUniqueId :: UniqueId
                            , instructionBasicBlock :: Maybe BasicBlock
                            , callIsTail :: !Bool
                            , callConvention :: !CallingConvention
                            , callParamAttrs :: [ParamAttribute]
                            , callFunction :: Value
                            , callArguments :: [(Value, [ParamAttribute])]
                            , callAttrs :: [FunctionAttribute]
                            , callHasSRet :: !Bool
                            }
                 | GetElementPtrInst { _instructionType :: Type
                                     , _instructionName :: !(Maybe Identifier)
                                     , instructionMetadata :: [Metadata]
                                     , instructionUniqueId :: UniqueId
                                     , instructionBasicBlock :: Maybe BasicBlock
                                     , getElementPtrInBounds :: !Bool
                                     , getElementPtrValue :: Value
                                     , getElementPtrIndices :: [Value]
                                     , getElementPtrAddrSpace :: !Int
                                     }
                 | InvokeInst { _instructionType :: Type
                              , _instructionName :: !(Maybe Identifier)
                              , instructionMetadata :: [Metadata]
                              , instructionUniqueId :: UniqueId
                              , instructionBasicBlock :: Maybe BasicBlock
                              , invokeConvention :: !CallingConvention
                              , invokeParamAttrs :: [ParamAttribute]
                              , invokeFunction :: Value
                              , invokeArguments :: [(Value, [ParamAttribute])]
                              , invokeAttrs :: [FunctionAttribute]
                              , invokeNormalLabel :: BasicBlock
                              , invokeUnwindLabel :: BasicBlock
                              , invokeHasSRet :: !Bool
                              }
                 | VaArgInst { _instructionType :: Type
                             , _instructionName :: !(Maybe Identifier)
                             , instructionMetadata :: [Metadata]
                             , instructionUniqueId :: UniqueId
                             , instructionBasicBlock :: Maybe BasicBlock
                             , vaArgValue :: Value
                             }
                 | LandingPadInst { _instructionType :: Type
                                  , _instructionName :: !(Maybe Identifier)
                                  , instructionMetadata :: [Metadata]
                                  , instructionUniqueId :: UniqueId
                                  , instructionBasicBlock :: Maybe BasicBlock
                                  , landingPadPersonality :: Value
                                  , landingPadIsCleanup :: !Bool
                                  , landingPadClauses :: [(Value, LandingPadClause)]
                                  }
                 | PhiNode { _instructionType :: Type
                           , _instructionName :: !(Maybe Identifier)
                           , instructionMetadata :: [Metadata]
                           , instructionUniqueId :: UniqueId
                           , instructionBasicBlock :: Maybe BasicBlock
                           , phiIncomingValues :: [(Value, Value)]
                           }
instance IsValue Instruction where
  valueType = instructionType
  valueName = instructionName
  valueMetadata = instructionMetadata
  valueContent = InstructionC
  valueUniqueId = instructionUniqueId

instance Eq Instruction where
  i1 == i2 = instructionUniqueId i1 == instructionUniqueId i2

instance Hashable Instruction where
  hashWithSalt s = hashWithSalt s . instructionUniqueId

instance Ord Instruction where
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
                              , constantInstruction :: Instruction
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
