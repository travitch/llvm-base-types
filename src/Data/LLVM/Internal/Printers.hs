{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Define the unparser for our LLVM IR
module Data.LLVM.Internal.Printers (
  printMetadata,
  printAsm,
  printType,
  printValue
  ) where

import Data.GraphViz
import Data.Int
import Data.List ( intercalate )
import Data.Monoid
import Data.ByteString.Char8 ( ByteString, unpack )

import Data.LLVM.Types.Attributes
import Data.LLVM.Types.Identifiers
import Data.LLVM.Types.Referential

-- TODO List
--
-- * Pretty up the DataLayout
-- * Print out named type definitions
-- * Make the function type printing as flexible as the official
--   version

showUntypedMDName :: Metadata -> String
showUntypedMDName = ("!"++) . show . metaValueUniqueId

showMDName :: Metadata -> String
showMDName = ("metadata !"++) . show . metaValueUniqueId

showMDString :: ByteString -> String
showMDString bs = "metadata !" ++ show bs

showBool :: Bool -> String
showBool True = "i1 true"
showBool False = "i1 false"

maybeShowMDName :: Maybe Metadata -> String
maybeShowMDName Nothing = "null"
maybeShowMDName (Just m) = showMDName m

dbgTag :: Int -> String
dbgTag i = show (i + fromIntegral llvmDebugVersion)

printMetadata :: Metadata -> String
printMetadata md@MetaSourceLocation { } =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", show (metaSourceRow md)
          , ", i32 ", show (metaSourceCol md)
          , ", ", maybeShowMDName (metaSourceScope md)
          , " null}"
          ]
printMetadata md@MetaDWLexicalBlock { } =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", dbgTag 11
          , ", i32 ", show (metaLexicalBlockRow md)
          , ", i32 ", show (metaLexicalBlockCol md)
          , ", ", maybeShowMDName (metaLexicalBlockContext md)
          , "}"
          ]
printMetadata md@MetaDWCompileUnit {} =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", dbgTag 17
          , ", i32 ", show (metaCompileUnitLanguage md)
          , ", ", showMDString (metaCompileUnitSourceFile md)
          , ", ", showMDString (metaCompileUnitCompileDir md)
          , ", ", showMDString (metaCompileUnitProducer md)
          , ", ", showBool (metaCompileUnitIsMain md)
          , ", ", showBool (metaCompileUnitIsOpt md)
          , ", i32 ", show (metaCompileUnitVersion md)
          , "}"
          ]
printMetadata md@MetaDWFile {} =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", dbgTag 41
          , ", ", showMDString (metaFileSourceFile md)
          , ", ", showMDString (metaFileSourceDir md)
          , "}"
          ]
printMetadata md@MetaDWVariable {} =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", dbgTag 52
          , ", ", maybeShowMDName (metaGlobalVarContext md)
          , ", ", showMDString (metaGlobalVarName md)
          , ", ", showMDString (metaGlobalVarDisplayName md)
          , ", ", showMDString (metaGlobalVarLinkageName md)
          , ", i32 ", show (metaGlobalVarLine md)
          , ", ", maybeShowMDName (metaGlobalVarType md)
          , ", ", showBool (metaGlobalVarStatic md)
          , ", ", showBool (metaGlobalVarNotExtern md)
          , "}"
          ]
printMetadata md@MetaDWSubprogram {} =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", dbgTag 46
          , ", ", maybeShowMDName (metaSubprogramContext md)
          , ", ", showMDString (metaSubprogramName md)
          , ", ", showMDString (metaSubprogramDisplayName md)
          , ", ", showMDString (metaSubprogramLinkageName md)
          , ", i32 ", show (metaSubprogramLine md)
          , ", ", maybeShowMDName (metaSubprogramType md)
          , ", ", showBool (metaSubprogramStatic md)
          , ", ", showBool (metaSubprogramNotExtern md)
          , ", i32 ", show (metaSubprogramVirtuality md)
          , ", i32 ", show (metaSubprogramVirtIndex md)
          , ", ", maybeShowMDName (metaSubprogramBaseType md)
          , ", ", showBool (metaSubprogramArtificial md)
          , ", ", showBool (metaSubprogramOptimized md)
          , "}"
          ]
printMetadata md@MetaDWBaseType {} =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", dbgTag 36
          , ", ", maybeShowMDName (metaBaseTypeContext md)
          , ", ", showMDString (metaBaseTypeName md)
          , ", ", maybeShowMDName (metaBaseTypeFile md)
          , ", i32 ", show (metaBaseTypeLine md)
          , ", i32 ", show (metaBaseTypeSize md)
          , ", i32 ", show (metaBaseTypeAlign md)
          , ", i64 ", show (metaBaseTypeOffset md)
          , ", i32 ", show (metaBaseTypeFlags md)
          , ", i32 ", show (metaBaseTypeEncoding md)
          , "}"
          ]
printMetadata md@MetaDWDerivedType {} =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", show (metaDerivedTypeTag md)
          , ", ", maybeShowMDName (metaDerivedTypeContext md)
          , ", ", showMDString (metaDerivedTypeName md)
          , ", ", maybeShowMDName (metaDerivedTypeFile md)
          , ", i32 ", show (metaDerivedTypeLine md)
          , ", i32 ", show (metaDerivedTypeSize md)
          , ", i32 ", show (metaDerivedTypeAlign md)
          , ", i64 ", show (metaDerivedTypeOffset md)
          , ", ", maybeShowMDName (metaDerivedTypeParent md)
          , "}"
          ]
printMetadata md@MetaDWCompositeType {} =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", show (metaCompositeTypeTag md)
          , ", ", maybeShowMDName (metaCompositeTypeContext md)
          , ", ", showMDString (metaCompositeTypeName md)
          , ", ", maybeShowMDName (metaCompositeTypeFile md)
          , ", i32 ", show (metaCompositeTypeLine md)
          , ", i32 ", show (metaCompositeTypeSize md)
          , ", i32 ", show (metaCompositeTypeAlign md)
          , ", i64 ", show (metaCompositeTypeOffset md)
          , ", i32 ", show (metaCompositeTypeFlags md)
          , ", ", maybeShowMDName (metaCompositeTypeParent md)
          , ", ", maybeShowMDName (metaCompositeTypeMembers md)
          , ", i32 ", show (metaCompositeTypeRuntime md)
          , "}"
          ]
printMetadata md@MetaDWSubrange {} =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", dbgTag 33
          , ", i32 ", show (metaSubrangeLow md)
          , ", i32 ", show (metaSubrangeHigh md)
          , "}"
          ]
printMetadata md@MetaDWEnumerator {} =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", dbgTag 40
          , ", ", showMDString (metaEnumeratorName md)
          , ", i32 ", show (metaEnumeratorValue md)
          , "}"
          ]
printMetadata md@MetaDWLocal {} =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", show (metaLocalTag md)
          , ", ", maybeShowMDName (metaLocalContext md)
          , ", ", showMDString (metaLocalName md)
          , ", i32 ", show (metaLocalLine md)
          , ", ", maybeShowMDName (metaLocalType md)
          , "}"
          ]
printMetadata md@(MetadataList _ vals) =
  mconcat [ showUntypedMDName md, " = metadata !{"
          , intercalate ", " (map maybeShowMDName vals)
          , "}"
          ]
printMetadata md@MetaDWNamespace {} =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", dbgTag 57
          , ", ", showMDString (metaNamespaceName md)
          , ", ", maybeShowMDName (metaNamespaceContext md)
          , ", i32 ", show (metaNamespaceLine md)
          , "}"
          ]
printMetadata md@MetaDWTemplateTypeParameter {} =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", dbgTag 0x2f
          , ", ", showMDString (metaTemplateTypeParameterName md)
          , ", i32 ", show (metaTemplateTypeParameterLine md)
          , ", i32 ", show (metaTemplateTypeParameterCol md)
          , ", ", maybeShowMDName (metaTemplateTypeParameterContext md)
          , ", ", maybeShowMDName (metaTemplateTypeParameterType md)
          , "}"
          ]
printMetadata md@MetaDWTemplateValueParameter {} =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", dbgTag 0x30
          , ", ", showMDString (metaTemplateValueParameterName md)
          , ", i32 ", show (metaTemplateValueParameterLine md)
          , ", i32 ", show (metaTemplateValueParameterCol md)
          , ", ", maybeShowMDName (metaTemplateValueParameterContext md)
          , ", ", maybeShowMDName (metaTemplateValueParameterType md)
          , ", i64 ", show (metaTemplateValueParameterValue md)
          , "}"
          ]
printMetadata md@(MetadataUnknown _ s) =
  mconcat [ showUntypedMDName md, " = metadata ", unpack s ]

-- Take all of the asm chunks, break their contents into lines,
-- then wrap each of those lines in the 'module asm' wrapper.
-- Combine them into a single string with newlines.
printAsm :: Assembly -> String
printAsm asm = mconcat asmLines
  where
    asmLines = map adorn (lines (show asm))
    adorn s = "module asm \"" ++ s ++ "\"\n"

-- When referencing a non-constant value during printing, just use
-- this instead of printValue to avoid problems printing cyclic data.
-- If the value doesn't have a name, just print it (it should be a
-- constant).
printConstOrName :: Value -> String
printConstOrName v = case valueName v of
  Nothing -> mconcat [ printType (valueType v), " ", printValue v ]
  Just ident -> mconcat [ printType (valueType v), " ", show ident ]

printConstOrNameNoType :: Value -> String
printConstOrNameNoType v = case valueName v of
  Nothing -> printValue v
  Just ident -> show ident

compose :: [String] -> String
compose = unwords . filter (not . null)

quote :: String -> String
quote s = mconcat [ "\"", s, "\"" ]

isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _ -> False

printValue :: Value -> String
printValue v = case valueContent v of
  FunctionC f ->
    let retAttrS = unwords $ map show (functionRetAttrs f)
        argS = intercalate ", " $ map (printValue . Value) (functionParameters f)
        fAttrS = unwords $ map show (functionAttrs f)
        bodyS = unlines $ map (printValue . Value) (functionBody f)
        vaTag = if functionIsVararg f then ", ..." else ""
        (TypeFunction rtype _ _) = functionType f
        name = functionName f
    in compose [ "define", show (functionLinkage f), show (functionVisibility f)
               , show (functionCC f), retAttrS, printType rtype, show name, "("
               , argS, vaTag, ")", fAttrS, maybe "" unpack (functionSection f)
               , printAlignment (functionAlign f), maybe "" show (functionGCName f)
               , "{\n", bodyS, "}"
               ]
  ArgumentC a ->
    compose [ printType (argumentType a)
            , unwords $ map show (argumentParamAttrs a)
            , show (argumentName a)
            ]
  BasicBlockC b ->
    let indent = ("  "++)
        dbgS = map (printDebugTag . valueMetadata) (basicBlockInstructions b)
        instS = map (printValue . Value) (basicBlockInstructions b)
        instS' = zipWith (++) instS dbgS
        instS'' = unlines $ map indent instS'
        identS = identifierAsString (basicBlockName b)
        label = case isInteger identS of
          True -> "; <label>:" ++ identS
          False -> identS ++ ":"
    in mconcat [ label, "\n", instS'' ]
  GlobalVariableC g ->
    let TypePointer _ addrSpace = globalVariableType g
        addrSpaceS = case addrSpace of
          0 -> ""
          _ -> mconcat [ "addrspace(", show addrSpace, ")" ]
        annotsS = if globalVariableIsConstant g then "constant" else ""
        initS = maybe "" printConstOrName (globalVariableInitializer g)
        sectionS = maybe "" ((", section"++) . quote . unpack) (globalVariableSection g)
    in compose [ show (globalVariableName g), "=", addrSpaceS
               , show (globalVariableLinkage g), show (globalVariableVisibility g)
               , annotsS, initS, sectionS, printAlignment (globalVariableAlignment g)
               ]
  GlobalAliasC a ->
    compose [ show (globalAliasName a), "= alias", show (globalAliasLinkage a)
            , show (globalAliasVisibility a), printConstOrName (globalAliasTarget a)
            ]
  ExternalValueC e ->
    compose [ "declare", printType (valueType e), show (externalValueName e) ]
  ExternalFunctionC e ->
    let TypeFunction rtype argTypes isva = externalFunctionType e
    in compose [ "declare", printType rtype, show (externalFunctionName e)
               , "(", intercalate ", " $ map printType argTypes
               , if isva then ", ..." else "", ")"
               ]
  InstructionC i ->
    case i of
      RetInst { retInstValue = Just rv } -> compose [ "ret", printConstOrName rv ]
      RetInst { } -> "ret void"
      ResumeInst { resumeException = val } ->
        compose [ "resume", printConstOrName val ]
      UnconditionalBranchInst { unconditionalBranchTarget = dest } ->
        compose [ "br", (printConstOrName . Value) dest ]
      BranchInst { branchCondition = cond
                 , branchTrueTarget = tTarget
                 , branchFalseTarget = fTarget
                 } ->
        compose [ "br", printConstOrName cond
                , ",", printConstOrName (Value tTarget)
                , ",", printConstOrName (Value fTarget)
                ]
      SwitchInst { switchValue = val
                 , switchDefaultTarget = defTarget
                 , switchCases = cases
                 } ->
        let caseDests = unwords $ map printPair cases
            printPair (caseVal, caseDest) =
              mconcat [ printConstOrName caseVal, ", ", printConstOrName (Value caseDest) ]
        in compose [ "switch", printConstOrName val, ",", printConstOrName (Value defTarget)
                   , "[", caseDests, "]"
                   ]
      IndirectBranchInst { indirectBranchAddress = addr
                         , indirectBranchTargets = targets
                         } ->
        compose [ "indirectbr", printConstOrName addr
                , "[", intercalate ", " $ map (printConstOrName . Value) targets, "]"
                ]
      UnreachableInst { } -> "unreachable"
      AddInst { } -> printFlaggedBinaryOp "add" i
      SubInst { } -> printFlaggedBinaryOp "sub" i
      MulInst { } -> printFlaggedBinaryOp "mul" i
      DivInst { } -> printBinaryOp "div" i
      RemInst { } -> printBinaryOp "rem" i
      ShlInst { } -> printBinaryOp "shl" i
      LshrInst { } -> printBinaryOp "lshr" i
      AshrInst { } -> printBinaryOp "ashr" i
      AndInst { } -> printBinaryOp "and" i
      OrInst { } -> printBinaryOp "or" i
      XorInst { } -> printBinaryOp "xor" i
      ExtractElementInst { extractElementVector = vec
                         , extractElementIndex = idx
                         } ->
        compose [ printInstNamePrefix i
                , "extractelement"
                , printConstOrName vec, ","
                , printConstOrName idx
                ]
      InsertElementInst { insertElementVector = vec
                        , insertElementValue = val
                        , insertElementIndex = idx
                        } ->
        compose [ printInstNamePrefix i
                , "insertelement"
                , printConstOrName vec, ","
                , printConstOrName val, ","
                , printConstOrName idx
                ]
      ShuffleVectorInst { shuffleVectorV1 = v1
                        , shuffleVectorV2 = v2
                        , shuffleVectorMask = mask
                        } ->
        compose [ printInstNamePrefix i
                , "shufflevector"
                , printConstOrName v1, ","
                , printConstOrName v2, ","
                , printConstOrName mask
                ]
      ExtractValueInst { extractValueAggregate = agg
                       , extractValueIndices = indices
                       } ->
        compose [ printInstNamePrefix i
                , "extractvalue"
                , printConstOrName agg
                , intercalate ", " $ map show indices
                ]
      InsertValueInst { insertValueAggregate = agg
                      , insertValueValue = val
                      , insertValueIndices = indices
                      } ->
        compose [ printInstNamePrefix i
                , "insertvalue"
                , printConstOrName agg, ","
                , printConstOrName val, ","
                , intercalate ", " $ map show indices
                ]
      AllocaInst { allocaNumElements = elems
                 , allocaAlign = align
                 } ->
        let count = case valueContent elems of
              ConstantC ConstantInt { constantIntValue = 1 } -> ""
              _ -> ", " ++ printConstOrName elems
            TypePointer ty _ = instructionType i
        in   compose [ printInstNamePrefix i
                     , "alloca"
                     , printType ty
                     , count
                     , printAlignment align
                     ]
      LoadInst { loadIsVolatile = volatile
               , loadAddress = src
               , loadAlignment = align
               } ->
        compose [ printInstNamePrefix i
                , printVolatileFlag volatile
                , "load"
                , printConstOrName src
                , printAlignment align
                ]
      StoreInst { storeIsVolatile = volatile
                , storeValue = val
                , storeAddress = dest
                , storeAlignment = align
                } ->
        compose [ printVolatileFlag volatile
                , "store"
                , printConstOrName val, ","
                , printConstOrName dest
                , printAlignment align
                ]
      FenceInst { fenceOrdering = o, fenceScope = s } ->
        compose [ "fence", show s, show o ]
      AtomicCmpXchgInst { atomicCmpXchgOrdering = o
                        , atomicCmpXchgScope = s
                        , atomicCmpXchgIsVolatile = isVol
                        , atomicCmpXchgAddressSpace = _ -- ?
                        , atomicCmpXchgPointer = ptr
                        , atomicCmpXchgComparison = cmp
                        , atomicCmpXchgNewValue = newV
                        } ->
        compose [ "cmpxchg", printVolatileFlag isVol
                , printConstOrName ptr, ","
                , printConstOrName cmp, ","
                , printConstOrName newV
                , show s, show o
                ]
      AtomicRMWInst { atomicRMWOrdering = o
                    , atomicRMWScope = s
                    , atomicRMWOperation = op
                    , atomicRMWIsVolatile = isVol
                    , atomicRMWPointer = p
                    , atomicRMWValue = val
                    , atomicRMWAddressSpace = _ -- ?
                    } ->
        compose [ "atomicrmw", printVolatileFlag isVol
                , show op
                , printConstOrName p, ","
                , printConstOrName val
                , show s
                , show o
                ]
      TruncInst { } -> printTypecast "trunc" i
      ZExtInst { } -> printTypecast "zext" i
      SExtInst { } -> printTypecast "sext" i
      FPTruncInst { } -> printTypecast "fptrunc" i
      FPExtInst { } -> printTypecast "fpext" i
      FPToUIInst { } -> printTypecast "fptoui" i
      FPToSIInst { } -> printTypecast "fptosi" i
      UIToFPInst { } -> printTypecast "uitofp" i
      SIToFPInst { } -> printTypecast "sitofp" i
      PtrToIntInst { } -> printTypecast "ptrtoint" i
      IntToPtrInst { } -> printTypecast "inttoptr" i
      BitcastInst { } -> printTypecast "bitcast" i
      ICmpInst { cmpPredicate = cond
               , cmpV1 = v1
               , cmpV2 = v2
               } ->
        compose [ printInstNamePrefix i
                , "icmp"
                , show cond
                , printConstOrName v1, ","
                , printConstOrNameNoType v2
                ]
      FCmpInst { cmpPredicate = cond
               , cmpV1 = v1
               , cmpV2 = v2
               } ->
        compose [ printInstNamePrefix i
                , "fcmp"
                , show cond
                , printConstOrName v1, ","
                , printConstOrNameNoType v2
                ]
      PhiNode { phiIncomingValues = vals
              } ->
        let printPair (val, lab) =
              mconcat [ "[", printConstOrNameNoType val
                      , ", ", printConstOrNameNoType lab, "]"
                      ]
            valS = intercalate ", " $ map printPair vals
        in compose [ printInstNamePrefix i
                   , "phi"
                   , printType (instructionType i)
                   , "[", valS, "]"
                   ]
      SelectInst { selectCondition = cond
                 , selectTrueValue = v1
                 , selectFalseValue = v2
                 } ->
        compose [ printInstNamePrefix i
                , "select"
                , printConstOrName cond, ","
                , printConstOrName v1, ","
                , printConstOrName v2
                ]
      GetElementPtrInst { getElementPtrInBounds = inBounds
                        , getElementPtrValue = val
                        , getElementPtrIndices = indices
                        } ->
        compose [ printInstNamePrefix i
                , "getelementptr"
                , printInBounds inBounds
                , printConstOrName val, ","
                , intercalate ", " $ map printConstOrName indices
                ]
      CallInst { callIsTail = isTail
               , callConvention = cc
               , callParamAttrs = pattrs
               , callFunction = f
               , callArguments = args
               , callAttrs = cattrs
               , callHasSRet = _
               } ->
        let rtype = valueType i
              -- case valueType f of
              -- TypeFunction r _ _ -> r
              -- TypePointer (TypeFunction r _ _) _ -> r
              -- _ -> error ("Unknown function return type: " ++ show (valueType f))
        in compose [ printInstNamePrefix i
                   , printTailTag isTail
                   , "call"
                   , show cc
                   , unwords $ map show pattrs
                   , printType rtype
                   , printConstOrNameNoType f
                   , "(", intercalate ", " $ map printArgument args, ")"
                   , unwords $ map show cattrs
                   ]
      InvokeInst { invokeConvention = cc
                 , invokeParamAttrs = pattrs
                 , invokeFunction = f
                 , invokeArguments = args
                 , invokeAttrs = atts
                 , invokeNormalLabel = nlabel
                 , invokeUnwindLabel = ulabel
                 , invokeHasSRet = _
                 } ->
        compose [ printInstNamePrefix i
                , "invoke"
                , show cc
                , unwords $ map show pattrs
                , printConstOrName f
                , "(", intercalate ", " $ map printArgument args, ")"
                , unwords $ map show atts
                , "to", printConstOrName (Value nlabel)
                , "unwind", printConstOrName (Value ulabel)
                ]
      VaArgInst { vaArgValue = va } ->
        compose [ printInstNamePrefix i
                , "va_arg"
                , printConstOrName va, ","
                , printType (instructionType i)
                ]
      -- FIXME: This might not be correct in printing the filter
      -- functions...
      LandingPadInst { landingPadPersonality = p
                     , landingPadIsCleanup = isClean
                     , landingPadClauses = cs
                     } ->
        compose [ printInstNamePrefix i
                , "landingpad"
                , printType (instructionType i)
                , "personality"
                , printConstOrName p
                , if isClean then "cleanup" else ""
                , intercalate " " $ map printClause cs
                ]
  ConstantC c -> printConstant c

printClause :: (Value, LandingPadClause) -> String
printClause (v, p) = case p of
  LPCatch -> compose [ "catch", printConstOrName v ]
  LPFilter -> compose [ "filter", printConstOrName v ]

printConstant :: Constant -> String
printConstant c = case c of
  UndefValue { } -> "undef"
  ConstantAggregateZero { } -> "zeroinitializer"
  ConstantPointerNull { } -> "null"
  BlockAddress { blockAddressFunction = f
               , blockAddressBlock = b
               } ->
    mconcat [ "blockaddress("
            , printConstOrNameNoType (Value f), ", "
            , printConstOrNameNoType (Value b), ")"
            ]
  ConstantArray { constantArrayValues = vs } ->
    mconcat [ "[", intercalate ", " $ map printConstOrName vs, "]" ]
  ConstantFP { constantFPValue = d } -> show d
  ConstantInt { constantIntValue = i } -> show i
  ConstantString { constantStringValue = s } -> mconcat [ "c\"", unpack s, "\"" ]
  ConstantStruct { constantStructValues = vs } ->
    mconcat [ "{", intercalate ", " $ map printConstOrName vs, "}" ]
  ConstantVector { constantVectorValues = vs } ->
    mconcat [ "<", intercalate ", " $ map printConstOrName vs, ">" ]
  ConstantValue { constantInstruction = i } ->
    mconcat [ printType (constantType c), " ", printConstInst i ]
  InlineAsm { inlineAsmString = asm
            , inlineAsmConstraints = constraints
            } ->
    mconcat [ "asm \"", unpack asm, "\", \"", unpack constraints, "\"" ]

printArgument :: (Value, [ParamAttribute]) -> String
printArgument (v, atts) =
  compose [ printType $ valueType v
          , unwords $ map show atts
          , printConstOrNameNoType v
          ]

instance Show Argument where
  show a = printArgument (Value a, [])

printConstInst :: Instruction -> String
printConstInst valT = case valT of
  TruncInst { } -> printTypecastConst "trunc" valT
  ZExtInst { } -> printTypecastConst "zext" valT
  SExtInst { } -> printTypecastConst "sext" valT
  FPTruncInst { } -> printTypecastConst "fptrunc" valT
  FPExtInst { } -> printTypecastConst "fpext" valT
  FPToUIInst { } -> printTypecastConst "fptoui" valT
  FPToSIInst { } -> printTypecastConst "fptosi" valT
  UIToFPInst { } -> printTypecastConst "uitofp" valT
  SIToFPInst { } -> printTypecastConst "sitofp" valT
  PtrToIntInst { } -> printTypecastConst "ptrtoint" valT
  IntToPtrInst { } -> printTypecastConst "inttoptr" valT
  BitcastInst { } -> printTypecastConst "bitcast" valT
  GetElementPtrInst { getElementPtrInBounds = inBounds
                    , getElementPtrValue = val
                    , getElementPtrIndices = indices
                    } ->
    compose [ "getelementptr"
            , printInBounds inBounds
            , "("
            , printConstOrName val, ", "
            , intercalate ", " $ map printConstOrName indices
            , ")"
            ]
  SelectInst { selectCondition = cond
             , selectTrueValue = v1
             , selectFalseValue = v2
             } ->
    mconcat [ "select ("
            , printConstOrName cond, ", "
            , printConstOrName v1, ", "
            , printConstOrName v2, ")"
            ]
  ICmpInst { cmpPredicate = cond
           , cmpV1 = v1
           , cmpV2 = v2
           } ->
    mconcat [ "icmp ", show cond, " ("
            , printConstOrName v1, ", "
            , printConstOrName v2, ")"
            ]
  FCmpInst { cmpPredicate = cond
           , cmpV1 = v1
           , cmpV2 = v2
           } ->
    mconcat [ "fcmp ", show cond, " ("
            , printConstOrName v1, ", "
            , printConstOrName v2, ")"
            ]
  ExtractElementInst { extractElementVector = v
                     , extractElementIndex = idx
                     } ->
    mconcat [ "extractelement ("
            , printConstOrName v, ", "
            , printConstOrName idx, ")"
            ]
  InsertElementInst { insertElementVector = vec
                    , insertElementValue = val
                    , insertElementIndex = idx
                    } ->
    mconcat [ "insertelement ("
            , printConstOrName vec, ", "
            , printConstOrName val, ", "
            , printConstOrName idx, ")"
            ]
  ShuffleVectorInst { shuffleVectorV1 = v1
                    , shuffleVectorV2 = v2
                    , shuffleVectorMask = mask
                    } ->
    mconcat [ "shufflevector ("
            , printConstOrName v1, ", "
            , printConstOrName v2, ", "
            , printConstOrName mask, ")"
            ]
  ExtractValueInst { extractValueAggregate = agg
                   , extractValueIndices = indices
                   } ->
    mconcat [ "extractvalue ("
            , printConstOrName agg, ", "
            , intercalate ", " $ map show indices, ")"
            ]
  InsertValueInst { insertValueAggregate = agg
                  , insertValueValue = val
                  , insertValueIndices = indices
                  } ->
    mconcat [ "insertvalue ("
            , printConstOrName agg, ", "
            , printConstOrName val, ", "
            , intercalate ", " $ map show indices, ")"
            ]
  AddInst { } -> printBinaryConst "add" valT
  SubInst { } -> printBinaryConst "sub" valT
  MulInst { } -> printBinaryConst "mul" valT
  DivInst { } -> printBinaryConst "div" valT
  RemInst { } -> printBinaryConst "rem" valT
  ShlInst { } -> printBinaryConst "shl" valT
  LshrInst { } -> printBinaryConst "lshr" valT
  AshrInst { } -> printBinaryConst "ashr" valT
  AndInst { } -> printBinaryConst "and" valT
  OrInst { } -> printBinaryConst "or" valT
  XorInst { } -> printBinaryConst "xor" valT
  _ -> error "Non-constant ValueT"

printBinaryConst :: String -> Instruction -> String
printBinaryConst name inst =
  mconcat [ name, " (", printConstOrName (binaryLhs inst), ", "
          , printConstOrName (binaryRhs inst), ")"
          ]

printTypecastConst :: String -> Instruction -> String
printTypecastConst n inst =
  mconcat [ n, " (", printConstOrName (castedValue inst)
          , " to ", printType (instructionType inst), ")"
          ]

printTailTag :: Bool -> String
printTailTag isTail = if isTail then "tail" else ""

printVolatileFlag :: Bool -> String
printVolatileFlag f = if f then "volatile" else ""

printAlignment :: Int64 -> String
printAlignment align = case align of
  0 -> ""
  _ -> ", align " ++ show align

printTypecast :: String -> Instruction -> String
printTypecast str inst =
  compose [ printInstNamePrefix inst
          , str
          , printConstOrName (castedValue inst)
          , "to"
          , printType (valueType inst) -- newType
          ]

printInBounds :: Bool -> String
printInBounds inBounds = if inBounds then "inbounds" else ""

printFlaggedBinaryOp :: String -> Instruction -> String
printFlaggedBinaryOp str inst =
  compose [ printInstNamePrefix inst
          , str
          , show (binaryArithFlags inst)
          , printType (instructionType inst)
          , printConstOrNameNoType (binaryLhs inst), ","
          , printConstOrNameNoType (binaryRhs inst)
          ]

printBinaryOp :: String -> Instruction -> String
printBinaryOp str inst =
  compose [ printInstNamePrefix inst
          , str
          , printType (instructionType inst)
          , printConstOrNameNoType (binaryLhs inst), ","
          , printConstOrNameNoType (binaryRhs inst)
          ]

printInstNamePrefix :: Instruction -> String
printInstNamePrefix i = case instructionName i of
  Nothing -> ""
  Just n -> mconcat [ show n, " =" ]

-- | This is kind of gross - it only prints out the first piece of
-- metadata.
printDebugTag :: [Metadata] -> String
printDebugTag [] = ""
printDebugTag (md:_) = ", !dbg !" ++ show (metaValueUniqueId md)

printType :: Type -> String
printType (TypeInteger bits) = 'i' : show bits
printType TypeFloat = "float"
printType TypeDouble = "double"
printType TypeFP128 = "fp128"
printType TypeX86FP80 = "x86_fp80"
printType TypePPCFP128 = "ppc_fp128"
printType TypeX86MMX = "x86mmx"
printType TypeVoid = "void"
printType TypeLabel = "label"
printType TypeMetadata = "metadata"
printType (TypeArray n ty) = mconcat [ "[", show n, " x ", printType ty, "]" ]
printType (TypeVector n ty) = mconcat [ "<", show n, " x ", printType ty, ">" ]
printType (TypeFunction retT argTs isVa) =
  mconcat [ printType retT, "(", argVals, vaTag, ")" ]
  where argVals :: String
        argVals = intercalate ", " $ map printType argTs
        vaTag :: String
        vaTag = if isVa then ", ..." else ""
printType (TypePointer ty _) = mconcat [ printType ty, "*" ]
printType (TypeStruct Nothing ts p) =
  case p of
    True -> mconcat [ "<", fieldVals, ">" ]
    False -> mconcat [ "{", fieldVals, "}" ]
  where fieldVals = intercalate ", " $ map printType ts
printType (TypeStruct (Just n) _ _) = '%' : n

instance Show Metadata where
  show = printMetadata

instance Show Type where
  show = printType

instance Show Value where
  show = printValue

instance Labellable Value where
  toLabelValue = toLabelValue . show

instance Show Instruction where
  show = printValue . Value

instance Show Function where
  show = printValue . Value

instance Show GlobalVariable where
  show = printValue . Value