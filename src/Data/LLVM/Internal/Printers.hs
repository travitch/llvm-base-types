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
import Data.List ( intersperse )
import Data.Monoid
import Data.Text ( Text, unpack )
import Data.Text.Lazy ( toStrict )
import Data.Text.Lazy.Builder
import qualified Text.PrettyPrint as PP
import Text.PrettyPrint.GenericPretty

import Data.LLVM.Types.Attributes
import Data.LLVM.Types.Identifiers
import Data.LLVM.Types.Referential

-- TODO List
--
-- * Pretty up the DataLayout
-- * Print out named type definitions
-- * Make the function type printing as flexible as the official
--   version

showUntypedMDName :: Metadata -> Builder
showUntypedMDName = fromString . ("!"++) . show . metaValueUniqueId

showMDName :: Metadata -> Builder
showMDName = fromString . ("metadata !"++) . show . metaValueUniqueId

showMDString :: Text -> Builder
showMDString t = mconcat [ fromString "metadata !\""
                         , fromText t
                         , singleton '"'
                         ]

showBool :: Bool -> Builder
showBool True = fromString "i1 true"
showBool False = fromString "i1 false"

maybeShowMDName :: Maybe Metadata -> Builder
maybeShowMDName Nothing = fromString "null"
maybeShowMDName (Just m) = showMDName m

dbgTag :: Int -> Builder
dbgTag i = fromShow (i + fromIntegral llvmDebugVersion)

printMetadata :: Metadata -> Builder
printMetadata md@MetaSourceLocation { } =
  mconcat [ showUntypedMDName md, fromString " = metadata !{i32 ", fromShow (metaSourceRow md)
          , fromString ", i32 ", fromShow (metaSourceCol md)
          , fromString ", ", maybeShowMDName (metaSourceScope md)
          , fromString" null}"
          ]
printMetadata md@MetaDWLexicalBlock { } =
  mconcat [ showUntypedMDName md, fromString " = metadata !{i32 ", dbgTag 11
          , fromString ", i32 ", fromShow (metaLexicalBlockRow md)
          , fromString ", i32 ", fromShow (metaLexicalBlockCol md)
          , fromString ", ", maybeShowMDName (metaLexicalBlockContext md)
          , fromString "}"
          ]
printMetadata md@MetaDWCompileUnit {} =
  mconcat [ showUntypedMDName md, fromString " = metadata !{i32 ", dbgTag 17
          , fromString ", i32 ", fromShow (metaCompileUnitLanguage md)
          , fromString ", ", showMDString (metaCompileUnitSourceFile md)
          , fromString ", ", showMDString (metaCompileUnitCompileDir md)
          , fromString ", ", showMDString (metaCompileUnitProducer md)
          , fromString ", ", showBool (metaCompileUnitIsMain md)
          , fromString ", ", showBool (metaCompileUnitIsOpt md)
          , fromString ", i32 ", fromShow (metaCompileUnitVersion md)
          , fromString "}"
          ]
printMetadata md@MetaDWFile {} =
  mconcat [ showUntypedMDName md, fromString " = metadata !{i32 ", dbgTag 41
          , fromString ", ", showMDString (metaFileSourceFile md)
          , fromString ", ", showMDString (metaFileSourceDir md)
          , fromString "}"
          ]
printMetadata md@MetaDWVariable {} =
  mconcat [ showUntypedMDName md, fromString " = metadata !{i32 ", dbgTag 52
          , fromString ", ", maybeShowMDName (metaGlobalVarContext md)
          , fromString ", ", showMDString (metaGlobalVarName md)
          , fromString ", ", showMDString (metaGlobalVarDisplayName md)
          , fromString ", ", showMDString (metaGlobalVarLinkageName md)
          , fromString ", i32 ", fromShow (metaGlobalVarLine md)
          , fromString ", ", maybeShowMDName (metaGlobalVarType md)
          , fromString ", ", showBool (metaGlobalVarStatic md)
          , fromString ", ", showBool (metaGlobalVarNotExtern md)
          , fromString "}"
          ]
printMetadata md@MetaDWSubprogram {} =
  mconcat [ showUntypedMDName md, fromString " = metadata !{i32 ", dbgTag 46
          , fromString ", ", maybeShowMDName (metaSubprogramContext md)
          , fromString ", ", showMDString (metaSubprogramName md)
          , fromString ", ", showMDString (metaSubprogramDisplayName md)
          , fromString ", ", showMDString (metaSubprogramLinkageName md)
          , fromString ", i32 ", fromShow (metaSubprogramLine md)
          , fromString ", ", maybeShowMDName (metaSubprogramType md)
          , fromString ", ", showBool (metaSubprogramStatic md)
          , fromString ", ", showBool (metaSubprogramNotExtern md)
          , fromString ", i32 ", fromShow (metaSubprogramVirtuality md)
          , fromString ", i32 ", fromShow (metaSubprogramVirtIndex md)
          , fromString ", ", maybeShowMDName (metaSubprogramBaseType md)
          , fromString ", ", showBool (metaSubprogramArtificial md)
          , fromString ", ", showBool (metaSubprogramOptimized md)
          , fromString "}"
          ]
printMetadata md@MetaDWBaseType {} =
  mconcat [ showUntypedMDName md, fromString " = metadata !{i32 ", dbgTag 36
          , fromString ", ", maybeShowMDName (metaBaseTypeContext md)
          , fromString ", ", showMDString (metaBaseTypeName md)
          , fromString ", ", maybeShowMDName (metaBaseTypeFile md)
          , fromString ", i32 ", fromShow (metaBaseTypeLine md)
          , fromString ", i32 ", fromShow (metaBaseTypeSize md)
          , fromString ", i32 ", fromShow (metaBaseTypeAlign md)
          , fromString ", i64 ", fromShow (metaBaseTypeOffset md)
          , fromString ", i32 ", fromShow (metaBaseTypeFlags md)
          , fromString ", i32 ", fromShow (metaBaseTypeEncoding md)
          , fromString "}"
          ]
printMetadata md@MetaDWDerivedType {} =
  mconcat [ showUntypedMDName md, fromString " = metadata !{i32 ", fromShow (metaDerivedTypeTag md)
          , fromString ", ", maybeShowMDName (metaDerivedTypeContext md)
          , fromString ", ", showMDString (metaDerivedTypeName md)
          , fromString ", ", maybeShowMDName (metaDerivedTypeFile md)
          , fromString ", i32 ", fromShow (metaDerivedTypeLine md)
          , fromString ", i32 ", fromShow (metaDerivedTypeSize md)
          , fromString ", i32 ", fromShow (metaDerivedTypeAlign md)
          , fromString ", i64 ", fromShow (metaDerivedTypeOffset md)
          , fromString ", ", maybeShowMDName (metaDerivedTypeParent md)
          , fromString "}"
          ]
printMetadata md@MetaDWCompositeType {} =
  mconcat [ showUntypedMDName md, fromString " = metadata !{i32 ", fromShow (metaCompositeTypeTag md)
          , fromString ", ", maybeShowMDName (metaCompositeTypeContext md)
          , fromString ", ", showMDString (metaCompositeTypeName md)
          , fromString ", ", maybeShowMDName (metaCompositeTypeFile md)
          , fromString ", i32 ", fromShow (metaCompositeTypeLine md)
          , fromString ", i32 ", fromShow (metaCompositeTypeSize md)
          , fromString ", i32 ", fromShow (metaCompositeTypeAlign md)
          , fromString ", i64 ", fromShow (metaCompositeTypeOffset md)
          , fromString ", i32 ", fromShow (metaCompositeTypeFlags md)
          , fromString ", ", maybeShowMDName (metaCompositeTypeParent md)
          , fromString ", ", maybeShowMDName (metaCompositeTypeMembers md)
          , fromString ", i32 ", fromShow (metaCompositeTypeRuntime md)
          , fromString "}"
          ]
printMetadata md@MetaDWSubrange {} =
  mconcat [ showUntypedMDName md, fromString " = metadata !{i32 ", dbgTag 33
          , fromString ", i32 ", fromShow (metaSubrangeLow md)
          , fromString ", i32 ", fromShow (metaSubrangeHigh md)
          , fromString "}"
          ]
printMetadata md@MetaDWEnumerator {} =
  mconcat [ showUntypedMDName md, fromString " = metadata !{i32 ", dbgTag 40
          , fromString ", ", showMDString (metaEnumeratorName md)
          , fromString ", i32 ", fromShow (metaEnumeratorValue md)
          , fromString "}"
          ]
printMetadata md@MetaDWLocal {} =
  mconcat [ showUntypedMDName md, fromString " = metadata !{i32 ", fromShow (metaLocalTag md)
          , fromString ", ", maybeShowMDName (metaLocalContext md)
          , fromString ", ", showMDString (metaLocalName md)
          , fromString ", i32 ", fromShow (metaLocalLine md)
          , fromString ", ", maybeShowMDName (metaLocalType md)
          , fromString "}"
          ]
printMetadata md@(MetadataList _ vals) =
  mconcat [ showUntypedMDName md, fromString " = metadata !{"
          , mconcat $ intersperse (fromString ", ") (map maybeShowMDName vals)
          , fromString "}"
          ]
printMetadata md@MetaDWNamespace {} =
  mconcat [ showUntypedMDName md, fromString " = metadata !{i32 ", dbgTag 57
          , fromString ", ", showMDString (metaNamespaceName md)
          , fromString ", ", maybeShowMDName (metaNamespaceContext md)
          , fromString ", i32 ", fromShow (metaNamespaceLine md)
          , fromString "}"
          ]
printMetadata md@MetaDWTemplateTypeParameter {} =
  mconcat [ showUntypedMDName md, fromString " = metadata !{i32 ", dbgTag 0x2f
          , fromString ", ", showMDString (metaTemplateTypeParameterName md)
          , fromString ", i32 ", fromShow (metaTemplateTypeParameterLine md)
          , fromString ", i32 ", fromShow (metaTemplateTypeParameterCol md)
          , fromString ", ", maybeShowMDName (metaTemplateTypeParameterContext md)
          , fromString ", ", maybeShowMDName (metaTemplateTypeParameterType md)
          , fromString "}"
          ]
printMetadata md@MetaDWTemplateValueParameter {} =
  mconcat [ showUntypedMDName md, fromString " = metadata !{i32 ", dbgTag 0x30
          , fromString ", ", showMDString (metaTemplateValueParameterName md)
          , fromString ", i32 ", fromShow (metaTemplateValueParameterLine md)
          , fromString ", i32 ", fromShow (metaTemplateValueParameterCol md)
          , fromString ", ", maybeShowMDName (metaTemplateValueParameterContext md)
          , fromString ", ", maybeShowMDName (metaTemplateValueParameterType md)
          , fromString ", i64 ", fromShow (metaTemplateValueParameterValue md)
          , fromString "}"
          ]
printMetadata md@(MetadataUnknown _ s) =
  mconcat [ showUntypedMDName md, fromString " = metadata ", fromText s ]

-- Take all of the asm chunks, break their contents into lines,
-- then wrap each of those lines in the 'module asm' wrapper.
-- Combine them into a single string with newlines.
printAsm :: Assembly -> Builder
printAsm asm = mconcat asmLines
  where
    asmLines = map adorn (lines (show asm))
    adorn s = mconcat [fromString "module asm \"", fromString s, fromString "\"\n"]

-- When referencing a non-constant value during printing, just use
-- this instead of printValue to avoid problems printing cyclic data.
-- If the value doesn't have a name, just print it (it should be a
-- constant).
printConstOrName :: Value -> Builder
printConstOrName v =
  case valueName v of
    Nothing -> mconcat [ printType (valueType v), singleton ' ', printValue v ]
    Just ident -> mconcat [ printType (valueType v), singleton ' ', fromShow ident ]

printConstOrNameNoType :: Value -> Builder
printConstOrNameNoType v =
  case valueName v of
    Nothing -> printValue v
    Just ident -> fromShow ident

compose :: [Builder] -> Builder
compose = mconcat . intersperse (singleton ' ') . filter (/= mempty)

quote :: Builder -> Builder
quote s = mconcat [ singleton '\\', s, singleton '\\' ]

printValue :: Value -> Builder
printValue v = case valueContent v of
  FunctionC f ->
    let retAttrS = unwords $ map show (functionRetAttrs f)
        argS = commaSep $ map (printValue . toValue) (functionParameters f)
        fAttrS = spaceSep $ map fromShow (functionAttrs f)
        bodyS = lineSep $ map (printValue . toValue) (functionBody f)
        vaTag = if functionIsVararg f then ", ..." else ""
        (TypeFunction rtype _ _) = functionType f
        name = functionName f
    in compose [ fromString "define", fromShow (functionLinkage f)
               , fromShow (functionVisibility f), fromShow (functionCC f)
               , fromString retAttrS, printType rtype, fromShow name, singleton '('
               , argS, fromString vaTag, singleton ')', fAttrS
               , maybe mempty fromText (functionSection f)
               , printAlignment (functionAlign f)
               , maybe mempty fromShow (functionGCName f)
               , fromString "{\n", bodyS, singleton '}'
               ]
  ArgumentC a ->
    compose [ printType (argumentType a)
            , compose $ map fromShow (argumentParamAttrs a)
            , fromShow (argumentName a)
            ]
  BasicBlockC b ->
    let indent = (fromString "  " `mappend`)
        dbgS = map (printDebugTag . valueMetadata) (basicBlockInstructions b)
        instS = map (printValue . toValue) (basicBlockInstructions b)
        instS' = zipWith mappend instS dbgS
        instS'' = mconcat $ intersperse (singleton '\n') $ map indent instS'
        identS = fromText $ identifierContent (basicBlockName b)
        label = case isAnonymousIdentifier (basicBlockName b) of
          True -> fromString "; <label>:" `mappend` identS
          False -> identS `mappend` singleton ':'
    in mconcat [ label, singleton '\n', instS'' ]
  GlobalVariableC g ->
    let TypePointer _ addrSpace = globalVariableType g
        addrSpaceS = case addrSpace of
          0 -> mempty
          _ -> mconcat [ fromString "addrspace(", fromShow addrSpace, singleton ')' ]
        annotsS = if globalVariableIsConstant g then fromString "constant" else mempty
        initS = maybe mempty printConstOrName (globalVariableInitializer g)
        sectionS = maybe mempty ((fromString ", section" `mappend`) . quote . fromText) (globalVariableSection g)
    in compose [ fromShow (globalVariableName g), singleton '=', addrSpaceS
               , fromShow (globalVariableLinkage g), fromShow (globalVariableVisibility g)
               , annotsS, initS, sectionS, printAlignment (globalVariableAlignment g)
               ]
  GlobalAliasC a ->
    compose [ fromShow (globalAliasName a)
            , fromString "= alias"
            , fromShow (globalAliasLinkage a)
            , fromShow (globalAliasVisibility a)
            , printConstOrName (globalAliasTarget a)
            ]
  ExternalValueC e ->
    compose [ fromString "declare"
            , printType (valueType e)
            , fromShow (externalValueName e)
            ]
  ExternalFunctionC e ->
    let TypeFunction rtype argTypes isva = externalFunctionType e
    in compose [ fromString "declare", printType rtype
               , fromShow (externalFunctionName e)
               , singleton '(', commaSep $ map printType argTypes
               , fromString $ if isva then ", ..." else ""
               , singleton ')'
               ]
  InstructionC i ->
    case i of
      RetInst { retInstValue = Just rv } ->
        compose [ fromString "ret", printConstOrName rv ]
      RetInst { } -> fromString "ret void"
      ResumeInst { resumeException = val } ->
        compose [ fromString "resume", printConstOrName val ]
      UnconditionalBranchInst { unconditionalBranchTarget = dest } ->
        compose [ fromString "br", (printConstOrName . toValue) dest ]
      BranchInst { branchCondition = cond
                 , branchTrueTarget = tTarget
                 , branchFalseTarget = fTarget
                 } ->
        compose [ fromString "br", printConstOrName cond
                , singleton ',', printConstOrName (toValue tTarget)
                , singleton ',', printConstOrName (toValue fTarget)
                ]
      SwitchInst { switchValue = val
                 , switchDefaultTarget = defTarget
                 , switchCases = cases
                 } ->
        let caseDests = mconcat $ intersperse (singleton ' ') $ map printPair cases
            printPair (caseVal, caseDest) =
              mconcat [ printConstOrName caseVal
                      , fromString ", "
                      , printConstOrName (toValue caseDest)
                      ]
        in compose [ fromString "switch", printConstOrName val
                   , singleton ',', printConstOrName (toValue defTarget)
                   , singleton '[', caseDests, singleton ']'
                   ]
      IndirectBranchInst { indirectBranchAddress = addr
                         , indirectBranchTargets = targets
                         } ->
        compose [ fromString "indirectbr", printConstOrName addr
                , singleton '[', commaSep $ map (printConstOrName . toValue) targets
                , singleton ']'
                ]
      UnreachableInst { } -> fromString "unreachable"
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
                , fromString "extractelement"
                , printConstOrName vec, singleton ','
                , printConstOrName idx
                ]
      InsertElementInst { insertElementVector = vec
                        , insertElementValue = val
                        , insertElementIndex = idx
                        } ->
        compose [ printInstNamePrefix i
                , fromString "insertelement"
                , printConstOrName vec, singleton ','
                , printConstOrName val, singleton ','
                , printConstOrName idx
                ]
      ShuffleVectorInst { shuffleVectorV1 = v1
                        , shuffleVectorV2 = v2
                        , shuffleVectorMask = mask
                        } ->
        compose [ printInstNamePrefix i
                , fromString "shufflevector"
                , printConstOrName v1, singleton ','
                , printConstOrName v2, singleton ','
                , printConstOrName mask
                ]
      ExtractValueInst { extractValueAggregate = agg
                       , extractValueIndices = indices
                       } ->
        compose [ printInstNamePrefix i
                , fromString "extractvalue"
                , printConstOrName agg
                , commaSep $ map fromShow indices
                ]
      InsertValueInst { insertValueAggregate = agg
                      , insertValueValue = val
                      , insertValueIndices = indices
                      } ->
        compose [ printInstNamePrefix i
                , fromString "insertvalue"
                , printConstOrName agg, singleton ','
                , printConstOrName val, singleton ','
                , commaSep $ map fromShow indices
                ]
      AllocaInst { allocaNumElements = elems
                 , allocaAlign = align
                 } ->
        let count = case valueContent elems of
              ConstantC ConstantInt { constantIntValue = 1 } -> mempty
              _ -> fromString ", " `mappend` printConstOrName elems
            TypePointer ty _ = instructionType i
        in   compose [ printInstNamePrefix i
                     , fromString "alloca"
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
                , fromString "load"
                , printConstOrName src
                , printAlignment align
                ]
      StoreInst { storeIsVolatile = volatile
                , storeValue = val
                , storeAddress = dest
                , storeAlignment = align
                } ->
        compose [ printVolatileFlag volatile
                , fromString "store"
                , printConstOrName val, singleton ','
                , printConstOrName dest
                , printAlignment align
                ]
      FenceInst { fenceOrdering = o, fenceScope = s } ->
        compose [ fromString "fence", fromShow s, fromShow o ]
      AtomicCmpXchgInst { atomicCmpXchgOrdering = o
                        , atomicCmpXchgScope = s
                        , atomicCmpXchgIsVolatile = isVol
                        , atomicCmpXchgAddressSpace = _ -- ?
                        , atomicCmpXchgPointer = ptr
                        , atomicCmpXchgComparison = cmp
                        , atomicCmpXchgNewValue = newV
                        } ->
        compose [ fromString "cmpxchg", printVolatileFlag isVol
                , printConstOrName ptr, singleton ','
                , printConstOrName cmp, singleton ','
                , printConstOrName newV
                , fromShow s, fromShow o
                ]
      AtomicRMWInst { atomicRMWOrdering = o
                    , atomicRMWScope = s
                    , atomicRMWOperation = op
                    , atomicRMWIsVolatile = isVol
                    , atomicRMWPointer = p
                    , atomicRMWValue = val
                    , atomicRMWAddressSpace = _ -- ?
                    } ->
        compose [ fromString "atomicrmw", printVolatileFlag isVol
                , fromShow op
                , printConstOrName p, singleton ','
                , printConstOrName val
                , fromShow s
                , fromShow o
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
                , fromString "icmp"
                , fromShow cond
                , printConstOrName v1, singleton ','
                , printConstOrNameNoType v2
                ]
      FCmpInst { cmpPredicate = cond
               , cmpV1 = v1
               , cmpV2 = v2
               } ->
        compose [ printInstNamePrefix i
                , fromString "fcmp"
                , fromShow cond
                , printConstOrName v1, singleton ','
                , printConstOrNameNoType v2
                ]
      PhiNode { phiIncomingValues = vals
              } ->
        let printPair (val, lab) =
              mconcat [ singleton '[', printConstOrNameNoType val
                      , fromString ", ", printConstOrNameNoType lab
                      , singleton ']'
                      ]
            valS = mconcat $ intersperse (fromString ", ") $ map printPair vals
        in compose [ printInstNamePrefix i
                   , fromString "phi"
                   , printType (instructionType i)
                   , singleton '[', valS, singleton ']'
                   ]
      SelectInst { selectCondition = cond
                 , selectTrueValue = v1
                 , selectFalseValue = v2
                 } ->
        compose [ printInstNamePrefix i
                , fromString "select"
                , printConstOrName cond, singleton ','
                , printConstOrName v1, singleton ','
                , printConstOrName v2
                ]
      GetElementPtrInst { getElementPtrInBounds = inBounds
                        , getElementPtrValue = val
                        , getElementPtrIndices = indices
                        } ->
        compose [ printInstNamePrefix i
                , fromString "getelementptr"
                , printInBounds inBounds
                , printConstOrName val, singleton ','
                , mconcat $ intersperse (fromString ", ") $ map printConstOrName indices
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
                   , fromString "call"
                   , fromShow cc
                   , mconcat $ intersperse (singleton ' ') $ map fromShow pattrs
                   , printType rtype
                   , printConstOrNameNoType f
                   , singleton '('
                   , mconcat $ intersperse (fromString ", ") $ map printArgument args
                   , singleton ')'
                   , mconcat $ intersperse (singleton ' ') $ map fromShow cattrs
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
                , fromString "invoke"
                , fromShow cc
                , spaceSep $ map fromShow pattrs
                , printConstOrName f
                , singleton '('
                , commaSep $ map printArgument args
                , singleton ')'
                , spaceSep $ map fromShow atts
                , fromString "to", printConstOrName (toValue nlabel)
                , fromString "unwind", printConstOrName (toValue ulabel)
                ]
      VaArgInst { vaArgValue = va } ->
        compose [ printInstNamePrefix i
                , fromString "va_arg"
                , printConstOrName va, singleton ','
                , printType (instructionType i)
                ]
      -- FIXME: This might not be correct in printing the filter
      -- functions...
      LandingPadInst { landingPadPersonality = p
                     , landingPadIsCleanup = isClean
                     , landingPadClauses = cs
                     } ->
        compose [ printInstNamePrefix i
                , fromString "landingpad"
                , printType (instructionType i)
                , fromString "personality"
                , printConstOrName p
                , if isClean then fromString "cleanup" else mempty
                , spaceSep $ map printClause cs
                ]
  ConstantC c -> printConstant c

printClause :: (Value, LandingPadClause) -> Builder
printClause (v, p) =
  case p of
    LPCatch -> compose [ fromString "catch", printConstOrName v ]
    LPFilter -> compose [ fromString "filter", printConstOrName v ]

commaSep :: [Builder] -> Builder
commaSep = mconcat . intersperse (fromString ", ")

spaceSep :: [Builder] -> Builder
spaceSep = mconcat . intersperse (singleton ' ')

lineSep :: [Builder] -> Builder
lineSep = mconcat . intersperse (singleton '\n')

printConstant :: Constant -> Builder
printConstant c = case c of
  UndefValue { } -> fromString "undef"
  ConstantAggregateZero { } -> fromString "zeroinitializer"
  ConstantPointerNull { } -> fromString "null"
  BlockAddress { blockAddressFunction = f
               , blockAddressBlock = b
               } ->
    mconcat [ fromString "blockaddress("
            , printConstOrNameNoType (toValue f)
            , fromString ", "
            , printConstOrNameNoType (toValue b)
            , singleton ')'
            ]
  ConstantArray { constantArrayValues = vs } ->
    mconcat [ singleton '['
            , commaSep $ map printConstOrName vs, singleton ']'
            ]
  ConstantFP { constantFPValue = d } -> fromShow d
  ConstantInt { constantIntValue = i } -> fromShow i
  ConstantString { constantStringValue = s } ->
    mconcat [ fromString "c\"", fromText s, singleton '"' ]
  ConstantStruct { constantStructValues = vs } ->
    mconcat [ singleton '{', commaSep $ map printConstOrName vs, singleton '}' ]
  ConstantVector { constantVectorValues = vs } ->
    mconcat [ singleton '<', commaSep $ map printConstOrName vs, singleton '>' ]
  ConstantValue { constantInstruction = i } ->
    mconcat [ printType (constantType c), singleton ' ', printConstInst i ]
  InlineAsm { inlineAsmString = asm
            , inlineAsmConstraints = constraints
            } ->
    mconcat [ fromString "asm \"", fromText asm
            , fromString "\", \"", fromText constraints, singleton '"' ]

printArgument :: (Value, [ParamAttribute]) -> Builder
printArgument (v, atts) =
  compose [ printType $ valueType v
          , spaceSep $ map fromShow atts
          , printConstOrNameNoType v
          ]

instance Show Argument where
  show a = builderToString $ printArgument (toValue a, [])

printConstInst :: Instruction -> Builder
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
    compose [ fromString "getelementptr"
            , printInBounds inBounds
            , singleton '('
            , printConstOrName val, fromString ", "
            , commaSep $ map printConstOrName indices
            , singleton ')'
            ]
  SelectInst { selectCondition = cond
             , selectTrueValue = v1
             , selectFalseValue = v2
             } ->
    mconcat [ fromString "select ("
            , printConstOrName cond, fromString ", "
            , printConstOrName v1, fromString ", "
            , printConstOrName v2, singleton ')'
            ]
  ICmpInst { cmpPredicate = cond
           , cmpV1 = v1
           , cmpV2 = v2
           } ->
    mconcat [ fromString "icmp ", fromShow cond, fromString " ("
            , printConstOrName v1, fromString ", "
            , printConstOrName v2, singleton ')'
            ]
  FCmpInst { cmpPredicate = cond
           , cmpV1 = v1
           , cmpV2 = v2
           } ->
    mconcat [ fromString "fcmp ", fromShow cond, fromString " ("
            , printConstOrName v1, fromString ", "
            , printConstOrName v2, singleton ')'
            ]
  ExtractElementInst { extractElementVector = v
                     , extractElementIndex = idx
                     } ->
    mconcat [ fromString "extractelement ("
            , printConstOrName v, fromString ", "
            , printConstOrName idx, singleton ')'
            ]
  InsertElementInst { insertElementVector = vec
                    , insertElementValue = val
                    , insertElementIndex = idx
                    } ->
    mconcat [ fromString "insertelement ("
            , printConstOrName vec, fromString ", "
            , printConstOrName val, fromString ", "
            , printConstOrName idx, singleton ')'
            ]
  ShuffleVectorInst { shuffleVectorV1 = v1
                    , shuffleVectorV2 = v2
                    , shuffleVectorMask = mask
                    } ->
    mconcat [ fromString "shufflevector ("
            , printConstOrName v1, fromString ", "
            , printConstOrName v2, fromString ", "
            , printConstOrName mask, singleton ')'
            ]
  ExtractValueInst { extractValueAggregate = agg
                   , extractValueIndices = indices
                   } ->
    mconcat [ fromString "extractvalue ("
            , printConstOrName agg, fromString ", "
            , commaSep $ map fromShow indices, singleton ')'
            ]
  InsertValueInst { insertValueAggregate = agg
                  , insertValueValue = val
                  , insertValueIndices = indices
                  } ->
    mconcat [ fromString "insertvalue ("
            , printConstOrName agg, fromString ", "
            , printConstOrName val, fromString ", "
            , commaSep $ map fromShow indices, singleton ')'
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

printBinaryConst :: String -> Instruction -> Builder
printBinaryConst name inst =
  mconcat [ fromString name, fromString " ("
          , printConstOrName (binaryLhs inst), fromString ", "
          , printConstOrName (binaryRhs inst), singleton ')'
          ]

printTypecastConst :: String -> Instruction -> Builder
printTypecastConst n inst =
  mconcat [ fromString n, fromString " (", printConstOrName (castedValue inst)
          , fromString " to ", printType (instructionType inst), singleton ')'
          ]

printTailTag :: Bool -> Builder
printTailTag isTail = if isTail then fromString "tail" else mempty

printVolatileFlag :: Bool -> Builder
printVolatileFlag f = if f then fromString "volatile" else mempty

printAlignment :: Int64 -> Builder
printAlignment align =
  case align of
    0 -> mempty
    _ -> fromString ", align " `mappend` fromShow align

printTypecast :: String -> Instruction -> Builder
printTypecast str inst =
  compose [ printInstNamePrefix inst
          , fromString str
          , printConstOrName (castedValue inst)
          , fromString "to"
          , printType (valueType inst)
          ]

printInBounds :: Bool -> Builder
printInBounds inBounds = if inBounds then fromString "inbounds" else mempty

printFlaggedBinaryOp :: String -> Instruction -> Builder
printFlaggedBinaryOp str inst =
  compose [ printInstNamePrefix inst
          , fromString str
          , fromShow (binaryArithFlags inst)
          , printType (instructionType inst)
          , printConstOrNameNoType (binaryLhs inst), singleton ','
          , printConstOrNameNoType (binaryRhs inst)
          ]

printBinaryOp :: String -> Instruction -> Builder
printBinaryOp str inst =
  compose [ printInstNamePrefix inst
          , fromString str
          , printType (instructionType inst)
          , printConstOrNameNoType (binaryLhs inst), singleton ','
          , printConstOrNameNoType (binaryRhs inst)
          ]

printInstNamePrefix :: Instruction -> Builder
printInstNamePrefix i =
  case instructionName i of
    Nothing -> mempty
    Just n -> mconcat [ fromShow n, fromString " =" ]

-- | This is kind of gross - it only prints out the first piece of
-- metadata.
printDebugTag :: [Metadata] -> Builder
printDebugTag [] = mempty
printDebugTag (md:_) =
  fromString ", !dbg !" `mappend` fromShow (metaValueUniqueId md)

printType :: Type -> Builder
printType (TypeInteger bits) = singleton 'i' `mappend` fromShow bits
printType TypeFloat = fromString "float"
printType TypeDouble = fromString "double"
printType TypeFP128 = fromString "fp128"
printType TypeX86FP80 = fromString "x86_fp80"
printType TypePPCFP128 = fromString "ppc_fp128"
printType TypeX86MMX = fromString "x86mmx"
printType TypeVoid = fromString "void"
printType TypeLabel = fromString "label"
printType TypeMetadata = fromString "metadata"
printType (TypeArray n ty) =
  mconcat [ singleton '[', fromShow n, fromString " x "
          , printType ty, singleton ']'
          ]
printType (TypeVector n ty) =
  mconcat [ singleton '<', fromShow n, fromString " x "
          , printType ty, singleton '>'
          ]
printType (TypeFunction retT argTs isVa) =
  mconcat [ printType retT, singleton '(', argVals, vaTag, singleton ')' ]
  where
    argVals = commaSep $ map printType argTs
    vaTag = if isVa then fromString ", ..." else mempty
printType (TypePointer ty _) = mconcat [ printType ty, singleton '*' ]
printType (TypeStruct (Left _) ts p) =
  case p of
    True -> mconcat [ singleton '<', fieldVals, singleton '>' ]
    False -> mconcat [ singleton '{', fieldVals, singleton '}' ]
  where fieldVals = commaSep $ map printType ts
printType (TypeStruct (Right n) _ _) = singleton '%' `mappend` fromString n

instance Show Metadata where
  show = builderToString . printMetadata

instance Show Type where
  show = builderToString . printType

instance Show Value where
  show = builderToString . printValue

instance Labellable Value where
  toLabelValue = toLabelValue . show

instance Show Instruction where
  show = builderToString . printValue . toValue

instance Show Function where
  show = builderToString . printValue . toValue

instance Show GlobalVariable where
  show = builderToString . printValue . toValue

instance Out Type where
  docPrec _ = PP.text . show
  doc = PP.text . show

instance Out Value where
  docPrec _ = PP.text . show
  doc = PP.text . show

instance Out Instruction where
  docPrec _ = PP.text . show
  doc = PP.text . show

builderToString :: Builder -> String
builderToString = unpack . toStrict . toLazyText

fromShow :: (Show a) => a -> Builder
fromShow = fromString . show