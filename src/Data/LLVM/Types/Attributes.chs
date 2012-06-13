module Data.LLVM.Types.Attributes (
  -- * Types
  ArithFlags(..),
  CmpPredicate(..),
  CallingConvention(..),
  LinkageType(..),
  VisibilityStyle(..),
  ParamAttribute(..),
  FunctionAttribute(..),
  TargetTriple(..),
  Assembly(..),
  AtomicOperation(..),
  LandingPadClause(..),
  AtomicOrdering(..),
  SynchronizationScope(..)
  ) where

#include "c++/llvm-base-enums.h"

import Control.DeepSeq
import Data.Default
import Data.Text ( Text, unpack )

{#enum LinkageType {} deriving (Eq) #}
instance Show LinkageType where
  show LTExternal = ""
  show LTAvailableExternally = "available_externally"
  show LTLinkOnceAny = "linkonce"
  show LTLinkOnceODR = "linkonce_odr"
  show LTWeakAny = "weak"
  show LTWeakODR = "weak_odr"
  show LTAppending = "appending"
  show LTInternal = "internal"
  show LTPrivate = "private"
  show LTLinkerPrivate = "linker_private"
  show LTLinkerPrivateWeak = "linker_private_weak"
  show LTLinkerPrivateWeakDefAuto = "linker_private_weak_def_auto"
  show LTDLLImport = "dllimport"
  show LTDLLExport = "dllexport"
  show LTExternalWeak = "extern_weak"
  show LTCommon = "common"

instance NFData LinkageType
instance Default LinkageType where
  def = LTExternal

{#enum VisibilityStyle {} deriving (Eq) #}

instance Show VisibilityStyle where
  show VisibilityDefault = ""
  show VisibilityHidden = "hidden"
  show VisibilityProtected = "protected"

instance NFData VisibilityStyle
instance Default VisibilityStyle where
  def = VisibilityDefault

{#enum CAtomicOrdering as AtomicOrdering {} deriving (Eq) #}

instance Show AtomicOrdering where
  show OrderNotAtomic = ""
  show OrderUnordered = "unordered"
  show OrderMonotonic = "monotonic"
  show OrderAcquire = "acquire"
  show OrderRelease = "release"
  show OrderAcquireRelease = "acq_rel"
  show OrderSequentiallyConsistent = "seq_cst"

instance NFData AtomicOrdering
instance Default AtomicOrdering where
  def = OrderNotAtomic

{#enum CSynchronizationScope as SynchronizationScope {} deriving (Eq) #}

instance Show SynchronizationScope where
  show SSSingleThread = "singlethread"
  show SSCrossThread = ""

instance NFData SynchronizationScope
instance Default SynchronizationScope where
  def = SSCrossThread

{#enum AtomicOperation {} deriving (Eq) #}

instance Show AtomicOperation where
  show AOXchg = "xchg"
  show AOAdd = "add"
  show AOSub = "sub"
  show AOAnd = "and"
  show AONand = "nand"
  show AOOr = "or"
  show AOXor = "xor"
  show AOMax = "max"
  show AOMin = "min"
  show AOUMax = "umax"
  show AOUMin = "umin"

instance NFData AtomicOperation

{#enum LandingPadClause {} deriving (Eq) #}

instance Show LandingPadClause where
  show LPCatch = "catch"
  show LPFilter = "filter"

instance NFData LandingPadClause

{#enum ArithFlags {} deriving (Eq) #}

instance Show ArithFlags where
  show ArithNone = ""
  show ArithNUW = "nuw"
  show ArithNSW = "nsw"
  show ArithBoth = "nuw nsw"

instance NFData ArithFlags
instance Default ArithFlags where
  def = ArithNone


{#enum CmpPredicate {underscoreToCase} deriving (Eq) #}

instance Show CmpPredicate where
  show FCmpFalse = "false"
  show FCmpOeq = "oeq"
  show FCmpOgt = "ogt"
  show FCmpOge = "oge"
  show FCmpOlt = "olt"
  show FCmpOle = "ole"
  show FCmpOne = "one"
  show FCmpOrd = "ord"
  show FCmpUno = "uno"
  show FCmpUeq = "ueq"
  show FCmpUgt = "ugt"
  show FCmpUge = "uge"
  show FCmpUlt = "ult"
  show FCmpUle = "ule"
  show FCmpUne = "une"
  show FCmpTrue = "true"
  show ICmpEq = "eq"
  show ICmpNe = "ne"
  show ICmpUgt = "ugt"
  show ICmpUge = "uge"
  show ICmpUlt = "ult"
  show ICmpUle = "ule"
  show ICmpSgt = "sgt"
  show ICmpSge = "sge"
  show ICmpSlt = "slt"
  show ICmpSle = "sle"

instance NFData CmpPredicate


{#enum CallingConvention {} deriving (Eq) #}
instance Show CallingConvention where
  show CC_C = ""
  show CC_FAST = "fastcc"
  show CC_COLD = "coldcc"
  show CC_GHC = "cc 10"
  show CC_X86_STDCALL = "cc 64"
  show CC_X86_FASTCALL = "cc 65"
  show CC_ARM_APCS = "cc 66"
  show CC_ARM_AAPCS = "cc 67"
  show CC_ARM_AAPCS_VFP = "cc 68"
  show CC_MSP430_INTR = "cc 69"
  show CC_X86_THISCALL = "cc 70"
  show CC_PTX_KERNEL = "cc 71"
  show CC_PTX_DEVICE = "cc 72"
  show CC_MBLAZE_INTR = "cc 73"
  show CC_MBLAZE_SVOL = "cc 74"

instance NFData CallingConvention
instance Default CallingConvention where
  def = CC_C

 -- Representing Assembly
data Assembly = Assembly !Text
                deriving (Eq, Ord)

instance Show Assembly where
  show (Assembly txt) = unpack txt

instance NFData Assembly where
  rnf a@(Assembly txt) = txt `seq` a `seq` ()


-- Param attributes

data ParamAttribute = PAZeroExt
                    | PASignExt
                    | PAInReg
                    | PAByVal
                    | PASRet
                    | PANoAlias
                    | PANoCapture
                    | PANest
                    | PAAlign !Int
                    deriving (Eq, Ord)

instance NFData ParamAttribute

instance Show ParamAttribute where
  show PAZeroExt = "zeroext"
  show PASignExt = "signext"
  show PAInReg = "inreg"
  show PAByVal = "byval"
  show PASRet = "sret"
  show PANoAlias = "noalias"
  show PANoCapture = "nocapture"
  show PANest = "nest"
  show (PAAlign i) = "align " ++ show i

-- Function Attributes

data FunctionAttribute = FAAlignStack !Int
                       | FAAlwaysInline
                       | FAHotPatch
                       | FAInlineHint
                       | FANaked
                       | FANoImplicitFloat
                       | FANoInline
                       | FANoRedZone
                       | FANoReturn
                       | FANoUnwind
                       | FAOptSize
                       | FAReadNone
                       | FAReadOnly
                       | FASSP
                       | FASSPReq
                       deriving (Eq, Ord)

instance NFData FunctionAttribute

instance Show FunctionAttribute where
  show (FAAlignStack n) = "alignstack(" ++ show n ++ ")"
  show FAAlwaysInline = "alwaysinline"
  show FAHotPatch = "hotpatch"
  show FAInlineHint = "inlinehint"
  show FANaked = "naked"
  show FANoImplicitFloat = "noimplicitfloat"
  show FANoInline = "noinline"
  show FANoRedZone = "noredzone"
  show FANoReturn = "noreturn"
  show FANoUnwind = "nounwind"
  show FAOptSize = "optsize"
  show FAReadNone = "readnone"
  show FAReadOnly = "readonly"
  show FASSP = "ssp"
  show FASSPReq = "sspreq"

data TargetTriple = TargetTriple Text
                    deriving (Eq)

instance Show TargetTriple where
  show (TargetTriple t) = unpack t

instance NFData TargetTriple where
  rnf t@(TargetTriple t') = t' `seq` t `seq` ()
