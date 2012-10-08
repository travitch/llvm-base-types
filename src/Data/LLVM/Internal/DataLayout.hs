module Data.LLVM.Internal.DataLayout (
  Endianness(..),
  AlignmentPref(..),
  DataLayout(..),
  defaultDataLayout,
  parseDataLayout
  ) where

import Data.Text ( Text )
import qualified Data.Text as T
import Data.List ( foldl' )

data Endianness = EndianBig
                | EndianLittle
                deriving (Eq, Ord, Show)

data AlignmentPref = AlignmentPref { alignmentPrefSize :: !Int
                                   , alignmentPrefABI :: !Int
                                   , alignmentPrefPref :: !Int
                                   }
                   deriving (Eq, Ord, Show)

data DataLayout = DataLayout { targetEndianness :: !Endianness
                             , targetStackAlignment :: Maybe Int
                             , targetIntegerWidths :: [Int]
                             , targetPointerPrefs :: AlignmentPref
                             , targetIntegerPrefs :: [AlignmentPref]
                             , targetVectorPrefs :: [AlignmentPref]
                             , targetFloatPrefs :: [AlignmentPref]
                             , targetAggregatePrefs :: [AlignmentPref]
                             , targetStackObjectPrefs :: [AlignmentPref]
                             }
                deriving (Eq, Ord, Show)

defaultDataLayout :: DataLayout
defaultDataLayout = DataLayout { targetEndianness = EndianBig
                               , targetStackAlignment = Nothing
                               , targetIntegerWidths = []
                               , targetPointerPrefs = AlignmentPref 64 64 64
                               , targetIntegerPrefs = [ AlignmentPref 1 8 8
                                                      , AlignmentPref 8 8 8
                                                      , AlignmentPref 16 16 16
                                                      , AlignmentPref 32 32 32
                                                      , AlignmentPref 64 32 64
                                                      ]
                               , targetVectorPrefs = [ AlignmentPref 64 64 64
                                                     , AlignmentPref 128 128 128
                                                     ]
                               , targetFloatPrefs = [ AlignmentPref 32 32 32
                                                    , AlignmentPref 64 64 64
                                                    ]
                               , targetAggregatePrefs = [ AlignmentPref 0 0 1 ]
                               , targetStackObjectPrefs = [ AlignmentPref 0 64 64 ]
                               }

parseDataLayout :: Text -> Either String DataLayout
parseDataLayout dl = foldr parseModifier (Right defaultDataLayout) modifiers
  where
    modifiers = T.split (=='-') dl

-- | Split a string on a separator
splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn sep s = reverse (reverse leftover : parts)
  where
    (leftover, parts) = foldl' doSplit ([], []) s
    doSplit (cur, acc) c =
      case c == sep of
        True -> ([], reverse cur : acc)
        False -> (c : cur, acc)

parseAlignPref :: String -> Maybe AlignmentPref
parseAlignPref s =
  case splitOn ':' s of
    [ssz, sabi, spref] ->
      case (parseInt ssz, parseInt sabi, parseInt spref) of
        (Just sz, Just abi, Just pref) -> Just $! AlignmentPref sz abi pref
        _ -> Nothing
    _ -> Nothing

mergePreference :: AlignmentPref -> [AlignmentPref] -> [AlignmentPref]
mergePreference newP oldPs = newP : ps
  where
    ps = filter (\p -> alignmentPrefSize p /= alignmentPrefSize newP) oldPs

parseInt :: String -> Maybe Int
parseInt s =
  case reads s of
    [(i, "")] -> Just i
    _ -> Nothing

parseModifier :: Text -> Either String DataLayout -> Either String DataLayout
parseModifier _ acc@(Left _) = acc
parseModifier m (Right acc) =
  case T.unpack m of
    ['E'] -> Right acc { targetEndianness = EndianBig }
    ['e'] -> Right acc { targetEndianness = EndianLittle }
    'p' : ':' : rest ->
      case splitOn ':' rest of
        [ssz, sabi, spref] ->
          case (parseInt ssz, parseInt sabi, parseInt spref) of
            (Just sz, Just abi, Just pref) -> Right acc { targetPointerPrefs = AlignmentPref sz abi pref }
            _ -> Left ("Could not parse pointer alignments: " ++ show rest)
        [ssz, sabi] ->
          case (parseInt ssz, parseInt sabi) of
            (Just sz, Just abi) -> Right acc { targetPointerPrefs = AlignmentPref sz abi abi }
            _ -> Left ("Could not parse pointer alignments: " ++ show rest)
        _ -> Left ("Could not parse pointer alignments: " ++ show rest)
    'i' : rest ->
      case parseAlignPref rest of
        Nothing -> Left ("Invalid integer alignment preference: " ++ rest)
        Just p -> Right acc { targetIntegerPrefs = mergePreference p (targetIntegerPrefs acc) }
    'v' : rest ->
      case parseAlignPref rest of
        Nothing -> Left ("Invalid vector alignment preference: " ++ rest)
        Just p -> Right acc { targetVectorPrefs = mergePreference p (targetVectorPrefs acc) }
    'f' : rest ->
      case parseAlignPref rest of
        Nothing -> Left ("Invalid float alignment preference: " ++ rest)
        Just p -> Right acc { targetFloatPrefs = mergePreference p (targetFloatPrefs acc) }
    'a' : rest ->
      case parseAlignPref rest of
        Nothing -> Left ("Invalid aggregate aggregate preference: " ++ rest)
        Just p -> Right acc { targetAggregatePrefs = mergePreference p (targetAggregatePrefs acc) }
    's' : rest ->
      case parseAlignPref rest of
        Nothing -> Left ("Invalid stack object alignment preference: " ++ rest)
        Just p -> Right acc { targetStackObjectPrefs = mergePreference p (targetStackObjectPrefs acc) }
    'S' : rest ->
      case reads rest of
        [(align, "")] -> Right acc { targetStackAlignment = Just align }
        _ -> Left ("Expected stack alignment: " ++ rest)
    'n' : rest ->
      case mapM parseInt (splitOn ':' rest) of
        Nothing -> Left ("Expected int list: " ++ rest)
        Just is -> Right acc { targetIntegerWidths = is }
    _ -> Left ("Unexpected data layout specifier: " ++ T.unpack m)
