{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.LLVM.Types.Dwarf (
    -- * Types
  module Data.Dwarf,
  DW_TAG(..),
  dw_tag
  ) where

import Data.Dwarf hiding ( DW_TAG(..) )

deriving instance Ord DW_LANG
deriving instance Ord DW_VIRTUALITY
deriving instance Ord DW_ATE
deriving instance Ord DW_TAG

data DW_TAG
    = DW_TAG_array_type
    | DW_TAG_class_type
    | DW_TAG_entry_point
    | DW_TAG_enumeration_type
    | DW_TAG_formal_parameter
    | DW_TAG_imported_declaration
    | DW_TAG_label
    | DW_TAG_lexical_block
    | DW_TAG_member
    | DW_TAG_pointer_type
    | DW_TAG_reference_type
    | DW_TAG_compile_unit
    | DW_TAG_string_type
    | DW_TAG_structure_type
    | DW_TAG_subroutine_type
    | DW_TAG_typedef
    | DW_TAG_union_type
    | DW_TAG_unspecified_parameters
    | DW_TAG_variant
    | DW_TAG_common_block
    | DW_TAG_common_inclusion
    | DW_TAG_inheritance
    | DW_TAG_inlined_subroutine
    | DW_TAG_module
    | DW_TAG_ptr_to_member_type
    | DW_TAG_set_type
    | DW_TAG_subrange_type
    | DW_TAG_with_stmt
    | DW_TAG_access_declaration
    | DW_TAG_base_type
    | DW_TAG_catch_block
    | DW_TAG_const_type
    | DW_TAG_constant
    | DW_TAG_enumerator
    | DW_TAG_file_type
    | DW_TAG_friend
    | DW_TAG_namelist
    | DW_TAG_namelist_item
    | DW_TAG_packed_type
    | DW_TAG_subprogram
    | DW_TAG_template_type_parameter
    | DW_TAG_template_value_parameter
    | DW_TAG_thrown_type
    | DW_TAG_try_block
    | DW_TAG_variant_part
    | DW_TAG_variable
    | DW_TAG_volatile_type
    | DW_TAG_dwarf_procedure
    | DW_TAG_restrict_type
    | DW_TAG_interface_type
    | DW_TAG_namespace
    | DW_TAG_imported_module
    | DW_TAG_unspecified_type
    | DW_TAG_partial_unit
    | DW_TAG_imported_unit
    | DW_TAG_condition
    | DW_TAG_shared_type
       -- Added these since Data.Dwarf doesn't seem to support them
    | DW_TAG_auto_variable
    | DW_TAG_arg_variable
    | DW_TAG_return_variable
    deriving (Show, Eq)

-- | Copied from Data.Dwarf since it is not exported
dw_tag :: (Num a, Show a, Ord a) => a -> Maybe DW_TAG
dw_tag 0x01 = return DW_TAG_array_type
dw_tag 0x02 = return DW_TAG_class_type
dw_tag 0x03 = return DW_TAG_entry_point
dw_tag 0x04 = return DW_TAG_enumeration_type
dw_tag 0x05 = return DW_TAG_formal_parameter
dw_tag 0x08 = return DW_TAG_imported_declaration
dw_tag 0x0a = return DW_TAG_label
dw_tag 0x0b = return DW_TAG_lexical_block
dw_tag 0x0d = return DW_TAG_member
dw_tag 0x0f = return DW_TAG_pointer_type
dw_tag 0x10 = return DW_TAG_reference_type
dw_tag 0x11 = return DW_TAG_compile_unit
dw_tag 0x12 = return DW_TAG_string_type
dw_tag 0x13 = return DW_TAG_structure_type
dw_tag 0x15 = return DW_TAG_subroutine_type
dw_tag 0x16 = return DW_TAG_typedef
dw_tag 0x17 = return DW_TAG_union_type
dw_tag 0x18 = return DW_TAG_unspecified_parameters
dw_tag 0x19 = return DW_TAG_variant
dw_tag 0x1a = return DW_TAG_common_block
dw_tag 0x1b = return DW_TAG_common_inclusion
dw_tag 0x1c = return DW_TAG_inheritance
dw_tag 0x1d = return DW_TAG_inlined_subroutine
dw_tag 0x1e = return DW_TAG_module
dw_tag 0x1f = return DW_TAG_ptr_to_member_type
dw_tag 0x20 = return DW_TAG_set_type
dw_tag 0x21 = return DW_TAG_subrange_type
dw_tag 0x22 = return DW_TAG_with_stmt
dw_tag 0x23 = return DW_TAG_access_declaration
dw_tag 0x24 = return DW_TAG_base_type
dw_tag 0x25 = return DW_TAG_catch_block
dw_tag 0x26 = return DW_TAG_const_type
dw_tag 0x27 = return DW_TAG_constant
dw_tag 0x28 = return DW_TAG_enumerator
dw_tag 0x29 = return DW_TAG_file_type
dw_tag 0x2a = return DW_TAG_friend
dw_tag 0x2b = return DW_TAG_namelist
dw_tag 0x2c = return DW_TAG_namelist_item
dw_tag 0x2d = return DW_TAG_packed_type
dw_tag 0x2e = return DW_TAG_subprogram
dw_tag 0x2f = return DW_TAG_template_type_parameter
dw_tag 0x30 = return DW_TAG_template_value_parameter
dw_tag 0x31 = return DW_TAG_thrown_type
dw_tag 0x32 = return DW_TAG_try_block
dw_tag 0x33 = return DW_TAG_variant_part
dw_tag 0x34 = return DW_TAG_variable
dw_tag 0x35 = return DW_TAG_volatile_type
dw_tag 0x36 = return DW_TAG_dwarf_procedure
dw_tag 0x37 = return DW_TAG_restrict_type
dw_tag 0x38 = return DW_TAG_interface_type
dw_tag 0x39 = return DW_TAG_namespace
dw_tag 0x3a = return DW_TAG_imported_module
dw_tag 0x3b = return DW_TAG_unspecified_type
dw_tag 0x3c = return DW_TAG_partial_unit
dw_tag 0x3d = return DW_TAG_imported_unit
dw_tag 0x3f = return DW_TAG_condition
dw_tag 0x40 = return DW_TAG_shared_type
dw_tag 0x100 = return DW_TAG_auto_variable
dw_tag 0x101 = return DW_TAG_arg_variable
dw_tag 0x102 = return DW_TAG_return_variable
dw_tag n | 0x4080 <= n && n <= 0xffff = Nothing -- error $ "User DW_TAG data requires extension of parser for code " ++ show n
dw_tag _ = Nothing -- error $ "Unrecognized DW_TAG " ++ show n
