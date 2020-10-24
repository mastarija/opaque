{-# LANGUAGE DeriveGeneric #-}
--
module Data.Opaque where
--
import Data.Text            ( Text )
import Data.Vector          ( Vector )
import Data.ByteString      ( ByteString )
import Data.Scientific      ( Scientific )
import Data.HashMap.Strict  ( HashMap )
--
import GHC.Generics         ( Generic )
--

data Opaque
  = ONull
  | OBool   !OBool
  | OBinary !OBinary
  | ONumber !ONumber
  | OString !OString
  | OVector !OVector
  | ORecord !ORecord
  deriving ( Eq, Show, Generic )

type OBool   = Bool
type OBinary = ByteString
type ONumber = Scientific
type OString = Text
type OVector = Vector Opaque
type ORecord = HashMap OLabel Opaque

type OLabel  = Text
