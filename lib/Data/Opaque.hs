{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
--
module Data.Opaque where
--
import Data.Text            ( Text )
import Data.Vector          ( Vector )
import Data.ByteString      ( ByteString )
import Data.HashMap.Strict  ( HashMap )
--
import GHC.Generics         ( Generic (..) , Datatype (..) , Constructor (..) , M1 (..) , D1 , C1 , (:+:) (..) )
--

type OLabel  = Text

type OBool   = Bool
type OBinary = ByteString
type ONumber = Double
type OString = String
type OVector = [ Opaque ]
type ORecord = HashMap OLabel Opaque

data Opaque
  = ONull
  | OBool   !OBool
  | OBinary !OBinary
  | ONumber !ONumber
  | OString !OString
  | OVector !OVector
  | ORecord !ORecord
  deriving ( Eq , Show , Generic )

class EncodeOpaque v where
  encodeOpaque :: v -> Opaque
  default
    encodeOpaque :: ( Generic v , EncodeOpaque' ( Rep v ) ) => v -> Opaque
  encodeOpaque = encodeOpaque' . from

class EncodeOpaque' f where
  encodeOpaque' :: f p -> Opaque

instance EncodeOpaque' f => EncodeOpaque' ( D1 c f ) where
  encodeOpaque' ( M1 v ) = encodeOpaque' v

instance Constructor c => EncodeOpaque' ( C1 c f ) where
  encodeOpaque' m@( M1 _ ) = OString $ conName m

instance ( EncodeOpaque' f , EncodeOpaque' g ) => EncodeOpaque' ( f :+: g ) where
  encodeOpaque' ( L1 v ) = encodeOpaque' v
  encodeOpaque' ( R1 v ) = encodeOpaque' v
