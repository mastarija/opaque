{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
--
module Data.Opaque where
--
import Data.ByteString      ( ByteString )
import Data.Map             ( Map , insert )
import Data.Aeson           ( ToJSON , encode )
--
import GHC.Generics
  ( Generic (..) , Constructor (..) , Selector (..)
  , Rec0 , M1 (..) , K1 (..) , D1 , C1 , S1 , U1
  , Meta (..)
  , (:+:) (..)
  , (:*:) (..)
  )
--

type OLabel  = String

type OBool   = Bool
type OBinary = ByteString
type ONumber = Double
type OString = String
type OVector = [ Opaque ]
type ORecord = Map OLabel Opaque

data Opaque
  = ONull
  | OBool   !OBool
  | OBinary !OBinary
  | ONumber !ONumber
  | OString !OString
  | OVector !OVector
  | ORecord !ORecord
  deriving ( Eq , Show , Generic )

data Test0 = Test0
  { field01 :: Bool
  , field02 :: String
  , field03 :: Int
  } deriving ( Show , Generic , ToJSON , EncodeOpaque )

test0 :: Test0
test0 = Test0
  { field01 = False
  , field02 = "Hello"
  , field03 = 1
  }

data Test1 = Test1V1 | Test1V2 | Test1V3 Int
  deriving ( Show , Generic , ToJSON , EncodeOpaque )

test10 :: Test1
test10 = Test1V1

test11 :: Test1
test11 = Test1V3 2

class EncodeOpaque v where
  encodeOpaque :: v -> Opaque
  default
    encodeOpaque :: ( Generic v , EncodeOpaque' ( Rep v ) ) => v -> Opaque
  encodeOpaque = encodeOpaque' . from

instance EncodeOpaque Bool where
  encodeOpaque = OBool

instance EncodeOpaque String where
  encodeOpaque = OString

instance EncodeOpaque Int where
  encodeOpaque = ONumber . fromIntegral

--

class EncodeOpaque' f where
  encodeOpaque' :: f p -> Opaque

instance EncodeOpaque' f => EncodeOpaque' ( D1 m f ) where
  encodeOpaque' = encodeOpaque' . unM1

instance {-# OVERLAPPABLE #-} EncodeOpaque' f => EncodeOpaque' ( C1 m f ) where
  encodeOpaque' = encodeOpaque' . unM1

instance {-# OVERLAPPABLE #-}
  ( Constructor m , EncodeOpaque' f ) =>
  EncodeOpaque' ( C1 m ( S1 ( 'MetaSel 'Nothing x y z ) f ) ) where
  encodeOpaque' m@( M1 ( M1 v ) )
    = ORecord
    $ insert "tag" ( OString $ conName m )
    $ insert "val" ( encodeOpaque' v )
    $ mempty

instance {-# OVERLAPPING #-} Constructor m => EncodeOpaque' ( C1 m U1 ) where
  encodeOpaque' m = ORecord $ insert "tag" ( OString $ conName m ) mempty

instance EncodeOpaque c => EncodeOpaque' ( Rec0 c ) where
  encodeOpaque' = encodeOpaque . unK1

instance ( Selector m , EncodeOpaque' f ) => EncodeOpaque' ( S1 m f ) where
  encodeOpaque' s@( M1 v )
    = ORecord
    $ insert ( selName s ) ( encodeOpaque' v ) mempty

instance ( Selector m , EncodeOpaque' f , EncodeOpaque' g ) => EncodeOpaque' ( ( S1 m f ) :*: g ) where
  encodeOpaque' ( s1@( M1 v1 ) :*: g )
    = ORecord
    $ insert ( selName s1 ) ( encodeOpaque' v1 )
    $ case encodeOpaque' g of
      ORecord v2 -> v2
      _          -> mempty

instance ( EncodeOpaque' f , EncodeOpaque' g ) => EncodeOpaque' ( f :+: g ) where
  encodeOpaque' ( L1 f ) = encodeOpaque' f
  encodeOpaque' ( R1 g ) = encodeOpaque' g