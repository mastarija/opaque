{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Map ( Map )
import Data.Bool ( bool )
import Data.Word ( Word8 )
import Data.List ( intersperse )
--
import qualified Data.Map as Map
--
import GHC.Generics
  ( Generic (..)
  , Meta (..)
  , M1 (..)
  , D1
  , V1
  , U1
  , K1 (..)
  , Rec0
  , C1 , Constructor (..)
  , S1 , Selector (..)
  , (:+:)(..)
  , (:*:)(..)
  )
--

type OLabel  = String

type OBool   = Bool
type OBinary = [ Word8 ]
type ONumber = Double
type OString = String
type OVector = [ Opaque ]
type ORecord = Map OLabel Opaque

data Opaque
  = OVoid
  | ONull
  | OBool   !OBool
  | OBinary !OBinary
  | ONumber !ONumber
  | OString !OString
  | OVector !OVector
  | ORecord !ORecord
  deriving ( Eq , Generic )

--

instance Show Opaque where
  show = aux 0
    where
      aux l = \case
        OVoid     -> "OVoid"
        ONull     -> "ONull"
        OBool   b -> show b
        OBinary _ -> "<010>"
        ONumber n -> show n
        OString s -> show s
        OVector v -> vec l v
        ORecord r -> rec l r

      pad l = ( replicate ( 4 * l ) ' ' <> )

      vec l vs = concat
        [ pad l "[ "
        , concat
        $ intersperse ( '\n' : pad l ", " )
        $ fmap
          ( \ v -> concat
            [ val v
            , " : "
            , bool "" "\n" ( cmp v )
            , aux ( bool 0 ( succ l ) ( cmp v ) ) v 
            ]
          )
          vs
        , "\n" <> pad l "]"
        ]

      rec l fs = concat
        [ pad l "{ "
        , concat
        $ intersperse ( '\n' : pad l ", " )
        $ fmap
          ( \ ( f , v ) -> concat
            [ f
            , " : "
            , val v
            , " = "
            , bool "" "\n" ( cmp v )
            , aux ( bool 0 ( succ l ) ( cmp v ) ) v
            ]
          )
          ( Map.toList fs )
        , "\n" <> pad l "}"
        ]

      val = \case
        OVoid     -> "OVoid"
        ONull     -> "ONull"
        OBool   _ -> "OBool"
        OBinary _ -> "OBinary"
        ONumber _ -> "ONumber"
        OString _ -> "OString"
        OVector _ -> "OVector"
        ORecord _ -> "ORecord"

      cmp = \case
        OVector _ -> True
        ORecord _ -> True
        _         -> False

--

class EncOpaque a where
  encOpaque :: a -> Opaque
  default encOpaque :: ( Generic a , EncOpaque' ( Rep a ) ) => a -> Opaque
  encOpaque = encOpaque' . from

class EncOpaque' f where
  encOpaque' :: f p -> Opaque

--

-- datatype entry point
instance EncOpaque' v => EncOpaque' ( D1 m v ) where
  encOpaque' = encOpaque' . unM1

-- instance for uninhabited types
instance EncOpaque' V1 where
  encOpaque' _ = OVoid

-- instance for unit types
instance EncOpaque' U1 where
  encOpaque' _ = ONull

-- recursive instance
instance EncOpaque v => EncOpaque' ( Rec0 v ) where
  encOpaque' = encOpaque . unK1

-- constructor with no fields
instance ( Constructor ( 'MetaCons s n 'False ) )
  => EncOpaque' ( C1 ( 'MetaCons s n 'False ) U1 ) where
  encOpaque' c = ORecord $ Map.fromList
    [ ( "con" , OString $ conName c )
    ]

-- constructor with a single field
instance ( Selector s , Constructor ( 'MetaCons c n 'False ) , EncOpaque v )
  => EncOpaque' ( C1 ( 'MetaCons c n 'False ) ( S1 s ( Rec0 v ) ) ) where
  encOpaque' c@( M1 s ) = ORecord $ Map.fromList
    [ ( "con" , OString $ conName c )
    , ( "val" , encOpaque' s )
    ]

-- constructor with multiple fields
instance ( Constructor ( 'MetaCons s n 'False ) , EncOpaque' ( f :*: g ) )
  => EncOpaque' ( C1 ( 'MetaCons s n 'False ) ( f :*: g ) ) where
  encOpaque' c@( M1 fg ) = ORecord $ Map.fromList
    [ ( "con" , OString $ conName c )
    , ( "val" , encOpaque' fg )
    ]

-- record with single field
instance ( Selector s , EncOpaque' v )
  => EncOpaque' ( C1 ( 'MetaCons c n 'True ) ( S1 s v ) ) where
  encOpaque' ( M1 ( s@( M1 v ) ) ) = ORecord $ Map.fromList
    [ ( selName s , encOpaque' v )
    ]

-- record with multiple fields
instance EncOpaque' ( f :*: g ) => EncOpaque' ( C1 ( 'MetaCons s n 'True ) ( f :*: g ) ) where
  encOpaque' ( M1 fg ) = encOpaque' fg 

-- selector instance
instance ( Selector s , EncOpaque' v ) => EncOpaque' ( S1 s v ) where
  encOpaque' s@( M1 v ) = case selName s of
    ""    -> case encOpaque' v of -- simple constructor
      OVector v' -> OVector v'
      v'         -> OVector [ v' ]
    label -> ORecord $ Map.fromList -- record with fields
      [ ( label , encOpaque' v )
      ]

-- sum instance
instance ( EncOpaque' f , EncOpaque' g ) => EncOpaque' ( f :+: g ) where
  encOpaque' = \case
    L1 f -> encOpaque' f
    R1 g -> encOpaque' g

-- smart (?) product instance
-- most of these won't happen
-- this avoids writing errors
instance ( EncOpaque' f , EncOpaque' g ) => EncOpaque' ( f :*: g ) where
  encOpaque' ( f :*: g ) = case ( encOpaque' f , encOpaque' g ) of
    ( OVector vsl , OVector vsr ) -> OVector $ vsl <> vsr
    ( ORecord rsl , ORecord rsr ) -> ORecord $ rsl <> rsr
    ( OVector vsl , ORecord rsr ) -> OVector $ vsl <> Map.elems rsr
    ( ORecord rsl , OVector vsr ) -> OVector $ Map.elems rsl <> vsr
    ( OVector vsl , v           ) -> OVector $ vsl <> [ v ]
    ( v           , OVector vsr ) -> OVector $ v : vsr
    ( vl          , vr          ) -> OVector $ [ vl , vr ]

--

class DecOpaque a where
  decOpaque :: Opaque -> Maybe a

class DecOpaque' a where
  decOpaque' :: Opaque -> Maybe ( Rep a )