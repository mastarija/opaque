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

--

show00 :: Opaque
show00 = OVector
  [ OVoid
  , ONull
  , OString "hello"
  ]

show01 :: Opaque
show01 = ORecord $ Map.fromList
  [ ( "f1" , OVoid )
  , ( "f2" , ONull )
  , ( "f3" , OString "hello" )
  , ( "f4" , show00 )
  ]

show02 :: Opaque
show02 = ORecord $ Map.fromList
  [ ( "f1" , OVoid )
  , ( "f2" , ONull )
  , ( "f3" , show00 )
  , ( "f4" , show01 )
  ]

show03 :: Opaque
show03 = OVector
  [ OVoid
  , ONull
  , OString "hello"
  , show01
  , show02
  ]

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

      pad l = ( replicate ( 2 * l ) ' ' <> )

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

data Test00
  deriving ( Generic , EncOpaque )

data Test01 = Test01_1
  deriving ( Generic , EncOpaque )

data Test02 = Test02_1 | Test02_2
  deriving ( Generic , EncOpaque )

data Test03 = Test03_1 Test01 Test02 Test01 Test02 Test01
  deriving ( Generic , EncOpaque )

data Test04 = Test04_1 | Test04_2 Test01 Test02
  deriving ( Generic , EncOpaque )

data Test05 = Test05_1 | Test05_2 Test01 Test02 Test03 Test04
  deriving ( Generic , EncOpaque )

data Test06 = Test06
  { t06_f1 :: Test01
  , t06_f2 :: Test02
  , t06_f3 :: Test03
  , t06_f4 :: Test04
  , t06_f5 :: Test05
  } deriving ( Generic , EncOpaque )

data Test07
  = Test07_1 Test03
  | Test07_2 Test03 Test04
  | Test07_3
    { t07_f1 :: Test04
    , t07_f2 :: Test05
    , t07_f3 :: Test06
    }
  deriving ( Generic , EncOpaque )

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
-- instead of throwing error we have special 'OVoid' value
instance EncOpaque' V1 where
  encOpaque' _ = OVoid

-- general constructor instance
instance {-# OVERLAPPABLE #-} ( Constructor c , EncOpaque' v ) => EncOpaque' ( C1 c v ) where
  encOpaque' c@( M1 v ) = ORecord $ Map.fromList
    [ ( "con" , OString $ conName c )
    , ( "val" , encOpaque' v )
    ]

-- instance for a simple constructor with no fields
instance ( Constructor c ) => EncOpaque' ( C1 c U1 ) where
  encOpaque' c = ORecord $ Map.fromList
    [ ( "con" , OString $ conName c )
    ]

-- selector instance
instance ( Selector s , EncOpaque' v ) => EncOpaque' ( S1 s v ) where
  encOpaque' s@( M1 v ) = case selName s of
    ""    -> encOpaque' v -- simple constructor
    label -> ORecord $ Map.fromList -- record with fields
      [ ( label , encOpaque' v )
      ]

-- vague recursive instance
instance EncOpaque v => EncOpaque' ( Rec0 v ) where
  encOpaque' = encOpaque . unK1

-- vague sum instance
instance ( EncOpaque' f , EncOpaque' g ) => EncOpaque' ( f :+: g ) where
  encOpaque' = \case
    L1 f -> encOpaque' f
    R1 g -> encOpaque' g

-- vague product instance
instance ( EncOpaque' f , EncOpaque' g ) => EncOpaque' ( f :*: g ) where
  encOpaque' ( f :*: g ) = OVector $ aux f <> aux g
    where aux v = case encOpaque' v of
            OVector r -> r
            r         -> [ r ]
