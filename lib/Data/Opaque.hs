{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
--
module Data.Opaque where
--
import Data.Map         ( Map , toList , fromList )
import Data.Word        ( Word8 )
import Data.List        ( intercalate )
import Numeric.Natural  ( Natural )
--
import GHC.Generics
  ( Generic (..)
  , Selector (..)
  , Constructor (..)
  , Meta (..)
  , M1 (..) , D1 , D , C1 , C , S1 , S
  , V1
  , U1 (..)
  , K1 (..) , Rec0 , R
  , (:+:) (..)
  , (:*:) (..)
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
  = ONull
  | OBool   !OBool
  | OBinary !OBinary
  | ONumber !ONumber
  | OString !OString
  | OVector !OVector
  | ORecord !ORecord
  deriving ( Eq , Generic )

instance Show Opaque where
  show = aux 0
    where aux :: Natural -> Opaque -> String
          aux l = \case
            ONull     -> "Null"
            OBool b   -> show b
            OBinary _ -> "..."
            ONumber n -> show n
            OString s -> show s
            OVector v -> "[..]"
            ORecord r -> concat
              [ indent l "{ "
              , intercalate ( "\n" <> ( indent l ", " ) ) $ fmap ( rec l ) $ toList r
              , "\n" <> ( indent l "}" )
              ]

          tab :: String
          tab = "  "

          rec :: Natural -> ( OLabel , Opaque ) -> String
          rec l ( label , value ) = show label <> " = " <> isRec value <> aux ( l + 1 ) value
            where isRec ( ORecord _ ) = "\n"
                  isRec _ = ""

          vec :: Natural -> Opaque -> String
          vec l value = undefined

          indent :: Natural -> String -> String
          indent 0 s = s
          indent n s = tab <> indent ( n - 1 ) s

--

ex1 :: Opaque
ex1 = ORecord $ fromList
  [ ( "field01" , ONull )
  , ( "field02" , ORecord $ fromList
      [ ( "test01" , OBool True )
      , ( "test02" , ONumber 12 )
      , ( "test03" , ORecord $ fromList
          [ ( "test01" , OString "hello world!" )
          , ( "test02" , ONumber 12 )
          ]
        )
      ]
    )
  ]

data Test00 = Test00
  deriving ( Show , Generic , EncOpaque )

data Test01 = Test01_0 | Test01_1
  deriving ( Show , Generic , EncOpaque )

data Test02 = Test02_0 | Test02_1 | Test02_3 Test00 | Test02_4 Test00 Test01
  deriving ( Show , Generic , EncOpaque )

data Test03 = Test03
  { t03Field00 :: Test00
  , t03Field01 :: Test01
  , t03Field02 :: Test02
  }

data Test04
  = Test04_0
  | Test04_1 Test03
  | Test04_2
    { t04Field00 :: Test00
    , t04Field01 :: Test01
    , t04Field02 :: Test02
    , t04Field03 :: Test03
    }

--

class EncOpaque a where
  encOpaque :: a -> Opaque
  default encOpaque :: ( Generic a , EncOpaque' ( Rep a ) ) => a -> Opaque
  encOpaque = encOpaque' . from

class EncOpaque' f where
  encOpaque' :: f p -> Opaque

--

instance EncOpaque' V1 where
  encOpaque' = const ONull

instance EncOpaque' U1 where
  encOpaque' = const ONull

instance EncOpaque v => EncOpaque' ( Rec0 v ) where
  encOpaque' = encOpaque . unK1

instance ( EncOpaque' f ) => EncOpaque' ( D1 d f ) where
  encOpaque' = encOpaque' . unM1

instance {-# OVERLAPPABLE #-} EncOpaque' f => EncOpaque' ( C1 c f ) where
  encOpaque' = encOpaque' . unM1

instance ( Constructor c ) => EncOpaque' ( C1 c U1 ) where
  encOpaque' c = ORecord $ fromList
    [ ( "con" , OString $ conName c )
    ]

instance ( Constructor c , EncOpaque' f ) => EncOpaque' ( C1 c ( S1 s f ) ) where
  encOpaque' c@( M1 ( M1 v ) ) = ORecord $ fromList
    [ ( "con" , OString $ conName c )
    , ( "val" , encOpaque' v )
    ]

instance ( s ~ 'MetaSel 'Nothing x y z , EncOpaque' f , EncOpaque' g ) => EncOpaque' ( ( S1 s f ) :*: g ) where
  encOpaque' ( ( M1 f ) :*: g ) = encOpaque' f `aux` encOpaque' g
    where aux :: Opaque -> Opaque -> Opaque
          aux v1 ( OVector v2 ) = OVector $ v1 : v2
          aux v1 v2             = OVector $ v1 : [ v2 ]

instance ( s ~ 'MetaSel 'Nothing x y z , EncOpaque' f ) => EncOpaque' ( S1 s f ) where
  encOpaque' = encOpaque' . unM1

instance ( EncOpaque' f , EncOpaque' g ) => EncOpaque' ( f :+: g ) where
  encOpaque' ( L1 v ) = encOpaque' v
  encOpaque' ( R1 v ) = encOpaque' v

