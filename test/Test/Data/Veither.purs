module Test.Data.Veither where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, case_, on, inj)
import Data.Veither (Veither(..), genVeitherFrequncy, genVeitherUniform, veither, vfromEither, vfromLeft, vfromLeft', vfromRight, vfromRight', vhandle, vhandleErrors, vhush, vnote, vnote', vsafe)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheckGen)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B, C, checkLaws)
import Test.QuickCheck.Laws.Control as Control
import Test.QuickCheck.Laws.Data as Data
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..), Proxy2(..))

type X r = (x :: Int | r)
type Y r = (y :: Int | r)

type E0 = () :: Row Type
type E1_X = X ()
type E1_Y = Y ()
type E2_XY = X (Y ())

_x = Proxy :: Proxy "x"
_y = Proxy :: Proxy "y"

a = 10 :: Int
x = 20 :: Int
y = 30 :: Int

v0a =                pure a :: Veither E0 Int
v0x =                pure x :: Veither E0 Int
v0y =                pure y :: Veither E0 Int

-- v<number of errors>_<which error rows are included>_<value>

v1_x_a =             pure a :: Veither E1_X Int
v1_x_x = Veither $ inj _x x :: Veither E1_X Int
v1_x_y =             pure y :: Veither E1_X Int

v1_y_a =             pure a :: Veither E1_Y Int
v1_y_x =             pure x :: Veither E1_Y Int
v1_y_y = Veither $ inj _y y :: Veither E1_Y Int

v2_xy_a =             pure a :: Veither E2_XY Int
v2_xy_x = Veither $ inj _x x :: Veither E2_XY Int
v2_xy_y = Veither $ inj _y y :: Veither E2_XY Int

onX :: forall a. (Int -> a) -> Variant E1_X -> a
onX doX =
  case_
    # on _x doX

onXY :: forall a. (Int -> a) -> (Int -> a) -> Variant E2_XY -> a
onXY doX doY =
  case_
    # on _x doX
    # on _y doY

addOne :: Int -> Int
addOne val = val + 1

er10 :: Either Int Int
er10 = Right 10

el20 :: Either Int Int
el20 = Left 20

el30 :: Either Int Int
el30 = Left 30

spec :: Spec Unit
spec = do
  describe "Standard functions" do
    it "veither works" do
      veither case_                    identity v0a `shouldEqual` a
      veither (onX identity) identity v1_x_a `shouldEqual` a
      veither (onX identity) identity v1_x_x `shouldEqual` x
      veither (onXY identity identity) identity v2_xy_a `shouldEqual` a
      veither (onXY identity identity) identity v2_xy_x `shouldEqual` x
      veither (onXY identity identity) identity v2_xy_y `shouldEqual` y
    it "vsafe works" do
      vsafe (pure 1) `shouldEqual` 1
    it "vhandle works" do
      vhandle _x identity v1_x_a `shouldEqual` v0a
      vhandle _x identity v1_x_x `shouldEqual` v0x

      vhandle _y identity (vhandle _x identity v2_xy_a) `shouldEqual` v0a
      vhandle _y identity (vhandle _x identity v2_xy_x) `shouldEqual` v0x
      vhandle _y identity (vhandle _x identity v2_xy_y) `shouldEqual` v0y

      vhandle _x identity (vhandle _y identity v2_xy_a) `shouldEqual` v0a
      vhandle _x identity (vhandle _y identity v2_xy_x) `shouldEqual` v0x
      vhandle _x identity (vhandle _y identity v2_xy_y) `shouldEqual` v0y
    describe "vhandleErrors works" do
      it "reduces one error down to zero" do
        let
          -- h_<all initial rows, including a>_<final rows, including a>
          h_xa_a :: { x :: Int -> Int }
          h_xa_a = { x: identity }

        vhandleErrors h_xa_a v1_x_a `shouldEqual` v0a
        vhandleErrors h_xa_a v1_x_x `shouldEqual` v0x

      it "reduces two errors down to one error" do
        let
          -- h_<all initial rows, including a>_<final rows, including a>
          h_xya_ya :: { x :: Int -> Int }
          h_xya_ya = { x: identity }

        vhandleErrors h_xya_ya v2_xy_a `shouldEqual` v1_y_a
        vhandleErrors h_xya_ya v2_xy_x `shouldEqual` v1_y_x
        vhandleErrors h_xya_ya v2_xy_y `shouldEqual` v1_y_y

        let
          -- h_<all initial rows, including a>_<final rows, including a>
          h_xya_xa :: { y :: Int -> Int }
          h_xya_xa = { y: identity }
        vhandleErrors h_xya_xa v2_xy_a `shouldEqual` v1_x_a
        vhandleErrors h_xya_xa v2_xy_x `shouldEqual` v1_x_x
        vhandleErrors h_xya_xa v2_xy_y `shouldEqual` v1_x_y
      it "reduces two errors down to zero errors" do
        let
          -- h_<all initial rows, including a>_<final rows, including a>
          h_xya_a :: { x :: Int -> Int, y :: Int -> Int }
          h_xya_a = { x: identity, y: identity }
        vhandleErrors h_xya_a v2_xy_a `shouldEqual` v0a
        vhandleErrors h_xya_a v2_xy_x `shouldEqual` v0x
        vhandleErrors h_xya_a v2_xy_y `shouldEqual` v0y
      -- it "attempting to change the '_' label's value won't compile" do
      --   vhandleErrors {"_": \(i :: Int) -> i + 1} v0a `shouldEqual` (map (_ + 1) v0a)
    it "vfromEither works" do
      vfromEither _x er10 `shouldEqual` v1_x_a
      vfromEither _x el20 `shouldEqual` v1_x_x
      vfromEither _y er10 `shouldEqual` v2_xy_a
      vfromEither _y el30 `shouldEqual` v2_xy_y
    it "fromRight works" do
      vfromRight 0 v0a `shouldEqual` a

      vfromRight 0 v1_x_a `shouldEqual` a
      vfromRight 0 v1_x_x `shouldEqual` 0

      vfromRight 0 v2_xy_a `shouldEqual` a
      vfromRight 0 v2_xy_x `shouldEqual` 0
      vfromRight 0 v2_xy_y `shouldEqual` 0
    it "fromRight' works" do
      vfromRight' (\_ -> 0) v0a `shouldEqual` a

      vfromRight' (\_ -> 0) v1_x_a `shouldEqual` a
      vfromRight' (\_ -> 0) v1_x_x `shouldEqual` 0

      vfromRight' (\_ -> 0) v2_xy_a `shouldEqual` a
      vfromRight' (\_ -> 0) v2_xy_x `shouldEqual` 0
      vfromRight' (\_ -> 0) v2_xy_y `shouldEqual` 0
    it "fromLeft works" do
      vfromLeft 0 case_                  v0a `shouldEqual` 0

      vfromLeft 0 (onX (_ + 1))          v1_x_a `shouldEqual` 0
      vfromLeft 0 (onX (_ + 1))          v1_x_x `shouldEqual` (x + 1)

      vfromLeft 0 (onXY (_ + 1) (_ + 2)) v2_xy_a `shouldEqual` 0
      vfromLeft 0 (onXY (_ + 1) (_ + 2)) v2_xy_x `shouldEqual` (x + 1)
      vfromLeft 0 (onXY (_ + 1) (_ + 2)) v2_xy_y `shouldEqual` (y + 2)
    it "fromLeft' works" do
      vfromLeft' (\_ -> 0) case_                  v0a `shouldEqual` 0

      vfromLeft' (\_ -> 0) (onX (_ + 1))          v1_x_a `shouldEqual` 0
      vfromLeft' (\_ -> 0) (onX (_ + 1))          v1_x_x `shouldEqual` (x + 1)

      vfromLeft' (\_ -> 0) (onXY (_ + 1) (_ + 2)) v2_xy_a `shouldEqual` 0
      vfromLeft' (\_ -> 0) (onXY (_ + 1) (_ + 2)) v2_xy_x `shouldEqual` (x + 1)
      vfromLeft' (\_ -> 0) (onXY (_ + 1) (_ + 2)) v2_xy_y `shouldEqual` (y + 2)
    it "vnote works" do
      vnote _x x (Just a) `shouldEqual` v1_x_a
      vnote _x x Nothing `shouldEqual` v1_x_x


      vnote _x x (Just a) `shouldEqual` v2_xy_a
      vnote _x x Nothing `shouldEqual` v2_xy_x

      vnote _y y (Just a) `shouldEqual` v2_xy_a
      vnote _y y Nothing `shouldEqual` v2_xy_y
    it "vnote' works" do
      vnote' _x (\_ -> x) (Just a) `shouldEqual` v1_x_a
      vnote' _x (\_ -> x) Nothing `shouldEqual` v1_x_x


      vnote' _x (\_ -> x) (Just a) `shouldEqual` v2_xy_a
      vnote' _x (\_ -> x) Nothing `shouldEqual` v2_xy_x

      vnote' _y (\_ -> y) (Just a) `shouldEqual` v2_xy_a
      vnote' _y (\_ -> y) Nothing `shouldEqual` v2_xy_y
    it "vhush works" do
      vhush v0a `shouldEqual` Just a

      vhush v1_x_a `shouldEqual` Just a
      vhush v1_x_x `shouldEqual` Nothing

      vhush v2_xy_a `shouldEqual` Just a
      vhush v2_xy_x `shouldEqual` Nothing
      vhush v2_xy_y `shouldEqual` Nothing

checkVeitherTypeClassLaws :: Effect Unit
checkVeitherTypeClassLaws = do
  checkLaws "Veither" do
    Data.checkEq prxVeither
    Data.checkOrd prxVeither
    Data.checkBounded prxVeither
    Data.checkFunctor prx2Veither
    Data.checkFunctorWithIndex prx2Veither
    Data.checkFoldableFunctor prx2Veither
    Control.checkApply prx2Veither
    Control.checkApplicative prx2Veither
    Control.checkAlt prx2Veither
    Control.checkBind prx2Veither
    Control.checkMonad prx2Veither
    Control.checkExtend prx2Veither

prxVeither = Proxy ∷ Proxy (Veither (foo :: A) B)
prx2Veither = Proxy2 ∷ Proxy2 (Veither (bar :: C))

checkGenerators :: Effect Unit
checkGenerators = do
  log "Checking genVeitherUniform"
  quickCheckGen do
    let
      -- compare this style of annotating the record
      -- with the approach used in the below `genVeitherFrequency` test
      r :: { "_" :: Gen Int, x :: Gen Int, y :: Gen Int }
      r = { "_": pure 10, x: pure 20, y: pure 30 }
    v <- genVeitherUniform r
    let result = vsafe (vhandle _x identity (vhandle _y identity v))
    pure case result of
      10 -> true
      20 -> true
      30 -> true
      _ -> false

  log "Checking genVeitherFrequency"
  quickCheckGen do
    v <- genVeitherFrequncy 
      -- the below style shows what we need to do if we don't
      -- define a record like `r` in the `genVeitherUniform` example
      { "_": Tuple 1.0 $ pure 10 :: Tuple Number (Gen Int)
      , x: Tuple 2.0 $ pure 20 :: Tuple Number (Gen Int)
      , y: Tuple 0.0 $ pure 30 :: Tuple Number (Gen Int)
      }
    let result = vsafe (vhandle _x identity (vhandle _y identity v))
    pure case result of
      10 -> true
      20 -> true
      30 -> false -- generator should never run
      _ -> false
