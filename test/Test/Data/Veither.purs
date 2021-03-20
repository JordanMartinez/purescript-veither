module Test.Data.Veither where

import Prelude

import Control.Monad.Gen (chooseInt)
import Data.Array (all)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant, case_, on, inj)
import Data.Veither (Veither(..), veither, vfromLeft, vfromLeft', vfromRight, vfromRight', vhush, vnote, vnote')
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Test.QuickCheck (quickCheckGen)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

type X r = (x :: Int | r)
type Y r = (y :: Int | r)

type E0 = () :: Row Type
type E1 = X ()
type E2 = X (Y ())

_x = Proxy :: Proxy "x"
_y = Proxy :: Proxy "y"

a = 10 :: Int
x = 20 :: Int
y = 30 :: Int

v0a =             pure a :: Veither E0 Int

v1a =             pure a :: Veither E1 Int
v1x = Veither $ inj _x x :: Veither E1 Int

v2a =             pure a :: Veither E2 Int
v2x = Veither $ inj _x x :: Veither E2 Int
v2y = Veither $ inj _y y :: Veither E2 Int

onX :: forall a. (Int -> a) -> Variant E1 -> a
onX doX = 
  case_ 
    # on _x doX

onXY :: forall a. (Int -> a) -> (Int -> a) -> Variant E2 -> a
onXY doX doY = 
  case_ 
    # on _x doX 
    # on _y doY

addOne :: Int -> Int
addOne val = val + 1

spec :: forall m. MonadEffect m => SpecT Aff Unit m Unit
spec = do
  describe "type class instance tests" do
    liftEffect $ quickCheckGen do
      i <- chooseInt bottom top
      e <- chooseInt bottom top
      let
        vi :: Veither (foo :: Int) Int
        vi = pure i

        ve :: Veither (foo :: Int) Int
        ve = pure e

      pure $ all identity
        [ vi == vi
        , ve == ve
        , vi /= ve
        , ve /= vi
        , (compare i e) == (compare vi ve)
        , (compare e i) == (compare ve vi)
        ]
  describe "Standard functions" do
    it "veither works" do
      veither case_                    identity v0a `shouldEqual` a
      veither (onX identity) identity v1a `shouldEqual` a
      veither (onX identity) identity v1x `shouldEqual` x
      veither (onXY identity identity) identity v2a `shouldEqual` a
      veither (onXY identity identity) identity v2x `shouldEqual` x
      veither (onXY identity identity) identity v2y `shouldEqual` y
    it "fromRight works" do
      vfromRight 0 v0a `shouldEqual` a

      vfromRight 0 v1a `shouldEqual` a
      vfromRight 0 v1x `shouldEqual` 0
      
      vfromRight 0 v2a `shouldEqual` a
      vfromRight 0 v2x `shouldEqual` 0
      vfromRight 0 v2y `shouldEqual` 0
    it "fromRight' works" do
      vfromRight' (\_ -> 0) v0a `shouldEqual` a

      vfromRight' (\_ -> 0) v1a `shouldEqual` a
      vfromRight' (\_ -> 0) v1x `shouldEqual` 0
      
      vfromRight' (\_ -> 0) v2a `shouldEqual` a
      vfromRight' (\_ -> 0) v2x `shouldEqual` 0
      vfromRight' (\_ -> 0) v2y `shouldEqual` 0
    it "fromLeft works" do
      vfromLeft 0 case_                  v0a `shouldEqual` 0
      
      vfromLeft 0 (onX (_ + 1))          v1a `shouldEqual` 0
      vfromLeft 0 (onX (_ + 1))          v1x `shouldEqual` (x + 1)
      
      vfromLeft 0 (onXY (_ + 1) (_ + 2)) v2a `shouldEqual` 0
      vfromLeft 0 (onXY (_ + 1) (_ + 2)) v2x `shouldEqual` (x + 1)
      vfromLeft 0 (onXY (_ + 1) (_ + 2)) v2y `shouldEqual` (y + 2)
    it "fromLeft' works" do
      vfromLeft' (\_ -> 0) case_                  v0a `shouldEqual` 0
      
      vfromLeft' (\_ -> 0) (onX (_ + 1))          v1a `shouldEqual` 0
      vfromLeft' (\_ -> 0) (onX (_ + 1))          v1x `shouldEqual` (x + 1)
      
      vfromLeft' (\_ -> 0) (onXY (_ + 1) (_ + 2)) v2a `shouldEqual` 0
      vfromLeft' (\_ -> 0) (onXY (_ + 1) (_ + 2)) v2x `shouldEqual` (x + 1)
      vfromLeft' (\_ -> 0) (onXY (_ + 1) (_ + 2)) v2y `shouldEqual` (y + 2)
    it "vnote works" do
      vnote _x x (Just a) `shouldEqual` v1a
      vnote _x x Nothing `shouldEqual` v1x


      vnote _x x (Just a) `shouldEqual` v2a
      vnote _x x Nothing `shouldEqual` v2x
      
      vnote _y y (Just a) `shouldEqual` v2a
      vnote _y y Nothing `shouldEqual` v2y
    it "vnote' works" do
      vnote' _x (\_ -> x) (Just a) `shouldEqual` v1a
      vnote' _x (\_ -> x) Nothing `shouldEqual` v1x


      vnote' _x (\_ -> x) (Just a) `shouldEqual` v2a
      vnote' _x (\_ -> x) Nothing `shouldEqual` v2x

      vnote' _y (\_ -> y) (Just a) `shouldEqual` v2a
      vnote' _y (\_ -> y) Nothing `shouldEqual` v2y
    it "vhush works" do
      vhush v0a `shouldEqual` Just a

      vhush v1a `shouldEqual` Just a
      vhush v1x `shouldEqual` Nothing

      vhush v2a `shouldEqual` Just a
      vhush v2x `shouldEqual` Nothing
      vhush v2y `shouldEqual` Nothing
