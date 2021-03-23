module Data.Veither where

import Prelude

import Control.Alt (class Alt)
import Control.Extend (class Extend)
import Data.Array.NonEmpty as NEA
import Data.Either (Either, either)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Foldable (class Foldable)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.List as L
import Data.Maybe (Maybe(..), fromJust, maybe, maybe')
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (class Traversable)
import Data.Variant (Variant, case_, inj, on)
import Data.Variant.Internal (VariantRep(..), impossible)
import Partial.Unsafe (unsafePartial)
import Prim.Row as Row
import Prim.RowList as RL
import Test.QuickCheck (class Arbitrary, class Coarbitrary, arbitrary, coarbitrary)
import Test.QuickCheck.Gen (Gen, oneOf)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | `Veither` is the same as `Either` except that the `l` type can be zero to many different types. 
-- | `Veither` has all the instances that `Either` has, except for `Eq1` and `Ord1`, which simply 
-- | haven't been implemented yet. If you would use a function from `Data.Either` (e.g. hush) and
-- | you want to use the equivalent for `Veither`, add a `v` in front of it (e.g. `vhush`).
-- |
-- | Conceptually, `Veither` has the following definition:
-- |
-- | ```
-- | data Veither l1 l2 ... ln a 
-- |   = Right a
-- |   | L1 l1
-- |   | L2 l2
-- |   | ...
-- |   | LN ln
-- | ```
-- |
-- | `Veither` is monadic via the `a` type parameter. For example, the `Int` type below
-- | represents the 'happy path' and any other errors will short-circuit the computation:
-- |
-- | ```
-- | foo :: Variant (e1 :: Error1, e2 :: Error2) Int
-- | foo = do
-- |   i1 <- returnIntOrFailWithError1
-- |   i2 <- returnIntOrFailWithError2
-- |   pure $ i1 + i2
-- | ````
-- | 
-- | Creating a value of `Veither` can be done in one of two ways, depending on whether 
-- | you want the resulting `Veither` to function like `Either`'s `Right` constructor or like
-- | `Either`'s `Left` constructor:
-- |  - `Either`'s `Right` constructor: use `pure`. For example, `pure 4 :: forall errorRows. Veither errorRows Int`
-- |  - `Either`'s `Left` constructor: use `Data.Variant.inj`. For example,  `Veither (inj (Proxy :: Proxy "foo") String)) :: forall a. Veither (foo :: String) a`
-- |
-- | One can also change an `Either a b` into a `Veither (x :: a) b` using `vfromEither`.
-- | 
-- | To consume a `Veither` value, use `veither`, `vfromRight`, `vfromLeft`, `vnote`, or `vhush`. For example,
-- | one might do the following using `veither`:
-- | 
-- | ```
-- | import Type.Proxy (Proxy(..))
-- | import Data.Variant (case_, on, inj)
-- | 
-- | -- Given a variant value...
-- | val :: Veither (a :: Int, b :: String, c :: Boolean) Number
-- | val = pure 5
-- | 
-- | -- you consume it using the following pattern. You'll need to handle every possible error type
-- | consume :: Veither (a :: Int, b :: String, c :: Boolean) Number -> String
-- | consume v = veither handleError handleSuccess v
-- |   where
-- |   handleError :: Variant (a :: Int, b :: String, c :: Boolean)
-- |   handleError = 
-- |     case_
-- |       # on (Proxy :: Proxy "a") show
-- |       # on (Proxy :: Proxy "b") show
-- |       # on (Proxy :: Proxy "c") show
-- | 
-- |   handleSuccess :: Number -> String
-- |   handleSuccess = show
-- | ```
-- | 
-- | Below are functions that exist in `Veither` but do not exist in `Either`:
-- | - `vsafe` (inspired by `purescript-checked-exceptions`'s `safe` function)
-- | - `vhandle`
-- | - `vfromEither`
newtype Veither ∷ Row Type → Type → Type
newtype Veither errorRows a = Veither (Variant ("_" ∷ a | errorRows))

-- | Proxy type for `Veither`'s happy path (e.g.. `Either`'s `Right` constructor). 
-- | 
-- | Note: the label `"_"` intentionally doesn't match the name of this value (i.e. '_veither').
_veither ∷ Proxy "_"
_veither = Proxy

derive instance newtypeVeither ∷ Newtype (Veither errorRows a) _

instance foldableVeither :: Foldable (Veither errorRows) where
  foldr f z v = veither (const z) (\a -> f a z) v
  foldl f z v = veither (const z) (\a -> f z a) v
  foldMap f v = veither (const mempty) f v

instance traversableVeither :: Traversable (Veither errorRows) where
  traverse :: forall a b m. Applicative m => (a -> m b) -> Veither errorRows a -> m (Veither errorRows b)
  traverse f v = veither (const (pure (coerceR v))) (\a -> pure <$> f a) v
    where
      coerceR ∷ forall a b. Veither errorRows a → Veither errorRows b
      coerceR = unsafeCoerce
  
  sequence :: forall a m. Applicative m => Veither errorRows (m a) -> m (Veither errorRows a)
  sequence v = veither (const (pure (coerceR v))) (\a -> pure <$> a) v
    where
      coerceR ∷ forall a b. Veither errorRows a → Veither errorRows b
      coerceR = unsafeCoerce

instance invariantVeither :: Invariant (Veither errorRows) where
  imap = imapF

instance functorVeither ∷ Functor (Veither errorRows) where
  map ∷ forall a b. (a → b) → Veither errorRows a → Veither errorRows b
  map f (Veither v) = case coerceV v of
      VariantRep v' | v'.type == "_" →
        Veither (inj _veither (f v'.value))
      _ → Veither (coerceR v)
    where
    coerceV ∷ forall a. Variant ("_" ∷ a | errorRows) → VariantRep a
    coerceV = unsafeCoerce

    coerceR ∷ forall a b. Variant ("_" ∷ a | errorRows) → Variant ("_" ∷ b | errorRows)
    coerceR = unsafeCoerce

instance functorWithIndexVeither ∷ FunctorWithIndex Unit (Veither errorRows) where
  mapWithIndex f = map $ f unit

instance applyVeither ∷ Apply (Veither errorRows) where
  apply ∷ forall a b. Veither errorRows (a → b) → Veither errorRows a → Veither errorRows b
  apply (Veither f) va = case coerceVF f of
      VariantRep f'
        | f'.type == "_" → f'.value <$> va
      _ → Veither (coerceR f)
    where
    coerceVF ∷ forall a b. Variant ("_" ∷ (a → b) | errorRows) → VariantRep (a → b)
    coerceVF = unsafeCoerce

    coerceVA ∷ forall a. Variant ("_" ∷ a | errorRows) → VariantRep a
    coerceVA = unsafeCoerce

    coerceR ∷ forall a b. Variant ("_" ∷ a | errorRows) → Variant ("_" ∷ b | errorRows)
    coerceR = unsafeCoerce

instance applicativeVeither ∷ Applicative (Veither errorRows) where
  pure ∷ forall a. a → Veither errorRows a
  pure arg = Veither (inj _veither arg)

instance bindVeither ∷ Bind (Veither errorRows) where
  bind ∷ forall a b. Veither errorRows a → (a → Veither errorRows b) → Veither errorRows b
  bind (Veither a) f = case coerceV a of
    VariantRep a'
      | a'.type == "_" → f a'.value
    _ → Veither $ coerceR a
    where
    coerceV ∷ forall a. Variant ("_" ∷ a | errorRows) → VariantRep a
    coerceV = unsafeCoerce

    coerceR ∷ forall a b. Variant ("_" ∷ a | errorRows) → Variant ("_" ∷ b | errorRows)
    coerceR = unsafeCoerce

instance monadVeither ∷ Monad (Veither errorRows)

instance altVeither :: Alt (Veither errorRows) where
  alt left@(Veither l) right@(Veither r) = case coerceV l, coerceV r of
    VariantRep l', VariantRep r'
      | l'.type /= "_", r'.type == "_" -> right
    _, _ -> left
    where
      coerceV ∷ forall a. Variant ("_" ∷ a | errorRows) → VariantRep a
      coerceV = unsafeCoerce

instance extendVeither :: Extend (Veither errorRows) where
  extend :: forall b a. (Veither errorRows a -> b) -> Veither errorRows a -> Veither errorRows b
  extend f v = map (\_ -> (f v)) v

derive newtype instance showVeither :: Show (Variant ("_" :: a | errorRows)) => Show (Veither errorRows a)

derive newtype instance eqVeither :: Eq (Variant ("_" :: a | errorRows)) => Eq (Veither errorRows a)

-- derive newtype instance eq1Either :: Eq a => Eq1 (Veither errorRows)

derive newtype instance ordVeither :: Ord (Variant ("_" :: a | errorRows)) => Ord (Veither errorRows a)

-- derive newtype instance ord1Either :: Ord a => Ord1 (Veither errorRows a)

derive newtype instance boundedVeither :: Bounded (Variant ("_" :: a | errorRows)) => Bounded (Veither errorRows a)

derive newtype instance enumVeither :: Enum (Variant ("_" :: a | errorRows)) => Enum (Veither errorRows a)

derive newtype instance boundedEnumVeither :: BoundedEnum (Variant ("_" :: a | errorRows)) => BoundedEnum (Veither errorRows a)

instance semigroupVeither :: (Semigroup b) => Semigroup (Veither errorRows b) where
  append x y = append <$> x <*> y

-- | Convert a `Veither` into a value by defining how to handle each possible value.
-- | Below is an example of the typical usage.
-- |
-- | ```
-- | consume :: Veither (a :: Int, b :: String, c :: Boolean) Number -> String
-- | consume v = veither handleError handleSuccess v
-- |   where
-- |   handleError :: Variant (a :: Int, b :: String, c :: Boolean)
-- |   handleError = 
-- |     case_
-- |       # on (Proxy :: Proxy "a") show
-- |       # on (Proxy :: Proxy "b") show
-- |       # on (Proxy :: Proxy "c") show
-- | 
-- |   handleSuccess :: Number -> String
-- |   handleSuccess = show
-- | ```
veither ∷ forall errorRows a b. (Variant errorRows → b) → (a → b) → Veither errorRows a → b
veither handleError handleSuccess (Veither v) = case coerceV v of
  VariantRep a | a.type == "_" → handleSuccess a.value
  _ → handleError (coerceR v)
  where
  coerceV ∷ Variant ("_" ∷ a | errorRows) → VariantRep a
  coerceV = unsafeCoerce

  coerceR ∷ Variant ("_" ∷ a | errorRows) → Variant errorRows
  coerceR = unsafeCoerce

-- | Extract the value out of the `Veither` when there are no other possible values
-- |
-- | ```
-- | vsafe (pure x) == x
-- | ```
vsafe ∷ forall a. Veither () a → a
vsafe (Veither v) = on _veither identity case_ v

-- | Removes one of the possible error types in the `Veither` by converting its value 
-- | to a value of type `a`, the 'happy path' type. This can be useful for gradually 
-- | picking off some of the errors the `Veither` value could have by handling only
-- | some of them at a given point in your code.
-- |
-- | If the number of errors in your `Veither` are small and can all be handled via `vhandle`, 
-- | one can use `vsafe` to extract the value of the 'happy path' `a` type.
-- |
-- | ```
-- | foo :: Veither (b :: Int) String
-- | foo = pure "2"
-- |
-- | _b :: Proxy "b"
-- | _b = Proxy
-- |
-- | bar :: Veither (b :: Int) String
-- | bar = Veither (inj_ _b 3)
-- |
-- | vhandle _b show bar == ((pure "3") :: Veither () String)
-- | vhandle _b show foo == ((pure "2") :: Veither () String)
-- |
-- | safe (vhandle _b show bar) == "3"
-- | safe (vhandle _b show foo) == "2"
-- | ````
vhandle ∷ forall sym b otherErrorRows errorRows a
  .  IsSymbol sym
  => Row.Cons sym b otherErrorRows errorRows
  => Proxy sym -> (b -> a) -> Veither errorRows a → Veither otherErrorRows a
vhandle proxy f variant@(Veither v) = case coerceV v of
  VariantRep b | b.type == reflectSymbol proxy → pure $ f b.value
  _ → coerceVeither variant
  where
  coerceV ∷ Variant ("_" ∷ a | errorRows) → VariantRep b
  coerceV = unsafeCoerce

  coerceVeither ∷ Veither errorRows a → Veither otherErrorRows a
  coerceVeither = unsafeCoerce

-- | Convert an `Either` into a `Veither`.
-- |
-- | ```
-- | p :: Proxy "foo"
-- | p = Proxy
-- |
-- | vfromEither p (Left Int)  :: forall a. Variant (foo :: Int) a
-- | vfromEither p (Right Int) :: forall a. Variant (foo :: a  ) Int
-- | ```
vfromEither ∷ forall sym otherRows errorRows a b
  .  IsSymbol sym 
  => Row.Cons sym a otherRows ("_" :: b | errorRows) 
  => Proxy sym -> Either a b -> Veither errorRows b
vfromEither proxy = either (\e -> Veither (inj proxy e)) (\a -> Veither (inj _veither a))

-- | Extract the value from a `Veither`, using a default value in case the underlying
-- | `Variant` is storing one of the error rows' values.
-- |
-- | ```
-- | vError :: Veither (foo :: Int) String
-- | vError = Veither (inj (Proxy :: Proxy "foo") 4)
-- |
-- | vSuccess :: Veither (foo :: Int) String
-- | vSuccess = pure "yay"
-- |
-- | vfromRight "" vError   == ""
-- | vfromRight "" vSuccess == "yay"
-- | ```
vfromRight ∷ forall errorRows a. a → Veither errorRows a → a
vfromRight default (Veither v) = case coerceV v of
  VariantRep a | a.type == "_" → a.value
  _ → default
  where
  coerceV ∷ Variant ("_" ∷ a | errorRows) → VariantRep a
  coerceV = unsafeCoerce

-- | Same as `vfromRight` but the default value is lazy.
vfromRight' ∷ forall errorRows a. (Unit → a) → Veither errorRows a → a
vfromRight' default (Veither v) = case coerceV v of
  VariantRep a | a.type == "_" → a.value
  _ → default unit
  where
  coerceV ∷ Variant ("_" ∷ a | errorRows) → VariantRep a
  coerceV = unsafeCoerce

-- | Extract the error value from a `Veither`, using a default value in case the underlying
-- | `Variant` is storing the `("_" :: a)` rows' values.
-- |
-- | ```
-- | vError :: Veither (foo :: Int) String
-- | vError = Veither (inj (Proxy :: Proxy "foo") 4)
-- |
-- | vSuccess :: Veither (foo :: Int) String
-- | vSuccess = pure "yay"
-- |
-- | vfromLeft  8 (case_ # on (Proxy :: Proxy "foo") identity) vError   == 4
-- | vfromRight 8 (case_ # on (Proxy :: Proxy "foo") identity) vSuccess == 8
-- | ```
vfromLeft ∷ forall errorRows a b. b → (Variant errorRows → b) → Veither errorRows a → b
vfromLeft default handleFailures (Veither v) =
  on _veither (const default) handleFailures v

-- | Same as `vfromLeft` but the default value is lazy.
vfromLeft' ∷ forall errorRows a b. (Unit → b) → (Variant errorRows → b) → Veither errorRows a → b
vfromLeft' default handleFailures (Veither v) =
  on _veither (\_ → default unit) handleFailures v

-- | Convert a `Maybe` value into a `Veither` value using a default value when the `Maybe` value is `Nothing`.
-- |
-- | ```
-- | mJust :: Maybe String
-- | mJust = Just "x"
-- |
-- | mNothing :: Maybe String
-- | mNothing = Nothing
-- |
-- | _foo :: Proxy "foo"
-- | _foo = Proxy
-- |
-- | vnote _foo 2 mJust    == (pure "y")             :: Veither (foo :: Int) String
-- | vnote _foo 2 mNothing == (Veither (inj _foo 2)) :: Veither (foo :: Int) String
-- | ```
vnote ∷ forall otherErrorRows errorRows s a b
   . Row.Cons s a otherErrorRows ("_" ∷ b | errorRows)
  => IsSymbol s
  => Proxy s → a → Maybe b → Veither errorRows b
vnote proxy a may = Veither (maybe (inj proxy a) (\b → inj _veither b) may)

-- | Same as `vnote` but the default value is lazy.
vnote' ∷ forall otherErrorRows errorRows s a b
   . Row.Cons s a otherErrorRows ("_" ∷ b | errorRows)
  => IsSymbol s
  => Proxy s → (Unit → a) → Maybe b → Veither errorRows b
vnote' proxy f may = Veither (maybe' (inj proxy <<< f) (\b → inj _veither b) may)

-- | Convert a `Veither` value into a `Maybe` value.
vhush ∷ forall errorRows a. Veither errorRows a → Maybe a
vhush = veither (const Nothing) Just

instance arbitraryVeither :: (
  RL.RowToList ("_" :: a | errorRows) rowList,
  VariantArbitrarys ("_" :: a | errorRows) rowList
  ) => Arbitrary (Veither errorRows a) where
  arbitrary = do
    let
      -- Create the list of generators
      vaList :: L.List (Gen (Variant ("_" :: a | errorRows)))
      vaList = variantArbitrarys (Proxy :: Proxy ("_" :: a | errorRows)) (Proxy :: Proxy rowList)

      -- This is guaranteed to be non-empty because there will always be a ("_" :: a) row
      vaNEA = unsafePartial $ fromJust $ NEA.fromFoldable vaList

    -- Choose one of the rows' generators and use it to generate a
    -- `Variant` whose rows fit the `Veither` rows
    randomVariant <- oneOf vaNEA
    pure $ Veither randomVariant

-- | Creates a list where each generator within the list will produce a Variant
-- | for one of the rows in `("_" :: a | errorRows)`
class VariantArbitrarys :: Row Type -> RL.RowList Type -> Constraint
class VariantArbitrarys finalRow currentRL where
  variantArbitrarys :: Proxy finalRow -> Proxy currentRL -> L.List (Gen (Variant finalRow))

instance variantArbitrarysNil ∷ VariantArbitrarys ignore RL.Nil where
  variantArbitrarys _ _ = L.Nil

instance variantArbitrarysCons ∷ (
  IsSymbol sym, 
  VariantArbitrarys final rlTail, 
  Row.Cons sym a rowTail final,
  Arbitrary a
  ) ⇒ VariantArbitrarys final (RL.Cons sym a rlTail) where
  variantArbitrarys _ _ = do
    let 
      va :: Gen (Variant final)
      va = do
        a <- arbitrary :: Gen a
        let
          v :: Variant final
          v = inj (Proxy :: Proxy sym) a
        pure v
    
    L.Cons va (variantArbitrarys (Proxy :: Proxy final) (Proxy ∷ Proxy rlTail))

foreign import data UnknownVariantValue :: Type 

instance coarbitraryVeither :: (
  RL.RowToList ("_" :: a | errorRows) rl,
  VariantCoarbitrarys rl
  ) => Coarbitrary (Veither errorRows a) where
  coarbitrary :: forall r. Veither errorRows a -> Gen r -> Gen r
  coarbitrary (Veither v) = case coerceV v of
    VariantRep a -> variantCoarbitrarys (Proxy :: Proxy rl) a
    where
      coerceV ∷ Variant ("_" ∷ a | errorRows) → VariantRep UnknownVariantValue
      coerceV = unsafeCoerce

class VariantCoarbitrarys :: RL.RowList Type -> Constraint
class VariantCoarbitrarys currentRL where
  variantCoarbitrarys :: forall r. Proxy currentRL -> { type :: String, value :: UnknownVariantValue } -> (Gen r -> Gen r)

instance variantCoarbitrarysNil :: VariantCoarbitrarys RL.Nil where
  variantCoarbitrarys _ _ = impossible "coarbtirary"

instance variantCoarbitrarysCons :: (
  IsSymbol sym, 
  Coarbitrary a, 
  VariantCoarbitrarys tail) => VariantCoarbitrarys (RL.Cons sym a tail) where
  variantCoarbitrarys _ a = 
    if a.type == reflectSymbol (Proxy :: Proxy sym) 
      then coarbitrary (coerceA a.value)
      else variantCoarbitrarys (Proxy :: Proxy tail) a
    where
      coerceA ∷ UnknownVariantValue -> a
      coerceA = unsafeCoerce