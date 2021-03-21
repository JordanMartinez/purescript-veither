module Data.Veither where

import Prelude

import Control.Alt (class Alt)
import Control.Extend (class Extend)
import Data.Array.NonEmpty as NEA
import Data.Enum (class BoundedEnum, class Enum)
import Data.Foldable (class Foldable)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.List as L
import Data.Maybe (Maybe(..), fromJust, maybe, maybe')
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol)
import Data.Traversable (class Traversable)
import Data.Variant (Variant, inj, on)
import Data.Variant.Internal (VariantRep(..))
import Partial.Unsafe (unsafePartial)
import Prim.Row as Row
import Prim.RowList as RL
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, elements)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype Veither ∷ Row Type → Type → Type
newtype Veither errorRows a = Veither (Variant ("_" ∷ a | errorRows))

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
  map f (Veither v) = Veither case coerceV v of
      VariantRep v' | v'.type == "_" →
        inj _veither (f v'.value)
      _ → coerceR v
    where
    coerceV ∷ forall a. Variant ("_" ∷ a | errorRows) → VariantRep a
    coerceV = unsafeCoerce

    coerceR ∷ forall a b. Variant ("_" ∷ a | errorRows) → Variant ("_" ∷ b | errorRows)
    coerceR = unsafeCoerce

instance applyVeither ∷ Apply (Veither errorRows) where
  apply ∷ forall a b. Veither errorRows (a → b) → Veither errorRows a → Veither errorRows b
  apply (Veither f) (Veither a) = Veither case coerceVF f, coerceVA a of
      VariantRep f', VariantRep a'
        | f'.type == "_" && a'.type == "_" →
          inj _veither (f'.value a'.value)
      _, _ → coerceR f
    where
    coerceVF ∷ forall a b. Variant ("_" ∷ (a → b) | errorRows) → VariantRep (a → b)
    coerceVF = unsafeCoerce

    coerceVA ∷ forall a. Variant ("_" ∷ a | errorRows) → VariantRep a
    coerceVA = unsafeCoerce

    coerceR ∷ forall a b. Variant ("_" ∷ a | errorRows) → Variant ("_" ∷ b | errorRows)
    coerceR = unsafeCoerce

instance applicativeVeither ∷ Applicative (Veither errorRows) where
  pure ∷ forall a. a → Veither errorRows a
  pure arg = Veither $ inj _veither arg

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

veither ∷ forall errorRows a b. (Variant errorRows → b) → (a → b) → Veither errorRows a → b
veither handleError handleSuccess (Veither v) = case coerceV v of
  VariantRep a | a.type == "_" → handleSuccess a.value
  _ → handleError (coerceR v)
  where
  coerceV ∷ Variant ("_" ∷ a | errorRows) → VariantRep a
  coerceV = unsafeCoerce

  coerceR ∷ Variant ("_" ∷ a | errorRows) → Variant errorRows
  coerceR = unsafeCoerce

vfromRight ∷ forall errorRows a. a → Veither errorRows a → a
vfromRight default (Veither v) = case coerceV v of
  VariantRep a | a.type == "_" → a.value
  _ → default
  where
  coerceV ∷ Variant ("_" ∷ a | errorRows) → VariantRep a
  coerceV = unsafeCoerce

vfromRight' ∷ forall errorRows a. (Unit → a) → Veither errorRows a → a
vfromRight' default (Veither v) = case coerceV v of
  VariantRep a | a.type == "_" → a.value
  _ → default unit
  where
  coerceV ∷ Variant ("_" ∷ a | errorRows) → VariantRep a
  coerceV = unsafeCoerce

vfromLeft ∷ forall errorRows a b. b → (Variant errorRows → b) → Veither errorRows a → b
vfromLeft default handleFailures (Veither v) =
  on _veither (const default) handleFailures v

vfromLeft' ∷ forall errorRows a b. (Unit → b) → (Variant errorRows → b) → Veither errorRows a → b
vfromLeft' default handleFailures (Veither v) =
  on _veither (\_ → default unit) handleFailures v

vnote ∷ forall otherErrorRows errorRows s a b
   . Row.Cons s a otherErrorRows ("_" ∷ b | errorRows)
  => IsSymbol s
  => Proxy s → a → Maybe b → Veither errorRows b
vnote proxy a may = Veither (maybe (inj proxy a) (\b → inj _veither b) may)

vnote' ∷ forall otherErrorRows errorRows s a b
   . Row.Cons s a otherErrorRows ("_" ∷ b | errorRows)
  => IsSymbol s
  => Proxy s → (Unit → a) → Maybe b → Veither errorRows b
vnote' proxy f may = Veither (maybe' (inj proxy <<< f) (\b → inj _veither b) may)

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

    -- Choose one of the rows' generators
    variantGenerator <- elements vaNEA
    -- use it to generate a Variant whose rows fit the Veither rows
    randomVariant <- variantGenerator
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