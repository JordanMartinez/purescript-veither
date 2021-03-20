module Data.Veither where

import Prelude

import Control.Alt (class Alt)
import Control.Extend (class Extend)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.Maybe (Maybe(..), maybe, maybe')
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, inj, on)
import Data.Variant.Internal (VariantRep(..))
import Prim.Row as Row
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype Veither ∷ Row Type → Type → Type
newtype Veither errorRows a = Veither (Variant ("_" ∷ a | errorRows))

_veither ∷ Proxy "_"
_veither = Proxy

derive instance newtypeVeither ∷ Newtype (Veither errorRows a) _

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
        | f'.type == "_", a'.type == "_" →
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
  extend f v = vfromRight (coerceR v) (\_ -> Veither $ inj _veither (f v)) v
    where
    coerceR ∷ forall a b. Veither errorRows a → Veither errorRows b
    coerceR = unsafeCoerce

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

vfromRight ∷ forall errorRows a b. b → (a → b) → Veither errorRows a → b
vfromRight default handleSuccess (Veither v) = case coerceV v of
  VariantRep a | a.type == "_" → handleSuccess a.value
  _ → default
  where
  coerceV ∷ Variant ("_" ∷ a | errorRows) → VariantRep a
  coerceV = unsafeCoerce

vfromRight' ∷ forall errorRows a b. (Unit → b) → (a → b) → Veither errorRows a → b
vfromRight' default handleSuccess (Veither v) = case coerceV v of
  VariantRep a | a.type == "_" → handleSuccess a.value
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
