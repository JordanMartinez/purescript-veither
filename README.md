# purescript-veither

## Why? Because `Either` forces you to use one `error` type

`Either` is defined (and often used) like so:
```purescript
data Either errorType valueType
    = Left errorType
    | Right valueType
```

However, it's not extensible. The `errorType` must always be the same. This can be annoying when one is using `Either` in a monadic way:

```purescript
foo :: Either SameErrorType
foo = do
  a <- stringOrFailWithErrorType1
  b <- stringOrFailWithErrorType2 -- uh-oh! Compiler error!
  pure $ doSomethingWith a b
```

One way around this is to use a nested `Either` type (e.g. [`Either3](https://pursuit.purescript.org/packages/purescript-either/5.0.0/docs/Data.Either.Nested#t:Either3)), but this comes at the cost of extra layers of "boxing:" 

```purescript
import Data.Either (Either(..))
import Data.Either.Nested (Either5)
import Data.Either.Inject (inj)

-- Syntactic sugar to help here...
foo :: Either5 a b c d e
foo = inj d

-- but it comes down to this
bar :: Either a (Either b (Either c (Either d e)))
bar = Right (Right (Right (Left d)))
```

One could reduce the amount of boxing by using [`Variant`](https://pursuit.purescript.org/packages/purescript-variant/7.0.1/docs/Data.Variant#t:Variant). However, `Variant` does not have a `Monad` instance, so one cannot use the "do notation" in the original example above. 

This library enables one to use all the type classes of `Either` (except `Eq1` and `Ord1`, which haven't yet been implemented) but with the extensible error type capaility of `Variant`.

## How? By hard-coding one of the rows in `Variant` to work like `Right` and leaving the others to work like `Left`

What if you took `Variant` and provided a newtype around it that hard-coded one of the rows to something that functions like `Either`'s `Right` constructor (e.g. `("_" :: a)`) and then made it extensible, so that the other rows function like an extensible `Left` constructor (e.g. `("_" :: a | errorRows)`)? You get `Veither`, a `Variant`-based version of `Either`.

```purescript
newtype Veither errorRows a = Veither (Variant ("_" :: a | errorRows))
--
-- `=~=` means 'isomorphic to'
--
-- Veither () a =~= a
-- Veither (foo :: Int) a =~= Either Int a
-- Veither (foo :: Int, bar :: String) a =~= Either Int (Either String a)
```

Now we can write our monadic `Either` code without the 'same error type' restriction:

```purescript
type PossibleErrors = (a :: ErrorType1, b :: ErrorType2)

foo :: Veither PossibleErrors String
foo = do
  a <- stringOrFailWithErrorType1
  b <- stringOrFailWithErrorType2 -- not a problem!
  pure $ doSomethingWith a b

main :: Effect Unit
main = do
  -- if you would use `either` (or some other
  -- `Either` function) for `Either`, 
  -- add a 'v' in front of it and you have the
  -- corresponding function name for `Veither`
  log $ veither handleFailure identity foo 
  where
  handleFailure :: Variant PossibleErrors -> String
  handleFailure =
    case_
      # on _a (const "error type 1 happened")
      # on _b (const "error type 2 happened")
```

## FAQs

### Why use "_" as the label of `a`?

While it likely doesn't do much, I thought it would lessen the impact on the runtime checking since checking whether this Variant's type matches another occurs frequently. I also didn't want to restrict the possible names one could use in the error types.
