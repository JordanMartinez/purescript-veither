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

## How? By hard-coding one of the rows in `Variant` to work like `Right` and leaving the others to work like `Left`

[`Variant`](https://github.com/natefaubion/purescript-variant) is the extensible version of `Either`, but one cannot use it in a monadic way. So, what if you took `Variant` and provided a newtype around it that hard-coded one of the rows to something that functions like `Either`'s `Right` constructor (e.g. `("_" :: a)`) and then made it extensible, so that the other rows function like an extensible `Left` constructor (e.g. `("_" :: a | errorRows)`)? You get `Veither`, a `Variant`-based version of `Either`. The other benefit is that we no longer have multiple layers of nesting (e.g. `Either a (Either b (Either c ...))`).

```purescript
newtype Veither errorRows a = Veither (Variant ("_" :: a | errorRows))
--
-- `=~=` means 'isomorphic to'
--
-- Veither () a =~= a
-- Veither (foo :: Int) a == Either Int a
-- Veither (foo :: Int, bar :: String) a == Either Int (Either String a)
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
  -- if you would use `fromRight` (or some other
  -- `Either` function) for `Either`, 
  -- add a 'v' in front of it and you have the
  -- corresponding function name for `Veither`
  log $ vfromRight handleFailure foo 
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