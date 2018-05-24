module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Kazunoko as K
import Record.Format (format)
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)

-- inferred:
result1 :: SProxy "five"
result1 =
  K.add
    (SProxy :: SProxy "two")
    (SProxy :: SProxy "three")

-- inferred:
result2 :: SProxy "six"
result2 =
  K.subtract
    (SProxy :: SProxy "nine")
    (SProxy :: SProxy "three")

letsAdd
  :: forall l r o
   . K.Add l r o
  => IsSymbol l
  => IsSymbol r
  => IsSymbol o
  => SProxy l
  -> SProxy r
  -> Effect Unit
letsAdd l r = log s
  where
    o = K.add l r
    s = format (SProxy :: SProxy "{l} plus {r} is {o}")
      { l: reflectSymbol l
      , r: reflectSymbol r
      , o: reflectSymbol o
      }

letsSubtract
  :: forall l r o
   . K.Sub l r o
  => IsSymbol l
  => IsSymbol r
  => IsSymbol o
  => SProxy l
  -> SProxy r
  -> Effect Unit
letsSubtract l r = log s
  where
    o = K.subtract l r
    s = format (SProxy :: SProxy "{l} minus {r} is {o}")
      { l: reflectSymbol l
      , r: reflectSymbol r
      , o: reflectSymbol o
      }

main :: Effect Unit
main = do
  log "we can do some arithmetic!"

  letsAdd (SProxy :: SProxy "two") (SProxy :: SProxy "three")

  -- correctly errors: i don't know how to count...
  -- letsAdd (SProxy :: SProxy "three") (SProxy :: SProxy "eight")

  letsSubtract (SProxy :: SProxy "five") (SProxy :: SProxy "three")

  -- correctly errors: i don't know how to count...
  -- letsSubtract (SProxy :: SProxy "two") (SProxy :: SProxy "three")

  -- result
  -- we can do some arithmetic!
  -- two plus three is five
  -- five minus three is two
