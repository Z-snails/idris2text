module Data.Text.Builder

import Data.Text.Internal
import Data.String

||| Constant used by operation like `<+>`
||| Maximum length of `Text` for `Text`s to be merged
-- Note: this hasn't yet been optimised
mergeLength : Int
mergeLength = 256

||| Useful for efficiently building `Text`s
export
data Builder : Type where
    Nil : Builder
    Cons : Text -> Builder -> Builder

Semigroup Builder where
    Nil <+> b2 = Nil
    b1 <+> Nil = b1
    Cons last Nil <+> Cons first tail =
        if last.len < mergeLength && first.len < mergeLength
            then Cons (last <+> first) tail
            else Cons last (Cons first tail)
    Cons head tail <+> t = Cons head (tail <+> t)

||| O(1) convert a `Text` to a `Builder`
export
fromText : Text -> Builder
fromText t = Cons t Nil

||| O(n) convert a `Builder` to a `String`
export
toString : Builder -> String
toString = fastConcat . toStringList
    where
        toStringList : Builder -> List String
        toStringList Nil = []
        toStringList (Cons head tail) = getString head :: toStringList tail

||| O(n) convert a `Builder` to a `Text`
export
toText : Builder -> Text
toText = makeText . toString

||| O(1) single `Char`
export
singleton : Char -> Builder
singleton = fromText . makeText . singleton

FromString Builder where
    fromString = fromText . makeText