module Data.Text

import public Data.Text.Internal
import Data.String

export
Show Text where
    show = show . getString

{-
======
String
======
-}

||| O(n) convert a `Text` to a `String`
||| if `Text` hasn't been reduced (eg by uncons) then O(1), more specifically if `off` = 0 and `len` = `length str`
export
toString : Text -> String
toString = getString

export
FromString Text where
    fromString = makeText

{-
=============
Querying Text
=============
-}

||| O(1) get the length of `Text`
export
length : Text -> Int
length text = text.len

||| O(1) get the `Char` at `ind`
export
index : Text -> (ind : Nat) -> Maybe Char
index str ind = unsafeIndex str (cast ind)

||| O(n) Check all `Char`s satisfy a predicate
||| Short-circuits on first `Char` that doesn't satisfy predicate 
export
all : (Char -> Bool) -> Text -> Bool
all p (MkText{off, len, str}) = loop off len
  where
    loop : Int -> Int -> Bool
    loop offset left = if left <= 0
        then True
        else p (assert_total $ prim__strIndex str offset) && loop (offset + 1) (left - 1)

||| O(n) Check any `Char` satisfies a predicate
||| Short-circuits on first `Char` that satisfies predicate 
export
any : (Char -> Bool) -> Text -> Bool
any p (MkText{off, len, str}) = loop off len
  where
    loop : Int -> Int -> Bool
    loop offset left = if left <= 0
        then False
        else p (assert_total $ prim__strIndex str offset) || loop (offset + 1) (left - 1)

||| O(n) count from start number of `Char`s that satisfy `pred`
||| stopping at first `Char` that doesn't satisfy `pred`
export
firstNAre : (pred : Char -> Bool) -> Text -> Nat
firstNAre pred (MkText{off, len, str}) = loop off len 0
  where
    loop : Int -> Int -> Nat -> Nat
    loop offset left acc = if left <= 0
        then acc
        else if pred (assert_total $ prim__strIndex str offset)
            then loop (offset + 1) (left - 1) (acc + 1)
            else acc

||| O(n) count from end number of `Char`s that satisfy `pred`
||| stopping at first `Char` that doesn't satisfy `pred`
export
lastNAre : (pred : Char -> Bool) -> Text -> Nat
lastNAre pred (MkText{off, len, str}) = loop len 0
  where
    loop : Int -> Nat -> Nat
    loop left acc = if left <= 0
        then acc
        else if pred (assert_total $ prim__strIndex str (off + left))
            then loop (left - 1) (acc + 1)
            else acc

{-
==============
Consuming Text
==============
-}

||| O(1) get the first `Char` and the rest of the `Text`
export
uncons : Text -> Maybe (Char, Text)
uncons (MkText{off, len, str}) =
    if len <= 0
        then Nothing
        else Just
            ( assert_total $ prim__strIndex str off
            , MkText{off = off + 1, len = len - 1, str}
            )

||| O(1) get the last `Char` and the rest of the `Text`
export
unsnoc : Text -> Maybe (Text, Char)
unsnoc (MkText{off, len, str}) =
    if len <= 0
        then Nothing
        else assert_total $ Just
            ( MkText{off = off, len = len - 1, str}
            , prim__strIndex str (off + len - 1)
            )

||| fold a `Text` with right precedence
export
foldr : (Char -> acc -> acc) -> acc -> Text -> acc
foldr f z (MkText{off, len, str}) = loop off len
  where
    loop : Int -> Int -> acc
    loop offset left = if left <= 0
        then z
        else f (assert_total $ prim__strIndex str offset) (loop (offset + 1) (left - 1))

||| fold a `Text` with left precedence with an accumulator
export
foldl : (acc -> Char -> acc) -> acc -> Text -> acc
foldl f z (MkText{off, len, str}) = loop off len z
  where
    loop : Int -> Int -> acc -> acc
    loop offset left acc = if left <= 0
        then acc
        else loop (offset + 1) (left - 1) (f acc (assert_total $ prim__strIndex str offset))

{-
=======
subtext
=======
-}

||| O(1) take the first `length` `Char`s
export
take : (length : Nat) -> Text -> Text
take n (MkText{off, len, str}) = let n' = min len (cast n) in MkText{off = off, len = n', str}

||| O(1) take the last `length` `Char`s
export
takeEnd : (length : Nat) -> Text -> Text
takeEnd n (MkText{off, len, str}) = let n' = min len (cast n) in MkText{off = off + len - n', len = n', str}

||| O(1) drop the first `length` `Char`s
export
drop : (length : Nat) -> Text -> Text
drop n (MkText{off, len, str}) = let n' = min len (cast n) in MkText{off = off + n', len = len - n', str}

||| O(1) drop the last `length` `Char`s
export
dropEnd : (length : Nat) -> Text -> Text
dropEnd n (MkText{off, len, str}) = let n' = min len (cast n) in MkText{off, len = len - n', str}

||| O(n) take the first `Char`s that satisfy `pred`
export
takeWhile : (pred : Char -> Bool) -> Text -> Text
takeWhile pred t = take (firstNAre pred t) t

||| O(n) return the first `Char`s that satisfy `pred`
||| or `Nothing` if no `Char`s satisfy
export
takeWhile1 : (pred : Char -> Bool) -> Text -> Maybe Text
takeWhile1 pred t = let n = firstNAre pred t in if n < 1
    then Nothing
    else Just $ take n t

||| O(n) take the last `Char`s that satisfy `pred`
export
takeWhileEnd : (pred : Char -> Bool) -> Text -> Text
takeWhileEnd pred t = takeEnd (lastNAre pred t) t

||| O(n) return the last `Char`s that satisfy `pred`
||| or `Nothing` if no `Char`s satisfy
export
takeWhile1End : (pred : Char -> Bool) -> Text -> Maybe Text
takeWhile1End pred t = let n = lastNAre pred t in if n < 1
    then Nothing
    else Just $ takeEnd n t

||| O(n) drop the first `Char`s that satisfy `pred`
||| up to first `Char` that doesn't satisfy `pred`
export
dropWhile : (pred : Char -> Bool) -> Text -> Text
dropWhile pred t = drop (firstNAre pred t) t

||| O(n) drop the last `Char`s that satisfy `pred`
||| up to first `Char` that doesn't satisfy `pred`
export
dropWhileEnd : (pred : Char -> Bool) -> Text -> Text
dropWhileEnd pred t = dropEnd (lastNAre pred t) t

||| O(1) return a slice of `Text`
export
slice : (offset : Nat) -> (length : Nat) -> Text -> Text
slice offset length = take length . drop offset

{-
=============
Building Text
=============

See Data.Text.Builder for a more efficient interface
-}

||| O(1) return `Text` containing single `Char`
export
singleton : Char -> Text
singleton char = fromString $ prim__strCons char ""

||| O(n) Add a `Char` to the front of a `Text`
export
cons : Char -> Text -> Text
cons char = makeText . prim__strCons char . getString

||| O(n) Add a `Char` to the end of a `Text`
export
snoc : Text -> Char -> Text
snoc text char = text <+> singleton char

||| O(n) Combine a list of `Text`s into one `Text`
||| uses fastConcat for single allocation
concat : List Text -> Text
concat = makeText . fastConcat . map getString