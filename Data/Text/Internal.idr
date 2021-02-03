module Data.Text.Internal

-- it is assumed that off and len will always be in range
||| Wrapper around String with better asymtopics (eg prim__strUncons is O(n))
public export
record Text where
    constructor MkText
    off : Int -- offset from start of 'str' (0 indexed)
    len : Int -- length of 'Text'
    str : String -- UTF-32 primitive String

||| convert a Text to a String (eg for IO output)
export
getString : Text -> String
getString (MkText{off, len, str}) = if off <= 0 && len == cast (length str)
    then str
    else prim__strSubstr off len str

||| convert a String to a Text (eg for IO input)
export
makeText : String -> Text
makeText str = MkText
    { off = 0
    , len = prim__strLength str
    , str
    }

||| O(1) get the character at the (zero based) index 'index' of 'Text'
||| unsafe because it doesn't check if ind < 0
export
unsafeIndex : Text -> (index : Int) -> Maybe Char
unsafeIndex (MkText{off, len, str}) ind =
    if ind >= len
        then Nothing
        else Just (assert_total $ prim__strIndex str (off + ind))

||| O(n) copy a 'Text' unlinking it from the orignal potentially
||| larger (in memory) 'Text'
||| use this if the Text will stay in memory for a while to reduce memory use
export
copy : Text -> Text
copy = makeText . getString

export
Semigroup Text where
    t1 <+> t2 = makeText $ getString t1 <+> getString t2

export
Eq Text where
    t1 == t2 = t1.len == t2.len && loop 0 t1.len
      where
        loop : Int -> Int -> Bool
        loop offset left = if left <= 0
            then True
            else unsafeIndex t1 offset == unsafeIndex t2 offset && loop (offset + 1) (left - 1)

export
Ord Text where
    compare t1 t2 = loop 0
      where
        loop : Int -> Ordering
        loop ind = case (unsafeIndex t1 ind, unsafeIndex t2 ind) of
            (Nothing, Nothing) => EQ
            (Just _, Nothing) => GT
            (Nothing, Just _) => LT
            (Just c1, Just c2) => case compare c1 c2 of
                EQ => loop (ind + 1)
                ord => ord