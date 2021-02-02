# idris2text
Wrapper around primitive String, with additional functionality

Inspired by Haskell's text package

## Performance
`Text` is never asymtopically slower than `String` however it may in some circumstances be slower
(eg if you reduce a `Text` then build it up again, as it must perform 2 allocations)

## Builder
`Builder` is designed for faster construction of `Text` or `String` by using a linked list of small chunks of `Text`

## Internals
Internally `Text` stores an offset into the `String`, the length of the `Text` and the primitive `String`

`String` uses UTF-32 encoding, which allows for `O(1)` indexing
