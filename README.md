# elm-uuid-dict

[Dict](https://package.elm-lang.org/packages/elm/core/latest/Dict) and [Set](https://package.elm-lang.org/packages/elm/core/latest/Set) for [TSFoster/elm-uuid](https://github.com/TSFoster/elm-uuid).

## What?

Re-implements [`elm-core`](https://package.elm-lang.org/packages/elm/core/1.0.5/)'s dictionary in order to replace the hardcoded `comparable` key type with an `UUUID`.


## Why?

All the entities in all my projects use UUID as key-column/identifiers. The number of places where I used `UUID.toString` in `Dict`, `Set`, or any de-duplication after two years became scary. And retrieving those types from `Dict.keys` was a pain in the @ss.
