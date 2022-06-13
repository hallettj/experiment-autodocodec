# experiment-autodocodec

This is a demo to show how to produce an OpenAPI document from Haskell types
using autodocodec, and in particular how to encode a sum type in OpenAPI.

The relevant code is in [serverc/src/Lib.hs](server/src/Lib.hs).

To emit the OpenAPI document run:

    $ cd server && stack build && stack exec experiment-autodocodec-exe
