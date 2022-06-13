# experiment-autodocodec

This is a demo to show how to produce an OpenAPI document from Haskell types
using autodocodec, and in particular how to encode a sum type in OpenAPI.

Relevant Haskell code is in [serverc/src/Lib.hs](server/src/Lib.hs).

Generated OpenAPI document is in [openapi.json](openapi.json).

Generated TypeScript type definitions are in
[client-typescript/src-generated/models](client-typescript/src-generated/models)

To emit an updated OpenAPI document run:

    $ make openapi.json

To emit updated OpenAPI *and* generated client code run:

    $ make

## Notes on TypeScript generation

TypeScript types are generated using
[openapi-typescript-codegen](https://www.npmjs.com/package/openapi-typescript-codegen).
When working with sum types be sure to use the `--useUnionTypes` option to the
`openapi` CLI tool to generate literal types for your discriminator fields.
