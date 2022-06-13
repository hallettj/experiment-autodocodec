gen_openapi_exe := $(shell cd server && echo "$$(stack path --local-install-root)/bin/generate-openapi")
server_src_files := $(shell find server/ -not -path '*/.*')
openapi_typescript_exe := client-typescript/node_modules/.bin/openapi

.PHONY: all
all: openapi.json client-typescript/src-generated/index.ts

# Generate OpenAPI document from Haskell source code

$(gen_openapi_exe): $(server_src_files)
	cd server && stack build

openapi.json: $(gen_openapi_exe)
	$(gen_openapi_exe) > $@

# TypeScript client

client-typescript/src-generated/index.ts: openapi.json $(openapi_typescript_exe)
	$(openapi_typescript_exe) --input openapi.json --output client-typescript/src-generated --useUnionTypes

$(openapi_typescript_exe): client-typescript/package-lock.json
	cd client-typescript && npm ci
