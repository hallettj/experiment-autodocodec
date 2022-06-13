gen_openapi_exe := $(shell cd server && echo "$$(stack path --local-install-root)/bin/generate-openapi")
server_src_files := $(shell find server/)

.PHONY: all
all: openapi.json

$(gen_openapi_exe): $(server_src_files)
	cd server && stack build

openapi.json: $(gen_openapi_exe)
	$(gen_openapi_exe) > $@

