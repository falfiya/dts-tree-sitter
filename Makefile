ifeq ($(OS), Windows_NT)
	PATH := ./node_modules/.bin;$(PATH)
else
	PATH := ./node_modules/.bin:$(PATH)
endif

export NODE_OPTIONS:=--enable-source-maps

beancount.d.ts: main.js
	node $< > beancount.d.ts

opts += --bundle
opts += --sourcemap
opts += --format=esm
opts += --platform=node
opts += --packages=external

main.js: main.ts
	esbuild $< $(opts) --outdir=.
