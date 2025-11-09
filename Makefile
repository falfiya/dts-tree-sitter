ifeq ($(OS), Windows_NT)
	PATH := ./node_modules/.bin;$(PATH)
else
	PATH := ./node_modules/.bin:$(PATH)
endif

export NODE_OPTIONS:=--enable-source-maps

example: example.js beancount.d.ts
	node $<

beancount.d.ts: main.js
	node $< > beancount.d.ts

opts += --bundle
opts += --sourcemap
opts += --format=esm
opts += --platform=node
opts += --packages=external

%.js: %.ts
	esbuild $< $(opts) --outdir=.
