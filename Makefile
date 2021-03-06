
CORPUS=$(HOME)/d/wikipedia/enwiki-20160601-pages-articles-multistream.xml.bz2

# BUILD_FLAGS=--pedantic --library-profiling --executable-profiling
BUILD_FLAGS=--pedantic
SAMPLE=10

RUN=stack exec -- afd

init: stack.yaml

stack.yaml:
	stack init --prefer-nightly

run: build
	-rm test.sqlite3
	pv $(CORPUS) | bzcat | $(RUN) import-xml --db-file test.sqlite3

samples/sample-$(SAMPLE).xml: build
	-mkdir -p samples
	pv $(CORPUS) | bzcat | $(RUN) sample-xml --n $(SAMPLE) > $@

view:
	bzcat $(CORPUS) | less

docs:
	stack haddock
	open `stack path --local-doc-root`/index.html

# package:
# build a release tarball or executable
#
# dev:
# start dev server or process. `vagrant up`, `yesod devel`, etc.
#
# deploy:
# prep and push

configure:
	cabal configure \
		--package-db=clear \
		--package-db=global \
		--package-db=`stack path --snapshot-pkg-db` \
		--package-db=`stack path --local-pkg-db`

install:
	stack install

tags: $(SRC)
	codex update

hlint:
	hlint *.hs src specs

clean:
	stack clean
	codex cache clean

distclean: clean
	rm stack.yaml

build:
	stack build $(BUILD_FLAGS)

test:
	stack test $(BUILD_FLAGS)

bench:
	stack bench $(BUILD_FLAGS)

watch:
	ghcid "--command=stack ghci"

watch-test:
	stack test --file-watch --pedantic # --test-arguments "-m TODO"

restart: distclean init build

rebuild: clean build

.PHONY: init run docs configure install hlint clean distclean build test
.PHONY: bench watch watch-test restart rebuild
