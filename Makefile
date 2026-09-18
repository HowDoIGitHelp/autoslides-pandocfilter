filter := dist-newstyle/build/x86_64-linux/ghc-9.10.3/pandoc-md-slides-1.1.0.0/x/md-slides/build/md-slides/md-slides
remarkjs := dist-newstyle/build/x86_64-linux/ghc-9.10.3/pandoc-md-slides-1.1.0.0/x/remarkjs/build/remarkjs/remarkjs

build-test: build filter-test

filter-test: build
	pandoc -t json test.md | \
		$(filter) -s "." -o "outputs/" -l 6 -w 60 | \
		$(remarkjs) | \
		pandoc -f json \
		-t markdown-simple_tables-multiline_tables-grid_tables \
		-o outputs/output.md

filter-test-no-args: build
	pandoc -t json test.md | \
		$(filter) | \
		$(remarkjs) | \
		pandoc -f json \
		-t markdown-simple_tables-multiline_tables-grid_tables \
		-o outputs/output.md

test-args: build
	pandoc -t json test.md | $(filter) -s "." -o "outputs/" -l 6 -w 60

build: app/Pandoc-filter.hs app/Remarkjs-pandoc-filter.hs
	cabal build
