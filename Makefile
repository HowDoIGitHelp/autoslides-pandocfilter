filter := dist-newstyle/build/x86_64-linux/ghc-9.10.3/pandoc-md-slides-0.1.0.0/x/md-slides/build/md-slides/md-slides
remarkjs := dist-newstyle/build/x86_64-linux/ghc-9.10.3/pandoc-md-slides-0.1.0.0/x/remarkjs/build/remarkjs/remarkjs

filter-test:
	pandoc -t json test.md | \
		$(filter) "." "outputs/" | \
		$(remarkjs) | \
		pandoc -f json \
		-t markdown-simple_tables-multiline_tables-grid_tables \
		-o outputs/output.md

filter-test-no-args:
	pandoc -t json test.md | \
		$(filter) | \
		pandoc -f json \
		-t markdown-simple_tables-multiline_tables-grid_tables \
		-o outputs/output.md

