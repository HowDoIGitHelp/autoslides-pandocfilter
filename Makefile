filter := dist-newstyle/build/x86_64-linux/ghc-9.10.3/pandoc-md-slides-1.0.0.2/x/md-slides/build/md-slides/md-slides
remarkjs := dist-newstyle/build/x86_64-linux/ghc-9.10.3/pandoc-md-slides-1.0.0.2/x/remarkjs/build/remarkjs/remarkjs

filter-test:
	pandoc -t json test.md | \
		$(filter) -s "." -o "outputs/" -l 6 -w 60 | \
		$(remarkjs) | \
		pandoc -f json \
		-t markdown-simple_tables-multiline_tables-grid_tables \
		-o outputs/output.md

filter-test-no-args:
	pandoc -t json test.md | \
		$(filter) | \
		$(remarkjs) | \
		pandoc -f json \
		-t markdown-simple_tables-multiline_tables-grid_tables \
		-o outputs/output.md
