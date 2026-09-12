filter := ../dist-newstyle/build/x86_64-linux/ghc-9.10.3/pandocfilter-0.1.0.0/x/paragraph-filter/build/paragraph-filter/paragraph-filter

filter-test:
	pandoc -t json ../test.md | \
		$(filter) "../" "." | \
		pandoc -f json \
		-t markdown-simple_tables-multiline_tables-grid_tables \
		-o output.md
