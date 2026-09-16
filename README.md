# pandoc-md-slides

## A Pandoc filter to convert markdown notes to markdown slides

This pandoc filter converts markdown notes (written in plain prose) into summarized slides.
The filter automatically splits long slides and resolves image target paths to new paths.

## Installing using cabal

You can install the package by unpacking the tarball distributable in the releases page and running `cabal install`

```bash
tar -xvf pandoc-md-slides-<version>.tar.gz
cd pandoc-md-slides-<version>
cabal install
```

## Basic Usage

```bash
pandoc test.md --filter md-slides -o outputs/output.md
```

## Transformations

### Paragraph Blocks

Paragraph blocks are split based on newlines and summarized into a `BulletList`.
The resulting list will only keep important sentences (i.e. sentences with emphasized words and/or strong words).

Separate paragraphs will be placed in separate slides.
Only top-level paragraph blocks are transformed into lists.

```markdown
## Header

This is a paragraph block.
That contains **important sentences**.
And unimportant sentences.
It will keep the *important ones*.
The header will be used as a *slide header*.

Separate Paragraphs will be placed in *separate slides*.
Like *this one*.
Paragraphs with no headers will use the *most recent* headers as slide headers.
```

```markdown
# Header

- That contains **important sentences**.
- It will keep the *important ones*.
- The header will be used as a *slide header*.

---

# Header

- Separate Paragraphs will be placed in *separate slides*.
- Like *this one*.
- Paragraphs with no headers will use the *most recent* headers as slide
  headers.
```

### Splitting slides

Long slides are split into multiple slides, if the blocks can be split.
Blocks that can be split are, bullet lists (including the transformed summarized paragraphs), ordered lists, display math blocks, code, table.

You can configure the split behavior by controlling the slide lines (`-l`) and line width (`-w`) arguments.
A slide is split if it exceeds the slide lines.
The number of lines of a slide is calculated based on the slide type and line width argument.

```bash
pandoc -t json test.md | \
    md-filter -l 6 -w 60 | \
    pandoc -f json \
    -t markdown-simple_tables-multiline_tables-grid_tables \
    -o outputs/output.md
```

The following default values will be used if the arguments are not set.

- slide lines (`-l`) - 6
- line width (`-w`) - 100

```markdown
## Header

- item1
- item2
- item3
- item4
- item5
- item6
- item7
- item8
- item9
- item10
- item11
- item12
- item13
```

```markdown
# Header

- item1
- item2
- item3
- item4
- item5
- item6

---

# Header

- item7
- item8
- item9
- item10

---

# Header

- item11
- item12
- item13
```

#### Math

When display math blocks are split, the display math is split based on math newlines (`\\`), ignoring newlines on `matrix` and `bmatrix` environments.

If the entire display math block are in an `aligned` environment, the split slides will inherit the `aligned` environment.

```markdown
## Header

$$
\begin{aligned}
{n \choose r-1}+{n \choose r}&=\frac{n!}{(r-1)!(n-(r-1))!}+\frac{n!}{r!(n-r)!}\\
&=\frac{n!}{(r-1)!(n-r+1)!}+\frac{n!}{r!(n-r)!}\\
&=\frac{n!r}{r!(n-r+1)!}+\frac{n!(n-r+1)}{r!(n-r+1)!}\\
&=\frac{n!r+n!(n-r+1)}{r!(n-r+1)!}\\
&=\frac{n!(n+1)}{r!(n+1-r)!}\\
&=\frac{(n+1)!}{r!(n+1-r)!}\\
{n \choose r-1}+{n \choose r}&={n+1 \choose r}
\end{aligned}
$$
```

```markdown
# Header

$$
\begin{aligned}
{n \choose r-1}+{n \choose r}&=\frac{n!}{(r-1)!(n-(r-1))!}+\frac{n!}{r!(n-r)!}\\
&=\frac{n!}{(r-1)!(n-r+1)!}+\frac{n!}{r!(n-r)!}\\
&=\frac{n!r}{r!(n-r+1)!}+\frac{n!(n-r+1)}{r!(n-r+1)!}\\
&=\frac{n!r+n!(n-r+1)}{r!(n-r+1)!}
\end{aligned}
$$

---

# Header

$$
\begin{aligned}
&=\frac{n!(n+1)}{r!(n+1-r)!}\\
&=\frac{(n+1)!}{r!(n+1-r)!}\\
{n \choose r-1}+{n \choose r}&={n+1 \choose r}
\end{aligned}
$$
```

### Image target replacement

You can pass two arguments in the filter, input file directory, and output file directory.
When used this way, the filter will replace image target paths with new paths resolved based on the output file directory.
If a path cannot be resolved based on the output file directory, then the said path is considered an external url or absolute path.

Here's an example of applying arguments to the filter.
Here the markdown is converted to `json` and the resulting `json` is piped to the filter with arguments, transformed `json` is then piped to a pandoc transformation back to the format `markdown-simple_tables-multiline_tables-grid_tables`.

```bash
pandoc -t json test.md | \
    md-filter -s "." -o "outputs/" | \
    pandoc -f json \
    -t markdown-simple_tables-multiline_tables-grid_tables \
    -o outputs/output.md
```

Here's an example Makefile that you can use to automatically resolve the directories of the source and the output.

```makefile
INPUTDIR ?= $(dir $(SOURCE))
OUTPUTDIR ?= $(dir $(OUTPUT))

$(OUTPUT): $(SOURCE)
	pandoc -t json $(SOURCE) | \
		md-slides -s $(INPUTDIR) -o $(OUTPUTDIR) | \
		remarkjs | \
		pandoc -f json \
		-t markdown-simple_tables-multiline_tables-grid_tables \
        -o $(OUTPUT)
```
