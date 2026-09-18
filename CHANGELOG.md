# Revision history for pandocfilter

## 0.1.0.0 -- 2026-09-13

- First version. Released on an unsuspecting world.

## 0.1.1.0 -- 2025-09-15

- fix length and height calculations for nested lists

## 1.0.0.0 -- 2025-09-16

- changed source dir (`-s`) and output dir (`-o`) into optional named arguments
- added optional arguments slide lines (`-l`) and line width (`-w`) to configure slide split behavior

## 1.0.0.1 -- 2025-09-16

- fixed issue where last slide is not affected by split filter

## 1.0.0.2 -- 2025-09-16

- change slide title from h1 to h2

## 1.0.0.3 -- 2025-09-16

- fixed issue where `-o` argument is read as `-s` in the filter.

## 1.0.0.4 -- 2025-09-18

- add option for external configuration through `slides.yaml`,
- add new configuration options through `slides.yaml`, `unOrphanDisplayBlocks`, `keptSentences`
