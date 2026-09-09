{-# LANGUAGE OverloadedStrings #-}

import Text.Pandoc.JSON
import Text.Pandoc.Walk (walk)
import Text.Pandoc.Definition (Inline(SoftBreak), Block(BulletList))
import Data.Text (Text, length, pack, splitOn, isPrefixOf, isSuffixOf, intercalate, stripPrefix, stripSuffix)
import Text.Regex.Pcre2 (gsub, match, sub)
import Debug.Trace
import Data.Maybe (fromMaybe)

slidelines :: Int
slidelines = 6

linewidth :: Int
linewidth = 100

isSoftBreak :: Inline -> Bool
isSoftBreak SoftBreak = True
isSoftBreak _ = False

isHeader :: Block -> Bool
isHeader (Header _ _ _) = True
isHeader _ = False

isNote :: Inline -> Bool
isNote (Note block) = True
isNote _ = False

filterOutNotes :: [Inline] -> [Inline]
filterOutNotes l = filter isNote l

wrapEquation :: Text -> Text
wrapEquation eq = "$$" <> eq <> "$$"

wrapEquationAligned :: Text -> Text
wrapEquationAligned eq = "\\begin{aligned}\n" <> eq <> "\n\\end{aligned}"

isImportant :: Inline -> Bool
isImportant (Strong _) = True
isImportant (Emph _) = True
isImportant _ = False

isImportantSentence :: [Inline] -> Bool
isImportantSentence words = any isImportant words

--replaces paragraphs with a bulletlist of only important sentences
itemize :: Block -> Block
itemize block@(OrderedList _ _) = block
itemize block@(BulletList _) = block
itemize block@(Para [Math DisplayMath eq]) = block
itemize (Para paracontents) = (BulletList (map (\x -> [Plain x]) importantItems))
    where
        items = (listSplit isSoftBreak (filter (not . isNote) paracontents))
        importantItems = filter isImportantSentence items
itemize block = block

topDownBlockFilter :: (Block -> Block) -> Pandoc -> Pandoc
topDownBlockFilter blockfilter (Pandoc meta blocks) = Pandoc meta (map blockfilter blocks)

topDownBlockListFilter :: ([Block] -> [Block]) -> Pandoc -> Pandoc
topDownBlockListFilter blocklistfilter (Pandoc meta blocks) = Pandoc meta (blocklistfilter blocks)

-- converts math blocks to code for mathjax integration
codifiedMath :: Block -> Block
codifiedMath (Para [Math DisplayMath eq]) = Para [Code ("", [], []) (wrapEquation eq)]
codifiedMath block = block

-- removes empty bulletlists
dropEmptyList :: Block -> [Block]
dropEmptyList (BulletList []) = []
dropEmptyList block = [block]

-- removes hrules that will interfere with slide boundaries
dropStrayHRule :: Block -> [Block]
dropStrayHRule HorizontalRule = []
dropStrayHRule block = [block]

-- splits a list into a list of lists based on a predicate
listSplit :: (a -> Bool) -> [a] -> [[a]]
listSplit _ [] = [[]]
listSplit p l = 
    case (break p l) of
        (lf, [])   -> lf : []
        (lf, _:rs) -> lf : listSplit p rs


-- splits a list into a list of lists based on a predicate
-- but keeps the delims as separate lists
listSplitKeep :: (a -> Bool) -> [a] -> [[a]]
listSplitKeep _ [] = [[]]
listSplitKeep pred l = 
    case (break pred l) of
        (lf, [])   -> lf : []
        (lf, r:rs) -> lf : [r] : (listSplitKeep pred rs)

-- returns the even indices of the list
evenIndices :: [a] -> [a]
evenIndices (x : _ : xs) = x : evenIndices xs
evenIndices [x] = [x]
evenIndices [] = []

-- returns the odd indices of the list
oddIndices :: [a] -> [a]
oddIndices (_ : x : xs) = x : oddIndices xs
oddIndices _ = []


-- given a prepared list of list of blocks from listSplitKeep,
-- apply interleave to each pair of list
-- given [[],[h1],[b1,b2,b3],[h2],[b4,b5]]
-- [[h1,b1,h1,b2,h1,b3],[h2,b4,h2,b5]]
combineHeaders :: [[Block]] -> [[Block]]
combineHeaders ([]:ls) = combineHeaders ls
combineHeaders ls = zipWith interleave (map head (evenIndices ls)) (oddIndices ls)

-- interleaves a block in between the elements of a list of blocks
-- if it the block is a header remove id metadata
interleave :: Block -> [Block] -> [Block]
interleave (Header level (_, classes, kvattrs) inlines) l = concatMap (\x -> [betweener,x]) l
    where betweener = (Header level ("", classes, kvattrs) inlines)
interleave betweener l = concatMap (\x -> [betweener,x]) l

-- applies combineHeaders list of blocks in the pandoc document
-- and flattens the resulting list
insertHeaders :: Pandoc -> Pandoc
insertHeaders (Pandoc meta blocks) = (Pandoc meta (foldl (++) [] (combineHeaders splitBlocks)))
    where splitBlocks = (listSplitKeep (isHeader) blocks)

-- add slide separators "---" in bewtween headers 
sectionToSlides :: [Block] -> [Block]
sectionToSlides (header@(Header _ _ _) : nonHeader : rest) | not (isHeader nonHeader) = 
    [header, nonHeader, (RawBlock (Format "markdown") "---")] ++ sectionToSlides rest
-- sectionToSlides (header1@(Header _ _ _) : header2@(Header _ _ _) : rest) =
    [header1, (RawBlock (Format "markdown") "---"), header2, (RawBlock (Format "markdown") "---")] ++ sectionToSlides rest
sectionToSlides (block : rest) = block : sectionToSlides rest
sectionToSlides [] = []

-- calculates the length of an inline
inlineLength :: Inline -> Int
inlineLength Space = 1
inlineLength SoftBreak = 1
inlineLength LineBreak = 1
inlineLength (Note _) = 1
inlineLength (Str text) = Data.Text.length text
inlineLength (Code _ text) = Data.Text.length text
inlineLength (Math _ text) = Data.Text.length text
inlineLength (RawInline _ text) = Data.Text.length text
inlineLength (Emph inlines) = sum (map inlineLength inlines)
inlineLength (Underline inlines) = sum (map inlineLength inlines)
inlineLength (Strong inlines) = sum (map inlineLength inlines)
inlineLength (Strikeout inlines) = sum (map inlineLength inlines)
inlineLength (Superscript inlines) = sum (map inlineLength inlines)
inlineLength (Subscript inlines) = sum (map inlineLength inlines)
inlineLength (SmallCaps inlines) = sum (map inlineLength inlines)
inlineLength (Link _ inlines _) = sum (map inlineLength inlines)
inlineLength (Image _ inlines _) = sum (map inlineLength inlines)
inlineLength (Span _ inlines) = sum (map inlineLength inlines)
inlineLength (Quoted _ inlines) = sum (map inlineLength inlines)
inlineLength (Cite _ inlines) = sum (map inlineLength inlines)

componentLength :: Block -> Int
componentLength (Plain inlines) = sum (map inlineLength inlines)

-- calculates the height of a block
-- if a blocks length exceeds 100,
-- assume that the block wraps to the next line
componentHeight :: Block -> Int
componentHeight (BulletList items) = sum (map blockListHeight items)
componentHeight (Para inlines) = ceiling ((fromIntegral totalInlineLength) / (fromIntegral linewidth))
    where totalInlineLength = sum (map inlineLength inlines)
componentHeight block = ceiling (((fromIntegral . componentLength) block) / (fromIntegral linewidth))

-- returns the sum of the heights in a list of blocks
blockListHeight :: [Block] -> Int
blockListHeight blocks = sum (map componentHeight blocks)

-- a helper function to replace list items with their heights
-- used for debugging
heightFilter :: Block -> Block
heightFilter (BulletList items) = BulletList (map ((\x -> [Plain [Str (pack (show x))]]) . blockListHeight) items)
heightFilter block = block

-- splits a list of a into list of lists of a
-- where each list in the list of lists has either 
-- one element e with sizeof e greater than binSize
-- or a list of elements with total sizeof <= binSize
binSplit :: [a] -> Int -> (a -> Int) -> [[a]]
binSplit (y:ys) binSize sizeof = go ys binSize sizeof [[y]]
    where
        go [] _ _ binList = binList
        go (x:xs) binSize sizeof binList
            | totalsize + (sizeof x) <= binSize = go xs binSize sizeof (ib ++ [(b ++ [x])])
            | otherwise = go xs binSize sizeof (ib ++ [b] ++ [[x]])
            where
                totalsize = sum (map sizeof b)
                b = last binList
                ib = init binList

-- splits a BulletList block into multiple bulletlists
-- based on max slidelines
splitList :: Block -> [Block]
splitList (BulletList items) = (map (\x -> BulletList x) binnedItems)
    where
        binnedItems = (binSplit items slidelines blockListHeight)
splitList block = [block]

dropEmpty :: [Block] -> [Block]
dropEmpty (Header _ _ _ : HorizontalRule : rest) = dropEmpty rest
dropEmpty (block : rest) = block : dropEmpty rest
dropEmpty [] = []

-- this function is used to mask multiline environments with {{<env>}}
maskedMultilines :: String -> Text -> (Text, [Text])
maskedMultilines env mathBlock = (replacedBlock, matches) 
    where
        pattern = pack ("(?s)\\\\begin\\{" ++ env ++ "\\}.*?\\\\end\\{" ++ env ++ "\\}") :: Text
        replacement = pack ("{{" ++ env ++ "}}") :: Text
        matches = match pattern mathBlock :: [Text]
        replacedBlock = gsub pattern replacement mathBlock

-- replaces latex newlines with {{nl}}
maskNewlines :: Text -> Text
maskNewlines mathBlock = replacedBlock
    where
        pattern = pack "\\\\\\\\"
        replacement = pack "{{nl}}"
        replacedBlock = gsub pattern replacement mathBlock

envs :: [String]
envs = ["bmatrix", "matrix"]

-- from a list of envs, it returns the same mathblock but with
-- each env from the list masked, it also recovers a list of list of the 
-- replaced env matches
multiEnvMask :: [String] -> Text -> (Text, [[Text]])
multiEnvMask [] mathBlock = (mathBlock, [])
multiEnvMask (x:xs) mathBlock = (replacedBlockR, matches : matchesR)
    where
        (replacedBlock, matches) = maskedMultilines x mathBlock
        (replacedBlockR, matchesR) = multiEnvMask xs replacedBlock

maskMath :: Block -> Block
maskMath (Para [Math DisplayMath mathBlock]) =
    (Para [Math DisplayMath (multiRecoverEnvs envs matches (maskNewlines replacedBlock))])
    where
       (replacedBlock, matches) = multiEnvMask envs mathBlock
maskMath block = block

-- multiple version of recoverEnvs,
-- recovers mulitple envs from a list of list of matches
multiRecoverEnvs :: [String] -> [[Text]] -> Text -> Text
multiRecoverEnvs [] _ mathBlock = mathBlock
multiRecoverEnvs _ [] mathBlock = mathBlock
multiRecoverEnvs (e:es) (m:ms) mathBlock = multiRecoverEnvs es ms (recoverEnvs e m mathBlock)

-- restores masked envs based on a list of matches
recoverEnvs :: String -> [Text] -> Text -> Text
recoverEnvs _ [] mathBlock = mathBlock
recoverEnvs env (m:ms) mathBlock = recoverEnvs env ms (sub pattern m mathBlock)
    where pattern = pack ("\\{\\{" ++ env ++ "\\}\\}")

-- checks if the mathblock is surrounded by some latex environment env
-- used for alignment
isEnvMath :: Text -> String -> Bool
isEnvMath mathBlock env =
    (isPrefixOf (pack ("\n\\begin{" ++ env ++ "}\n")) mathBlock) && (isSuffixOf (pack ("\n\\end{" ++ env ++ "}\n")) mathBlock)

-- surrounds the mathblock with a latex environment env
envMath :: Text -> String -> Text
envMath mathBlock env = (pack ("\n\\begin{" ++ env ++ "}\n")) <> mathBlock <> (pack ("\n\\end{" ++ env ++ "}\n"))

-- strips a latex environment env from a mathblock
stripEnvMath :: Text -> String -> Text
stripEnvMath mathBlock env = fromMaybe strippedPrefix (stripSuffix (pack ("\n\\end{" ++ env ++ "}\n")) strippedPrefix)
    where strippedPrefix = fromMaybe mathBlock (stripPrefix (pack ("\n\\begin{" ++ env ++ "}\n")) mathBlock)

-- splits a mathBlock with the masked newline "{{nl}}"
mathLines :: Text -> [Text]
mathLines mathBlock = splitOn (pack "{{nl}}") mathBlock

-- calculates the height of a mathblock
mathLineHeight :: Text -> Int
mathLineHeight text = 1
    where
        pattern = (pack "\\\\\\\\") :: Text
        matches = (match pattern text) :: [Text]

-- removes empty lines from the mathblock
cleanUpMathBlock :: Text -> Text
cleanUpMathBlock mathBlock = gsub pattern replacement mathBlock
    where
        pattern = (pack "\\n\\n") :: Text
        replacement = (pack "\n") :: Text

-- splits a mathblock into bins
splitMath :: Block -> [Block]
splitMath (Para [Math DisplayMath mathBlock]) = (map (\x -> (Para [Math DisplayMath ((cleanUpMathBlock . alignment) x)])) binnedBlocks)
    where
        strippedEnv = stripEnvMath mathBlock "aligned"
        lines = mathLines strippedEnv
        binnedLines = (binSplit lines slidelines mathLineHeight)
        binnedBlocks = map (intercalate "\\\\") binnedLines
        alignment = if (isEnvMath mathBlock "aligned") then (\x -> envMath x "aligned") else id
splitMath block = [block]

-- returns the height of a row
rowHeight :: Row -> Int
rowHeight _ = 1

-- splits a table body into a list of tablebodies based on bins
splitTableBody :: TableBody -> [TableBody]
splitTableBody (TableBody attr rowHeadColumns headerRows rows) =
    (map (\x -> TableBody attr rowHeadColumns headerRows x) binnedRows)
    where binnedRows = binSplit rows slidelines rowHeight

-- splits a table's tablebody component
splitTable :: Block -> [Block]
splitTable (Table attr caption colspec head (body:rest) foot) = tableList
    where tableList = (map (\x -> Table attr caption colspec head [x] foot) (splitTableBody body))

-- splits blocks into multiple slides
split :: [Block] -> [Block]
split (header@(Header _ _ _) : bulletList@(BulletList _) : (RawBlock (Format "markdown") "---") : rest) = 
    (concatMap (\x -> [header, x, (RawBlock (Format "markdown") "---")]) (splitList bulletList)) ++ (split rest)
split (header@(Header _ _ _) : displayMath@(Para [Math DisplayMath _]) : (RawBlock (Format "markdown") "---") : rest) = 
    (concatMap (\x -> [header, x, (RawBlock (Format "markdown") "---")]) (splitMath displayMath)) ++ (split rest)
split (header@(Header _ _ _) : table@(Table _ _ _ _ _ _) : (RawBlock (Format "markdown") "---") : rest) = 
    (concatMap (\x -> [header, x, (RawBlock (Format "markdown") "---")]) (splitTable table)) ++ (split rest)
split (header@(Header _ _ _) : block : (RawBlock (Format "markdown") "---") : rest) = 
    [header, block, (RawBlock (Format "markdown") "---")] ++ (split rest)
split l = l

main :: IO ()
main = toJSONFilter
    $ (walk codifiedMath)
    . (topDownBlockListFilter split)
    . (walk maskMath)
    . (walk sectionToSlides)
    . insertHeaders
    . (walk (concatMap dropStrayHRule))
    . (walk (concatMap dropEmptyList))
    . (topDownBlockFilter itemize)
