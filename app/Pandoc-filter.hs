{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}

import Text.Pandoc.JSON
import Text.Pandoc.Walk (walk, walkM)
import Data.Text
    ( Text
    , length
    , pack
    , unpack
    , splitOn
    , isPrefixOf
    , isSuffixOf
    , intercalate
    , stripPrefix
    , stripSuffix
    , replace
    , stripStart
    )
import Text.Regex.Pcre2 (gsub, match, sub)
-- import Debug.Trace (trace, traceM)
import Data.Maybe (fromMaybe, listToMaybe)
import Path (Path, Abs, Dir, File, toFilePath)
import Path.IO (resolveDir', resolveFile)
import System.FilePath (splitDirectories, (</>))
import System.Environment (withArgs)
import Data.List (isSuffixOf)
import Options.Applicative
    ( Parser
    , strOption
    , execParser
    , helper
    , progDesc
    , header
    , fullDesc
    , info
    , help
    , metavar
    , short
    , long
    , optional
    , auto
    , option
    , (<**>) )

data FilterArgs = FilterArgs
    { sourceDirArg :: Maybe String
    , outputDirArg :: Maybe String
    , slidelinesArg :: Maybe Int
    , linewidthArg :: Maybe Int
    } deriving (Show)

argParser :: Parser FilterArgs
argParser = FilterArgs
    <$> optional
        ( strOption
            ( long "sourceDir"
            <> short 's'
            <> metavar "DIR"
            <> help "path to directory of source" )
        )
    <*> optional 
        ( strOption
            ( long "outputDir"
            <> short 'o'
            <> metavar "DIR"
            <> help "path to directory of output" )
        )
    <*> optional
        ( option auto
            ( long "lines"
            <> short 'l'
            <> metavar "INT"
            <> help "max lines in a slide" )
        )
    <*> optional
        ( option auto
            ( long "width"
            <> short 'w'
            <> metavar "INT"
            <> help "maximum width of a line in characters" )
        )

pattern SlideSep :: Block
pattern SlideSep <- RawBlock (Format "markdown") "---"

slideSep :: Block
slideSep = RawBlock (Format "markdown") "---"

isSoftBreak :: Inline -> Bool
isSoftBreak SoftBreak = True
isSoftBreak _ = False

isHeader :: Block -> Bool
isHeader (Header _ _ _) = True
isHeader _ = False

isNote :: Inline -> Bool
isNote (Note _) = True
isNote _ = False

-- remove notes from para and plain blocks
dropNotes :: Block -> Block
dropNotes (Para inlines) = Para (filter (not . isNote) inlines)
dropNotes (Plain inlines) = Plain (filter (not . isNote) inlines)
dropNotes block = block

-- wrapEquationAligned :: Text -> Text
-- wrapEquationAligned eq = "\\begin{aligned}\n" <> eq <> "\n\\end{aligned}"

isImportant :: Inline -> Bool
isImportant (Strong _) = True
isImportant (Emph _) = True
isImportant _ = False

isImportantSentence :: [Inline] -> Bool
isImportantSentence inlines = any isImportant inlines

endsWithPunctuation :: Inline -> Bool
endsWithPunctuation (Str inline) =
    (Data.Text.isSuffixOf (pack ".") inline) || (Data.Text.isSuffixOf (pack "!") inline) || (Data.Text.isSuffixOf (pack "?") inline)
endsWithPunctuation _ = False

softBreakInlines :: [Inline] -> [Inline]
softBreakInlines (inline : Space : rest) | endsWithPunctuation inline =
    [inline, SoftBreak] ++ (softBreakInlines rest)
softBreakInlines (inline : rest) = inline : (softBreakInlines rest)
softBreakInlines [] = []

softBreakParagraph :: Block -> Block
softBreakParagraph (Para inlines) = Para (softBreakInlines inlines)
softBreakParagraph block = block

--replaces paragraphs with a bulletlist of only important sentences
itemize :: Block -> Block
itemize block@(OrderedList _ _) = block
itemize block@(BulletList _) = block
itemize block@(Para [Math DisplayMath _]) = block
itemize (Para inlines) = (BulletList (map (\x -> [Plain x]) importantItems))
    where
        items = listSplit isSoftBreak inlines
        importantItems = filter isImportantSentence items
itemize block = block

topDownBlockFilter :: (Block -> Block) -> Pandoc -> Pandoc
topDownBlockFilter blockfilter (Pandoc meta blocks) = Pandoc meta (map blockfilter blocks)

topDownBlockListFilter :: ([Block] -> [Block]) -> Pandoc -> Pandoc
topDownBlockListFilter blocklistfilter (Pandoc meta blocks) = Pandoc meta (blocklistfilter blocks)

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
listSplitKeep predicate l = 
    case (break predicate l) of
        (lf, [])   -> lf : []
        (lf, r:rs) -> lf : [r] : (listSplitKeep predicate rs)

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
combineHeaders ls =
    zipWith interleave betweeners (oddIndices ls)
    where
        betweeners = (map ((fromMaybe defaultHeader) . listToMaybe) (evenIndices ls))
        defaultHeader = Header 1 ( "default-headerBlock" , [] , [] ) [ Str "Default" , Space , Str "Header" ]
-- interleaves a block in between the elements of a list of blocks
-- also removes id metadata of headers
interleave :: Block -> [Block] -> [Block]
interleave (Header _ (_, classes, kvattrs) inlines) [] = [Header 1 ("", classes, kvattrs) inlines]
interleave (Header _ (_, classes, kvattrs) inlines) l = concatMap (\x -> [betweener,x]) l
    where betweener = (Header 1 ("", classes, kvattrs) inlines)
interleave betweener l = concatMap (\x -> [betweener,x]) l

-- applies combineHeaders list of blocks in the pandoc document
-- and flattens the resulting list
insertHeaders :: Pandoc -> Pandoc
insertHeaders (Pandoc meta blocks) = (Pandoc meta (foldl (++) [] (combineHeaders splitBlocks)))
    where splitBlocks = (listSplitKeep (isHeader) blocks)

-- add slide separators "---" in bewtween headers 
sectionToSlides :: [Block] -> [Block]
sectionToSlides (header1@(Header _ _ _) : header2@(Header _ _ _) : rest) =
   [header1, slideSep] ++ (sectionToSlides (header2 : rest))
sectionToSlides (headerBlock@(Header _ _ _) : nonHeader : rest) =
    [headerBlock, nonHeader, slideSep] ++ sectionToSlides rest
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
componentLength (Para inlines) = sum (map inlineLength inlines)
componentLength block = error ("unsupported length calculation" ++ (show block))

-- calculates the height of a block
-- if a blocks length exceeds 100,
-- assume that the block wraps to the next line
componentHeight :: Int -> Int -> Block -> Int
componentHeight slidelines linewidth (BulletList items) = sum (map (blockListHeight slidelines linewidth) items)
componentHeight slidelines linewidth (OrderedList _ items) = sum (map (blockListHeight slidelines linewidth) items)
componentHeight _ linewidth block@(Para _) = ceiling (blockLength / (fromIntegral linewidth))
    where blockLength = ((fromIntegral . componentLength) block) :: Double
componentHeight _ linewidth block@(Plain _) = ceiling (blockLength / (fromIntegral linewidth))
    where blockLength = ((fromIntegral . componentLength) block) :: Double
componentHeight slidelines _ _ = slidelines

-- returns the sum of the heights in a list of blocks
blockListHeight :: Int -> Int -> [Block] -> Int
blockListHeight slidelines linewidth blocks = sum (map (componentHeight slidelines linewidth) blocks)

-- a helper function to replace list items with their heights
-- used for debugging
-- heightFilter :: Block -> Block
-- heightFilter (BulletList items) = BulletList (map ((\x -> [Plain [Str (pack (show x))]]) . blockListHeight) items)
-- heightFilter block = block

balanceLists :: [a] -> [a] -> ([a], [a])
balanceLists first second =
    splitAt halfLength (first ++ second)
    where
        totalLength = fromIntegral ((Prelude.length first) + (Prelude.length second)) :: Double
        halfLength = ceiling (totalLength / 2)

balanceLast :: [[a]] -> [[a]]
balanceLast [] = []
balanceLast [x] = [x]
balanceLast l = ((init . init) l) ++ [bal2Last, balLast]
    where (bal2Last, balLast) = balanceLists ((last . init) l) (last l)

-- splits a list of a into list of lists of a
-- where each list in the list of lists has either 
-- one element e with sizeof e greater than binSize
-- or a list of elements with total sizeof <= binSize
binSplit :: [a] -> Int -> (a -> Int) -> [[a]]
binSplit [] _ _ = []
binSplit (y:ys) binSize sizeof = balanceLast (go ys binSize sizeof [[y]])
    where
        go [] _ _ binList = binList
        go (x:xs) binSize_ sizeof_ binList
            | totalsize + (sizeof_ x) <= binSize_ = go xs binSize sizeof_ (ib ++ [(b ++ [x])])
            | otherwise = go xs binSize_ sizeof_ (ib ++ [b] ++ [[x]])
            where
                totalsize = sum (map sizeof_ b)
                b = last binList
                ib = init binList



-- change the numbering of an OrderedList
renumberedList :: Int -> Block -> Block
renumberedList newStart (OrderedList (_, numbering, suffix) items) =
    OrderedList (newStart, numbering, suffix) items
renumberedList _ block = block

-- change the numbering of a list of ordered lists so that
-- numbering continues from element to element
renumberedLists :: [Block] -> [Block]
renumberedLists (lists@((OrderedList (startNum, _, _) _) : _)) =
    go startNum lists
    where
        go newStart (list'@(OrderedList (_, _, _) items) : rest) =
            (renumberedList newStart list') : (go nextNumber rest)
            where nextNumber = newStart + (Prelude.length items)
        go newStart (block : rest) = block : (go newStart rest)
        go _ [] = []
renumberedLists (block : rest) = block : (renumberedLists rest)
renumberedLists [] = []

-- splits a list blocks into multiple lists
-- based on max slidelines
splitList :: Int -> Int -> Block -> [Block]
splitList slidelines linewidth (BulletList items) = map (\x -> BulletList x) (binSplit items slidelines (blockListHeight slidelines linewidth))
splitList slidelines linewidth (OrderedList prefix items) =
    renumberedLists (map (\x -> OrderedList prefix x) (binSplit items slidelines (blockListHeight slidelines linewidth)))
splitList _ _ block = [block]

-- dropEmpty :: [Block] -> [Block]
-- dropEmpty (Header _ _ _ : HorizontalRule : rest) = dropEmpty rest
-- dropEmpty (block : rest) = block : dropEmpty rest
-- dropEmpty [] = []

-- this function is used to mask multiline environments with {{<env>}}
maskedMultilines :: String -> Text -> (Text, [Text])
maskedMultilines env mathBlock = (replacedBlock, matches)
    where
        rePattern = pack ("(?s)\\\\begin\\{" ++ env ++ "\\}.*?\\\\end\\{" ++ env ++ "\\}") :: Text
        replacement = pack ("{{" ++ env ++ "}}") :: Text
        matches = match rePattern mathBlock :: [Text]
        replacedBlock = gsub rePattern replacement mathBlock

-- replaces latex newlines with {{nl}}
maskNewlines :: Text -> Text
maskNewlines mathBlock = replace (pack "\\\\") (pack "{{nl}}") mathBlock

envs :: [String]
envs = ["bmatrix", "matrix", "array"]

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
recoverEnvs env (m:ms) mathBlock =
    recoverEnvs env ms (sub rePattern m mathBlock)
    where rePattern = pack ("\\{\\{" ++ env ++ "\\}\\}")

-- checks if the mathblock is surrounded by some latex environment env
-- used for alignment
isEnvMath :: String -> Text -> Bool
isEnvMath env mathBlock =
    (isPrefixOf (pack ("\n\\begin{" ++ env ++ "}\n")) mathBlock)
        && (Data.Text.isSuffixOf (pack ("\n\\end{" ++ env ++ "}\n")) mathBlock)

-- surrounds the mathblock with a latex environment env
envMath :: String -> Text -> Text
envMath env mathBlock =
    (pack ("\n\\begin{" ++ env ++ "}\n"))
        <> mathBlock
        <> (pack ("\n\\end{" ++ env ++ "}\n"))

-- strips a latex environment env from a mathblock
stripEnvMath :: String -> Text -> Text
stripEnvMath env mathBlock =
    fromMaybe strippedPrefix (stripSuffix (pack ("\n\\end{" ++ env ++ "}\n")) strippedPrefix)
    where
        strippedPrefix = fromMaybe mathBlock (stripPrefix (pack ("\n\\begin{" ++ env ++ "}\n")) mathBlock)

-- splits a mathBlock with the masked newline "{{nl}}"
mathLines :: Text -> [Text]
mathLines mathBlock = splitOn (pack "{{nl}}") mathBlock

-- calculates the height of a mathblock
mathLineHeight :: Text -> Int
mathLineHeight _ = 1

-- removes empty lines from the mathblock
cleanUpMathBlock :: Text -> Text
cleanUpMathBlock mathBlock = gsub rePattern replacement mathBlock
    where
        rePattern = (pack "\\n\\n") :: Text
        replacement = (pack "\n") :: Text

-- strips whitespace (indentation) from display math blocks
stripIndentMath :: Block -> Block
stripIndentMath (Para [Math DisplayMath mathBlock]) =
    Para [Math DisplayMath cleanBlock]
    where
        splitLines = splitOn (pack "\n") mathBlock
        cleanBlock = intercalate "\n" (map stripStart splitLines)
stripIndentMath block = block

replaceAlignment :: String -> Text -> Text
replaceAlignment env text =
    ((replace (pack ("\\begin{" ++ env ++ "}")) (pack "\\begin{aligned}"))
        . (replace (pack ("\\end{" ++ env ++ "}")) (pack "\\end{aligned}"))) text

alignEnvs :: [String]
alignEnvs = ["align", "align*"]

normalizedAlignment :: Block -> Block
normalizedAlignment (Para [Math DisplayMath mathBlock]) =
    Para [Math DisplayMath (composedReplace mathBlock)]
    where
        composedReplace = foldl (.) id (map replaceAlignment alignEnvs)
normalizedAlignment block = block

-- splits a mathblock into bins
splitMath :: Int -> Block -> [Block]
splitMath slidelines (Para [Math DisplayMath mathBlock]) =
    map (\x -> (Para [Math DisplayMath ((cleanUpMathBlock . alignment) x)])) binnedBlocks
    where
        strippedEnv = stripEnvMath "aligned" mathBlock
        mLines = mathLines strippedEnv
        binnedLines = (binSplit mLines slidelines mathLineHeight)
        binnedBlocks = map (intercalate "\\\\") binnedLines
        alignment = if (isEnvMath "aligned" mathBlock) then (envMath "aligned") else id
splitMath _ block = [block]

-- returns the height of a row
rowHeight :: Row -> Int
rowHeight _ = 1

-- splits a table body into a list of tablebodies based on bins
splitTableBody :: Int -> TableBody -> [TableBody]
splitTableBody slidelines (TableBody attr rowHeadColumns headerRows rows) =
    (map (\x -> TableBody attr rowHeadColumns headerRows x) binnedRows)
    where binnedRows = binSplit rows slidelines rowHeight

-- splits a table's tablebody component
splitTable :: Int -> Block -> [Block]
splitTable slidelines (Table attr caption colspec headerBlock (body:_) foot) = tableList
    where
        tableList = (map (\x -> Table attr caption colspec headerBlock [x] foot) (splitTableBody slidelines body))
splitTable _ block = [block]

codeLineHeight :: Text -> Int
codeLineHeight _ = 1

-- splits code blocks int lines of code
splitCode :: Int -> Block -> [Block]
splitCode slidelines (CodeBlock (_, classes, kvs) codeBlock) =
    map (\x -> (CodeBlock ("", classes, kvs) x)) binnedCode
    where
        codeLines = splitOn (pack "\n") codeBlock
        binnedLines = binSplit codeLines slidelines codeLineHeight
        binnedCode = map (intercalate "\n") binnedLines
splitCode _ block = [block]

-- splits blocks into multiple slides
split :: Int -> Int -> [Block] -> [Block]
split slidelines linewidth (headerBlock@(Header _ _ _) : SlideSep : rest) =
    [headerBlock, slideSep] ++ (split slidelines linewidth rest)
split slidelines linewidth (headerBlock@(Header _ _ _) : bulletList@(BulletList _) : SlideSep : rest) = 
    (concatMap (\x -> [headerBlock, x, slideSep]) (splitList slidelines linewidth bulletList)) ++ (split slidelines linewidth rest)
split slidelines linewidth (headerBlock@(Header _ _ _) : orderedList@(OrderedList _ _) : SlideSep : rest) = 
    (concatMap (\x -> [headerBlock, x, slideSep]) (splitList slidelines linewidth orderedList)) ++ (split slidelines linewidth rest)
split slidelines linewidth (headerBlock@(Header _ _ _) : displayMath@(Para [Math DisplayMath _]) : SlideSep : rest) = 
    (concatMap (\x -> [headerBlock, x, slideSep]) (splitMath slidelines displayMath)) ++ (split slidelines linewidth rest)
split slidelines linewidth (headerBlock@(Header _ _ _) : table@(Table _ _ _ _ _ _) : SlideSep : rest) = 
    (concatMap (\x -> [headerBlock, x, slideSep]) (splitTable slidelines table)) ++ (split slidelines linewidth rest)
split slidelines linewidth  (headerBlock@(Header _ _ _) : code@(CodeBlock _ _) : SlideSep : rest) = 
    (concatMap (\x -> [headerBlock, x, slideSep]) (splitCode slidelines code)) ++ (split slidelines linewidth rest)
split slidelines linewidth (headerBlock@(Header _ _ _) : block : SlideSep : rest) = 
    [headerBlock, block, slideSep] ++ (split slidelines linewidth rest)
split slidelines linewidth (block : rest) = block : (split slidelines linewidth rest)
split _ _ [] = []

-- drop common prefix from a list
dropCommon :: Eq a => [a] -> [a] -> ([a], [a])
dropCommon (x:xs) (y:ys) | x == y = dropCommon xs ys
dropCommon a b = (a, b)

-- convert absolute file path to relative file path based on some
-- absoulte directory path
relatePath :: Path Abs Dir -> Path Abs File -> FilePath
relatePath directory file
    | (Prelude.length dirPathSuffix) < (Prelude.length dirPathList) = foldl (</>) "" relativePathList
    | otherwise = toFilePath file
    where
        dirPathList = splitDirectories (toFilePath directory)
        filePathList = splitDirectories (toFilePath file)
        (dirPathSuffix, filePathSuffix) = dropCommon dirPathList filePathList
        relativePathList = (map (\_ -> "..") dirPathSuffix) ++ filePathSuffix

-- replace image targets with new paths resolved from 
-- output directory
resolveImagePaths :: Path Abs Dir -> Path Abs Dir -> Inline -> IO Inline
resolveImagePaths inputDir outputDir (Image attr alttext (target, title)) = do
    absoluteImagePath <- resolveFile inputDir (unpack target)
    -- traceM (show absoluteImagePath)
    -- traceM (show outputDir)
    -- traceM (show (relatePath outputDir absoluteImagePath))
    let newPath = pack (relatePath outputDir absoluteImagePath)
    return (Image attr alttext (newPath, title))
resolveImagePaths _ _ inline = return inline

initSafe :: [a] -> Maybe [a]
initSafe [] = Nothing
initSafe l = Just (init l)

removeTrailingSep :: Pandoc -> Pandoc
removeTrailingSep (Pandoc meta blocks) | (Data.List.isSuffixOf [slideSep] blocks) =
    Pandoc meta (fromMaybe [] (initSafe blocks))
removeTrailingSep pandoc = pandoc

-- validInt :: String -> Bool
-- validInt str = case (readMaybe str :: Maybe Int) of
--     Just int -> int > 0
--     Nothing -> False

-- the main pandoc filter, returns IO Pandoc because
-- of absolute path resolution
pandocFilterWithArgs :: FilterArgs -> Pandoc -> IO Pandoc
pandocFilterWithArgs args (Pandoc meta blocks) = do
    let beforeSplitFilter =
            walk dropNotes
            . topDownBlockFilter maskMath
            . topDownBlockListFilter sectionToSlides
            . insertHeaders
            . walk (concatMap dropStrayHRule)
            . walk (concatMap dropEmptyList)
            . topDownBlockFilter itemize
            . topDownBlockFilter normalizedAlignment
            . topDownBlockFilter stripIndentMath
            . topDownBlockFilter softBreakParagraph
    let slidelines = case (slidelinesArg args) of
            Just l -> l
            Nothing -> 6
    let linewidth = case (linewidthArg args) of
            Just w -> w
            Nothing -> 100
    let combinedFilter =
            removeTrailingSep
            . (topDownBlockListFilter (split slidelines linewidth))
            . beforeSplitFilter
    case (sourceDirArg args, sourceDirArg args) of
        (Just inputPathStr, Just outputPathStr) -> do
            inputPathAbs <- resolveDir' inputPathStr
            outputPathAbs <- resolveDir' outputPathStr
            replacedPathsBlocks <- walkM (resolveImagePaths inputPathAbs outputPathAbs) blocks
            return (combinedFilter (Pandoc meta replacedPathsBlocks))
        _ -> return (combinedFilter (Pandoc meta blocks))

main :: IO ()
main = do
    args <- execParser opts
    withArgs [] $ toJSONFilter (pandocFilterWithArgs args)
    where
        opts = info (argParser <**> helper)
            ( fullDesc
            <> progDesc "Convert md notes AST to md slides AST"
            <> header "md-slides")
