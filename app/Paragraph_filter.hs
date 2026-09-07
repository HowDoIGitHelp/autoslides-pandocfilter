{-# LANGUAGE OverloadedStrings #-}

import Text.Pandoc.JSON
import Text.Pandoc.Walk (walk)
import Text.Pandoc.Definition (Inline(SoftBreak))
import Data.Text (Text, length, pack)

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

isImportant :: Inline -> Bool
isImportant (Strong _) = True
isImportant (Emph _) = True
isImportant _ = False

isImportantSentence :: [Inline] -> Bool
isImportantSentence words = any isImportant words

--replaces paragraphs with a bulletlist of only important sentences
itemize :: Block -> Block
itemize (Para [Math DisplayMath eq]) = Para [Code ("", [], []) (wrapEquation eq)]
itemize (Para paracontents) = BulletList (map (\x -> [Plain x]) importantItems)
    where
        items = (listSplit isSoftBreak (filter (not . isNote) paracontents))
        importantItems = filter isImportantSentence items
itemize x = x

dropEmptyList :: Block -> [Block]
dropEmptyList (BulletList []) = []
dropEmptyList block = [block]

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
    where
        betweener = (Header level ("", classes, kvattrs) inlines)
interleave betweener l = concatMap (\x -> [betweener,x]) l

-- applies combineHeaders list of blocks in the pandoc document
-- and flattens the resulting list
insertHeaders :: Pandoc -> Pandoc
insertHeaders (Pandoc meta blocks) = Pandoc meta (foldl (++) [] (combineHeaders (listSplitKeep (isHeader) blocks)))

-- add slide separators "---" in bewtween headers 
sectionToSlides :: [Block] -> [Block]
sectionToSlides (header@(Header _ _ _) : nonHeader : rest) | not (isHeader nonHeader) = 
    [header, nonHeader, (RawBlock (Format "markdown") "---")] ++ sectionToSlides rest
sectionToSlides (block : rest) = block : sectionToSlides rest
sectionToSlides [] = []

-- calculates the lenght of an inline
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
componentHeight block = ceiling (((fromIntegral . componentLength) block) / 100.0)

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

dropEmpty :: [Block] -> [Block]
dropEmpty (Header _ _ _ : HorizontalRule : rest) = dropEmpty rest
dropEmpty (block : rest) = block : dropEmpty rest
dropEmpty [] = []

main :: IO ()
main = toJSONFilter ((walk sectionToSlides) . insertHeaders . (walk (concatMap dropStrayHRule)) . (walk (concatMap dropEmptyList)) . (walk itemize))
