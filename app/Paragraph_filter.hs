{-# LANGUAGE OverloadedStrings #-}

import Text.Pandoc.JSON
import Text.Pandoc.Walk (walk)
import Text.Pandoc.Definition (Inline(SoftBreak))
import Data.Text (Text)

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

listSplit :: (a -> Bool) -> [a] -> [[a]]
listSplit _ [] = [[]]
listSplit p l = 
    case (break p l) of
        (lf, [])   -> lf : []
        (lf, _:rs) -> lf : listSplit p rs


listSplitKeep :: (a -> Bool) -> [a] -> [[a]]
listSplitKeep _ [] = [[]]
listSplitKeep pred l = 
    case (break pred l) of
        (lf, [])   -> lf : []
        (lf, r:rs) -> lf : [r] : (listSplitKeep pred rs)

evenIndices :: [a] -> [a]
evenIndices (x : _ : xs) = x : evenIndices xs
evenIndices [x] = [x]
evenIndices [] = []

oddIndices :: [a] -> [a]
oddIndices (_ : x : xs) = x : oddIndices xs
oddIndices _ = []

combineHeaders :: [[Block]] -> [[Block]]
combineHeaders ([]:ls) = combineHeaders ls
combineHeaders ls = zipWith interleave (map head (evenIndices ls)) (oddIndices ls)

interleave :: Block -> [Block] -> [Block]
interleave (Header level (_, classes, kvattrs) inlines) l = concatMap (\x -> [betweener,x]) l
    where
        betweener = (Header level ("", classes, kvattrs) inlines)
interleave betweener l = concatMap (\x -> [betweener,x]) l

insertHeaders :: Pandoc -> Pandoc
insertHeaders (Pandoc meta blocks) = Pandoc meta (foldl (++) [] (combineHeaders (listSplitKeep (isHeader) blocks)))

sectionToSlides :: [Block] -> [Block]
sectionToSlides (header@(Header _ _ _) : nonHeader : rest) | not (isHeader nonHeader) = 
    [header, nonHeader, (RawBlock (Format "markdown") "---")] ++ sectionToSlides rest
sectionToSlides (block : rest) = block : sectionToSlides rest
sectionToSlides [] = []

dropEmpty :: [Block] -> [Block]
dropEmpty (Header _ _ _ : HorizontalRule : rest) = dropEmpty rest
dropEmpty (block : rest) = block : dropEmpty rest
dropEmpty [] = []

main :: IO ()
main = toJSONFilter ((walk sectionToSlides) . insertHeaders . (walk (concatMap dropStrayHRule)) . (walk (concatMap dropEmptyList)) . (walk itemize))
