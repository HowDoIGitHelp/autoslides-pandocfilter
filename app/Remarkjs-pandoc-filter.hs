{-# LANGUAGE OverloadedStrings #-}

import Text.Pandoc.JSON
import Data.Text (Text)

wrapEquation :: Text -> Text
wrapEquation eq = "$$" <> eq <> "$$"

-- converts math blocks to code for mathjax integration
codifiedMath :: Block -> Block
codifiedMath (Para [Math DisplayMath eq]) = Para [Code ("", [], []) (wrapEquation eq)]
codifiedMath block = block

centeredHeader :: Block -> [Block]
centeredHeader header@(Header _ _ _) = [RawBlock (Format "markdown") "class: center, middle", header]
centeredHeader block = [block]

centeredHeaderSlides :: [Block] -> [Block]
centeredHeaderSlides (header@(Header _ _ _) : (RawBlock (Format "markdown") "---") : rest) =
    (centeredHeader header) ++ [(RawBlock (Format "markdown") "---")] ++ (centeredHeaderSlides rest)
centeredHeaderSlides (header@(Header _ _ _) : block : (RawBlock (Format "markdown") "---") : rest) =
    [header, block, (RawBlock (Format "markdown") "---")] ++ (centeredHeaderSlides rest)
centeredHeaderSlides (block : rest) = block : (centeredHeaderSlides rest)
centeredHeaderSlides [] = []

topDownBlockFilter :: (Block -> Block) -> Pandoc -> Pandoc
topDownBlockFilter blockfilter (Pandoc meta blocks) = Pandoc meta (map blockfilter blocks)

topDownBlockListFilter :: ([Block] -> [Block]) -> Pandoc -> Pandoc
topDownBlockListFilter blocklistfilter (Pandoc meta blocks) = Pandoc meta (blocklistfilter blocks)

main :: IO ()
main = toJSONFilter $
    topDownBlockListFilter centeredHeaderSlides
    . topDownBlockFilter codifiedMath
